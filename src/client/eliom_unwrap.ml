(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Pierre Chambart
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* TODO: implement with WeakMap when standardised:
   https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/WeakMap

class type ['a,'b] weakMap = object
  method get : 'a -> 'b Js.optdef Js.meth
  method set : 'a -> 'b -> unit Js.meth
  method has : 'a -> bool Js.t Js.meth
end

let weakMap : ('a,'b) weakMap Js.t Js.constr = Js.Unsafe.variable "window.WeakMap"

let map : (Obj.t,Obj.t) weakMap Js.t = jsnew weakMap ()

let get_obj_copy map o = Js.Optdef.to_option ( map##get(o) )
let set_obj_copy map o c = map##set(o,c)
*)

class type obj_with_copy = object
  method camlObjCopy : Obj.t Js.optdef Js.prop
end

let get_obj_copy o =
  let v : obj_with_copy Js.t = Obj.obj o in
  Js.Optdef.to_option ( v##camlObjCopy )

let set_obj_copy o c =
  let v : obj_with_copy Js.t = Obj.obj o in
  v##camlObjCopy <- Js.def c;

module Mark : sig
  type t
end
=
struct
  type t = string
end

(* XXX must be the same as in Ocsigen_wrap *)
type unwrap_id = int

let id_of_int x = x

type unwrapper =
    { id : unwrap_id;
      mutable umark : Mark.t; }

let unwrap_table : (int,Obj.t -> Obj.t) Hashtbl.t = Hashtbl.create 0
(* table containing all the unwrapping functions referenced by their id *)

let register_unwrapper id f =
  if Hashtbl.mem unwrap_table id
  then failwith (Printf.sprintf "the unwrapper id %i is already registered" id);
  Hashtbl.add unwrap_table id (fun x -> Obj.repr (f (Obj.obj x)))

let apply_unwrapper unwrapper v =
  let f =
    try
      Hashtbl.find unwrap_table unwrapper.id
    with
      | Not_found -> failwith ("unregistered unwrapping id: " ^ (string_of_int unwrapper.id))
  in
  f v

let is_marked (mark:Mark.t) o =

  let is_mark o =
    if (Obj.tag o = 0 && Obj.size o = 2 && Obj.field o 1 == (Obj.repr mark))
    then (let id = (Obj.field o 0) in
	  assert (Obj.tag id = Obj.int_tag);
	  true)
    else false
  in

  if (Obj.tag o = 0 && Obj.size o >= 2)
  (* WARNING: we only allow block values with tag = 0 to be wrapped.
     It is easier: we do not have to do another test to know if the
     value is a function *)
  then
    begin
      let potential_mark = (Obj.field o (Obj.size o - 1)) in
      if is_mark potential_mark
      then Some (Obj.obj potential_mark:unwrapper)
      else None
    end
  else None

type action =
  | Set_field of ( Obj.t * int )
  | Replace of Obj.t
  | Return

type stack =
  | Do of (Obj.t * action)
  | Unwrap of (unwrapper * Obj.t)

let find v =
  let tag = Obj.tag v in
  if tag >= Obj.no_scan_tag || tag = Obj.closure_tag
  || tag = Obj.infix_tag || tag = Obj.lazy_tag || tag = Obj.object_tag
  then Some v
  else get_obj_copy v

let search_and_replace_ mark v =
  let rec loop = function
    | [] -> assert false
    | (Unwrap (unwrapper,v))::q ->
      let new_v = apply_unwrapper unwrapper v in
      loop ((Do (new_v,Replace v))::q)
    | (Do (v,action))::q as s ->
      match find v with
	| Some r ->
	  (match action with
	    | Set_field (o,i) ->
	      Obj.set_field o i r;
	      loop q
	    | Replace o -> set_obj_copy o r;
	      loop q
	    | Return -> r)
	| None ->
	  match is_marked mark v with
	    | Some unwrapper ->
	      let stack = ref ((Unwrap (unwrapper,v))::s) in
	      let size = Obj.size v in
	      for i = 0 to size - 2 do
		stack := (Do ((Obj.field v i),Set_field (v,i))) :: !stack;
	      done;
	      loop !stack
	    | None ->
	      begin
		set_obj_copy v v;
	      (* It is ok to do this because tag < no_scan_tag and it is
		 not a closure ( either infix, normal or lazy ) *)
		let stack = ref s in
		let size = Obj.size v in
		for i = 0 to size - 1 do
		  stack := (Do ((Obj.field v i),Set_field (v,i))) :: !stack;
		done;
		loop !stack
	      end
  in
  loop [Do (v,Return)]

let unwrap (mark,v) = Obj.obj (search_and_replace_ (Obj.magic mark) (Obj.repr v))
