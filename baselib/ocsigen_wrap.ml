(* Ocsigen
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

module Mark : sig
  type t
  val wrap : t
  val touch : t
  val unwrap : t
  val do_nothing : t
end
=
struct
  type t = string
  let wrap = "wrap_mark"
  let touch = "touch_mark"
  let unwrap = "unwrap_mark"
  let do_nothing = "no wrap"
end

type mark = Mark.t

type 'a wrapper =
    { f : ( Obj.t -> Obj.t ) option;
      mutable mark : Mark.t; }

type 'a toucher = 'a wrapper

let mark_no_copy t mark =
  Obj_table.no_copy t mark;
  let wrappers =
    List.map (fun (_,v) -> (Obj.obj v:'a wrapper)) (Obj_table.find_parents t mark) in
  List.iter (fun wrapper ->
    Obj_table.no_copy t (Obj.repr wrapper);
    match wrapper.f with
      | None -> ()
      | Some f ->
	Obj_table.no_copy t (Obj.repr wrapper.f);
	Obj_table.no_copy t (Obj.repr f))
    wrappers

let make_table v =
  let t = Obj_table.make v in
  mark_no_copy t (Obj.repr Mark.wrap);
  mark_no_copy t (Obj.repr Mark.touch);
  Obj_table.no_copy t (Obj.repr Mark.unwrap);
  t

(* XXX : there can be sharing problems:
   let a = ref 0
   let f x = a
   let b = (a,(ref 1,{mark,f}))
   
   When we wrap the value b, we expect the new value to be let a = ref 0 in (a,a),
   but the sharing is lost: the value is in fact (ref 0,ref 0): a is copied before being wrapped.

   It could be corrected if we kept the reference to the old value: it
   would be possible to restore the sharing at the end. *)

let replace_one mark t =
  let t = Obj_table.copy t in
  match Obj_table.find_parents t mark with
    | [] -> assert false
    (* since [Obj_table.mem t Mark.wrap] is true this list can't be empty *)
    | (_,wrapper)::_ ->
      match Obj_table.find_parents t wrapper with
	| [] -> failwith "Can't wrap directly a wrapper, it must be part of a bigger value"
	(* a wrapper can't be the root: it must have ancessors.
	   XXX Should I forbid it outside a block value: It can be
	   dangerous if it is unexpectedly present in the closure
	   of a function. I could also add a specific function to
	   allow it to be in a function ex: create_for_closure*)
	| (_,father)::_ ->
	  let wrapper = (Obj.obj wrapper:'a wrapper) in
	  ( match wrapper.f with
	    | None -> wrapper.mark <- Mark.do_nothing
	    | Some f ->
	      let v = f father in
              Obj_table.replace t father v );
	  t

(** wrapper **)

let rec replace_wrap_marked t =
  if not (Obj_table.mem t (Obj.repr Mark.wrap))
  then Obj_table.root t
  else
    begin
      let t = replace_one (Obj.repr Mark.wrap) t in
      replace_wrap_marked (make_table (Obj_table.root t))
    end

let wrap v =
  let v = Obj.repr v in
  let t = make_table v in
  (Mark.unwrap,Obj.obj (replace_wrap_marked t))

let create_wrapper (f:'a -> 'b) : 'a wrapper =
  { f = Some (fun x -> Obj.repr (f (Obj.obj x)));
    mark = Mark.wrap; }

let empty_wrapper =
  { f = None;
    mark = Mark.do_nothing; }

let debug_wrap v =
  let v = Obj.repr v in
  let t = make_table v in
  let d1 = Obj_table.debug t in
  let v = replace_wrap_marked t in
  let t = make_table v in
  let d2 = Obj_table.debug t in
  d1,d2

(** toucher **)

let create_toucher (f:'a -> unit) : 'a toucher =
  { f = Some (fun x -> Obj.repr (f (Obj.obj x)));
    mark = Mark.touch; }

let touch v =
  let t = Obj_table.make (Obj.repr v) in
  if not (Obj_table.mem t (Obj.repr Mark.touch))
  then ()
  else
    begin
      let aux (_,toucher) =
	let toucher' = (Obj.obj toucher:'a toucher) in
	match toucher'.f with
	  | None -> toucher'.mark <- Mark.do_nothing
	  | Some f ->
	    List.iter (fun (_,x) -> ignore (f x); Gc.minor ();) (Obj_table.find_parents t toucher)
      in
      List.iter aux (Obj_table.find_parents t (Obj.repr Mark.touch))
    end

(** unwrap **)

type unwrap_id = int

type unwrapper =
    { id : unwrap_id;
      mutable umark : Mark.t; }

let id_of_int x = x

let create_unwrapper id : unwrapper =
  { id = id;
    umark = Mark.unwrap; }

let empty_unwrapper =
  { id = -1;
    umark = Mark.do_nothing; }

let unwrapper_mark = Mark.unwrap
