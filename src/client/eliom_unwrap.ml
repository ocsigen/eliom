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

let unwrap_table : (Obj.t -> Obj.t) Js.js_array Js.t = jsnew Js.array_empty ()
(* table containing all the unwrapping functions referenced by their id *)

let register_unwrapper id f =
  Js.Optdef.case (Js.array_get unwrap_table id)
    (fun () -> Js.array_set unwrap_table id (fun x -> Obj.repr (f (Obj.obj x))))
    (fun _ -> failwith (Printf.sprintf "the unwrapper id %i is already registered" id))

let apply_unwrapper unwrapper v =
  Js.Optdef.case (Js.array_get unwrap_table unwrapper.id)
    (fun () -> failwith ("unregistered unwrapping id: " ^ (string_of_int unwrapper.id)))
    (fun f -> f v)

external raw_unmarshal_and_unwrap
  : (unwrapper -> 'a -> 'b) -> string -> int -> 'c
  = "caml_unwrap_value_from_string"

let unwrap s i =
  raw_unmarshal_and_unwrap
    apply_unwrapper s i

let unwrap_js_var s =
  unwrap (Js.to_bytestring (Js.Unsafe.variable s)) 0

