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

let weakMap : ('a,'b) weakMap Js.t Js.constr = Js.Unsafe.global##_WeakMap

let map : (Obj.t,Obj.t) weakMap Js.t = jsnew weakMap ()
*)

open Js_of_ocaml
open Eliom_lib

module Mark : sig
  type t
end = struct
  type t = string
end

(* XXX must be the same as in Ocsigen_wrap *)
type unwrap_id = int

let id_of_int x = x

type unwrapper = {id : unwrap_id; mutable umark : Mark.t} [@@warning "-69"]

let unwrap_table : (Obj.t -> Obj.t option) Js.js_array Js.t =
  new%js Js.array_empty
(* table containing all the unwrapping functions referenced by their id *)

type occurrence = {parent : Obj.t; field : int}

let register_unwrapper' id f =
  if Js.Optdef.test (Js.array_get unwrap_table id)
  then
    failwith (Printf.sprintf ">> the unwrapper id %i is already registered" id);
  let f x = Ocsigen_lib_base.Option.map Obj.repr (f (Obj.obj x)) in
  (* Store unwrapper *)
  Js.array_set unwrap_table id f

let register_unwrapper id f = register_unwrapper' id (fun x -> Some (f x))

let apply_unwrapper unwrapper v =
  Js.Optdef.case
    (Js.array_get unwrap_table unwrapper.id)
    (fun () -> None) (* Use late unwrapping! *)
    (fun f -> f v)

let late_unwrap_value old_value new_value =
  let old_value = Obj.repr old_value in
  (* Only process if the value is a block with the late-unwrap marker structure.
     The late-unwrap marker is stored in the last field and should be
     a block with at least 3 fields (tag, unwrapper_id, mark, occurrences).
     If the value doesn't have this structure, there are no occurrences
     to update, so we can skip. *)
  if Obj.is_block old_value then begin
    let size = Obj.size old_value in
    if size >= 1 then begin
      let last_field = Obj.field old_value (size - 1) in
      (* Check that last_field is defined (not undefined in JS) before proceeding *)
      if not (Js.Optdef.test (Obj.magic last_field)) then ()
      else if Obj.is_block last_field && Obj.size last_field >= 3 then
        (* This looks like a late-unwrap marker, process the occurrences *)
        List.iter
          (fun {parent; field} ->
             Obj.set_field parent (field - 1) (Obj.repr new_value))
          (Obj.obj (Obj.field last_field 2))
      (* else: no late-unwrap marker, nothing to update *)
    end
  end

external raw_unmarshal_and_unwrap :
   (unwrapper -> _ -> _ option)
  -> string
  -> int
  -> _
  = "caml_unwrap_value_from_string"

let unwrap s i =
  if !Eliom_config.debug_timings
  then Console.console##(time (Js.string "unwrap"));
  let res = raw_unmarshal_and_unwrap apply_unwrapper s i in
  if !Eliom_config.debug_timings
  then Console.console##(timeEnd (Js.string "unwrap"));
  res

let unwrap_js s = unwrap (Js.to_bytestring s) 0
