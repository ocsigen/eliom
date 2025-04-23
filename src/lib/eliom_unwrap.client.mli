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

open Js_of_ocaml

type unwrap_id
(** Values of type [unwrap_id] are used to identify a specific unwrapper. *)

val id_of_int : int -> unwrap_id

val unwrap_js : Js.js_string Js.t -> 'a
(** [unwrap_js v] unwraps the content of the JavaScript variable [v] *)

(**/**)

val register_unwrapper : unwrap_id -> ('a -> 'b) -> unit
(** [register_unwrapper id f] register an unwrapping function [f] to
    be called when a value is marked with the id [id].

    This function will only work if called very early during
    client-side initialization, before the values sent by the server
    are unmarshalled. See issue #232. *)

val unwrap : string -> int -> 'a
(** [unwrap s i] unmarshal [s] (starting at character [i]) and
    transform the value [v] using registered wrappers. The marshalled
    value must have been produced with [Marshal.to_string
    (Eliom_wrap.wrap v)]. This function is for internal use only *)

(* == Internals

   [Eliom_unwrap.unwrap] implements basically the unmarshalling of a
   string to the JavaScript-representation an OCaml value, i.e.
   JavaScript Arrays in lieu of [Obj.t].

   It is implemented in the JavaScript function
   [caml_unwrap_value_from_string] in [eliom_client.js].

   However, the unmarshalling for [unwrap] is provided with the
   detection of unwrapping markers and application of the respective
   unwrapping functions:

   Consider [s] to be a string with the marshalled representation of a
   value [v]. Then [unwrap s 0] produces value similar to [v] but with
   all values [w] in [v] whose JavaScript representation is

     [0, ..., [0, id, "unwrap_mark"]]

   replaced by [f w] if [f] was registered as [register_unwrapper
   (id_of_int id) f]. Note, that the JavaScript's

     [0, id, "unwrap_mark"]

   corresponds to OCaml's

     (id, "unwrap_mark").

   == Apropos late unwrapping

   When no unwrapper is registered for a value with an unwrapping
   marker with unwrap ID [id], that marker is replaced by a late
   unwrapping marker

     (id, "late_unwrap_mark").

   Every occurrence of such a value in a field of another value
   (i.e. sharing) is recorded during unwrapping.

   These values can be later replaced using the function
   [late_unwrap_value] below. This is used for the consecutive
   unwrapping of client values.

   Note, that when starting the actual client program, i.e. after
   running all top level declarations, no values marked for late
   unwrapping should remain.
*)

val register_unwrapper' : unwrap_id -> ('a -> 'b option) -> unit

val late_unwrap_value : _ Eliom_runtime.Client_value_server_repr.t -> _ -> unit
(** [late_unwrap_value old_value new_value] replaces each occurrence
    of [old_value] with [new_value].
*)
