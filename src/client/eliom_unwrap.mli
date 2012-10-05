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

type unwrap_id

val register_unwrapper : unwrap_id -> ('a -> 'b) -> unit
(** [register_unwrapper id f] register an unwrapping function [f] to
    be called when a value is marked with the id [id] *)

(** [unwrap_js_var v] execute [unwrap] on the content of the javascript
    variable [v] *)
val unwrap_js_var : string -> 'a

val id_of_int : int -> unwrap_id

(**/**)

(** [unwrap s i] unmarshal [s] (starting at character [i]) and
    transform the value [v] using registered wrappers. The marshalled
    value must have been produced with [Marshal.to_string
    (Eliom_wrap.wrap v)]. This function is for internal use only *)
val unwrap : string -> int -> 'a

(* == BBB Apropos late unwrapping

   When no unwrapper is registered for a value with a certain
   unwrapping marker, that marker is replaced by a late unwrapping
   marker in the JavaScript function caml_unwrap_value_from_string.

   All occurrences of such values inside other values (sharing) are
   recorded during unwrapping.

   There are two way for the replacing values marked for late
   unwrapping:
   
   Register an unwrapping [register_unwrapper]. All values which are
   marked for late unwrapping with the respective unwrap_id are
   applied to the unwrapping function, and all occurrences are
   replaced.

   Set a specific set of values marked for late unwrapping explicitly
   by [late_unwrap_value]. All values marked for late unwrapping with
   the given unwrap_id to which the predicade applies are replaced
   with the given value.

   Note, that when starting the actual client program, i.e. after
   running all top level declarations, no values marked for late
   unwrapping may remain (cf. [has_values_for_late_unwrapping]).
*)

(** [late_unwrap_value id predicate new_value] replaces each occurrence
    of every value which is marked for late unwrapping and on which
    applies [predicate], with [new_value].
*)
val late_unwrap_value : unwrap_id -> (_ -> bool) -> _ -> unit

(** The list of unwrap_id for which values marked for late unwrapping
    remain. *)
val remaining_values_for_late_unwrapping : unit -> int list
