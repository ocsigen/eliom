(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) CNRS Univ Paris Diderot
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

val get_global_data : unit -> Eliom_runtime.global_data
val get_request_data : unit -> Eliom_runtime.request_data

(*****************************************************************************)

val client_value :
   ?pos:Eliom_lib.pos
  -> string
  -> 'args
  -> 'a Eliom_client_value.t
(** Registers a client value datum for the next server section when
    executed in a global_data (cf. {!Eliom_syntax.set_global}) or in
    the request_data when executed in a request. *)

val set_global : bool -> unit
(** All client values created between [set_global true] and
    [set_global false] are considered global client values
    (cf. <<a_manual chapter="clientserver-language"
    chapter="clientvalues"|the manual>>).  *)

val global_context : unit -> bool
(** Returns whether client values created in the current context
    should be considered global *)

val close_server_section : string -> unit
(** Called at the end of each server or shared section. The argument
    identifies the compilation unit.

    Adds the list of recently registered
    {!Eliom_runtime.client_value_datum}s into the queue of server
    section data of the compilation unit
    ({!Eliom_lib_base.compilation_unit_global_data}).

    Called in parallel with <<a_api
    subproject="client"|Eliom_client.Syntax_helpers.close_server_section>>.  *)

val close_client_section :
   string
  -> (int * Ocsigen_lib.poly * Eliom_lib.pos * string option) list
  -> unit
(** Called at the end of every client or shared section. The first
    argument identifies the compilation unit. The second is the list
    of novel injections in that section.

    Adds a list of {!Eliom_lib_base.injection_datum}s into the queue
    of client section data of the compilation unit
    ({!Eliom_lib_base.compilation_unit_global_data}).

    Called in parallel with <<a_api
    subproject="client"|Eliom_client.Syntax_helpers.open_client_section>>.  *)

val escaped_value : 'a -> Eliom_runtime.escaped_value
(** Convert any value to a {! Eliom_runtime.escaped_value} for usage
    in the [args] argument to {! Eliom_syntax.client_value}. *)
