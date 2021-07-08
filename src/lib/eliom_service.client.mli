(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
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

(** Creation and manipulation of Eliom services.

    See the Eliom manual for a detailed introduction to the concept of
    {% <<a_manual chapter="server-services"|Eliom services>>%} and to
    {% <<a_manual chapter="clientserver-services"|client-side service implementation>>%}.

    The main functions about services are documented in
    {% <<a_api | module Eliom_service_sigs.S >>%}. *)

include Eliom_service_sigs.S

val set_client_fun :
  ?app:string ->
  service:('a, 'b, _, _, _, _, _, _, _, _, _) t ->
  ('a -> 'b -> result Lwt.t) ->
  unit

(**/**)

val reset_reload_fun : (_, _, _, _, _, _, _, _, _, _, _) t -> unit

val pre_applied_parameters :
  (_, _, _, _, _, _, _, _, _, _, _) t ->
  (string * Eliommod_parameters.param) list Eliom_lib.String.Table.t *
    (string * Eliommod_parameters.param) list

val reload_fun :
  ('a, _, _, _, _, _, _, _, _, _, _) t ->
  ('a -> unit -> result Lwt.t) Eliom_client_value.t option

module Cohttp: sig module Header : sig type t end end
