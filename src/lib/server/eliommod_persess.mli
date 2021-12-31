(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_persess.ml
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

open Eliom_common

val perstables : string list ref

(* TODO: to be removed? *)
module Persistent_cookies : sig
  type cookie = full_state_name * float option * timeout * perssessgrp option
  module Cookies : Ocsipersist.TABLE
    with type key = string and type value = cookie
  module Expiry_dates : Ocsipersist.TABLE
    with type key = float and type value = string
  val add : string -> cookie -> unit Lwt.t
  val replace_if_exists : string -> cookie -> unit Lwt.t
  val garbage_collect :
    section:Lwt_log_core.Section.t -> (Cookies.key -> unit Lwt.t) -> unit Lwt.t
end

val number_of_persistent_tables : unit -> int
val number_of_persistent_table_elements : unit -> (string * int) list Lwt.t
val close_persistent_state2 :
  scope:Eliom_common.user_scope ->
  Eliom_common.sitedata ->
  Eliom_common.perssessgrp option -> string -> unit Lwt.t
val close_persistent_state :
  scope:[< Eliom_common.user_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params -> unit -> unit Lwt.t
val find_or_create_persistent_cookie :
  ?set_session_group:string ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
val find_persistent_cookie_only :
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit -> Eliom_common.one_persistent_cookie_info Lwt.t
