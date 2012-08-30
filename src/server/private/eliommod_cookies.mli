(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_cookies.ml
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

open Eliom_lib
open Ocsigen_cookies

val cookieset_to_json : Ocsigen_cookies.cookieset -> string

val make_new_session_id : unit -> string

val get_cookie_info :
  float ->
  Eliom_common.sitedata ->
  Eliom_common.SessionCookies.key Eliom_common.Full_state_name_table.t ->
  Eliom_common.SessionCookies.key Eliom_common.Full_state_name_table.t ->
  string Eliom_common.Full_state_name_table.t ->
  (Eliom_common.SessionCookies.key Eliom_common.Full_state_name_table.t *
     Eliom_common.SessionCookies.key Eliom_common.Full_state_name_table.t *
     string Eliom_common.Full_state_name_table.t) option ->
  Eliom_common.tables Eliom_common.cookie_info *
  Eliom_common.Full_state_name_table.key list

val new_service_cookie_table :
  unit -> Eliom_common.tables Eliom_common.servicecookiestable
val new_data_cookie_table : unit -> Eliom_common.datacookiestable
val compute_session_cookies_to_send :
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookies.cookieset -> 
  Ocsigen_cookies.cookieset Lwt.t
val compute_cookies_to_send :
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookies.cookieset -> 
  Ocsigen_cookies.cookieset Lwt.t
val compute_new_ri_cookies :
  float ->
  string list ->
  string CookiesTable.t ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookies.cookieset -> string CookiesTable.t Lwt.t
