(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessexpl.ml
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

val iter_service_cookies :
   (Eliom_common.SessionCookies.key
    * Eliom_common.tables Eliom_common.Service_cookie.t
    -> unit Lwt.t)
  -> unit Lwt.t

val iter_data_cookies :
   (Eliom_common.SessionCookies.key * Eliom_common.Data_cookie.t -> unit Lwt.t)
  -> unit Lwt.t

val iter_persistent_cookies :
   (string * Eliommod_cookies.cookie -> unit Lwt.t)
  -> unit Lwt.t

val fold_service_cookies :
   (Eliom_common.SessionCookies.key
    * Eliom_common.tables Eliom_common.Service_cookie.t
    -> 'a
    -> 'a Lwt.t)
  -> 'a
  -> 'a Lwt.t

val fold_data_cookies :
   (Eliom_common.SessionCookies.key * Eliom_common.Data_cookie.t
    -> 'a
    -> 'a Lwt.t)
  -> 'a
  -> 'a Lwt.t

val fold_persistent_cookies :
   (string * Eliommod_cookies.cookie -> 'a -> 'a Lwt.t)
  -> 'a
  -> 'a Lwt.t

val number_of_service_cookies : unit -> int
val number_of_data_cookies : unit -> int
val number_of_tables : unit -> int
val number_of_table_elements : unit -> int list
val number_of_persistent_cookies : unit -> int Lwt.t
