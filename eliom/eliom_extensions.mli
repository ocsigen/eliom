(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2008 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

val register_eliom_extension :
  (Eliom_common.server_params -> Ocsigen_extensions.answer Lwt.t) ->
  unit

(**/**)

val get_eliom_extension :
  unit ->
  Eliom_common.server_params ->
  Ocsigen_extensions.answer Lwt.t




val run_eliom_extension :
  'a ->
  Ocsigen_extensions.request_info * Eliom_common.sess_info * 'b *
  (((string option *
     Eliom_common.tables Eliom_common.one_service_cookie_info
     Eliom_common.session_cookie ref)
    Ocsigen_http_frame.Cookievalues.t ref *
    (string option *
     Eliom_common.one_data_cookie_info Eliom_common.session_cookie ref)
    Lazy.t Ocsigen_http_frame.Cookievalues.t ref *
    ((string * Eliom_common.timeout * float option *
      Eliommod_sessiongroups.perssessgrp option)
     option *
     Eliom_common.one_persistent_cookie_info Eliom_common.session_cookie ref)
    Lwt.t Lazy.t Ocsigen_http_frame.Cookievalues.t ref) *
   Eliom_common.tables Eliom_common.cookie_info1 option) ->
  Eliom_common.sitedata ->
  Ocsigen_extensions.answer Lwt.t
