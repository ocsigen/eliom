(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

exception Looping_redirection
exception Failed_request of int
exception Program_terminated

val redirect_get : string -> unit
val redirect_post : string -> (string * string) list -> unit

val send :
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * string) list ->
  ?form_arg:Form.form_contents -> string ->
  string Lwt.t

val send_get_form :
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * string) list ->
  Dom_html.formElement Js.t ->
  string ->
  string Lwt.t

val send_post_form :
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * string) list ->
  Dom_html.formElement Js.t ->
  string ->
  string Lwt.t

val http_get :
  ?cookies_info:bool * string list ->
  string ->
  (string * string) list ->
  string Lwt.t

val http_post :
  ?cookies_info:bool * string list ->
  string ->
  (string * string) list ->
  string Lwt.t

val get_cookie_info_for_uri_js : Js.js_string Js.t -> bool * string list

val max_redirection_level : int
