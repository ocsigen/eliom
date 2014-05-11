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
exception Non_xml_content

val redirect_get : string -> unit
val redirect_post : string -> (string * Eliommod_parameters.param) list -> unit
val redirect_put : string -> (string * Eliommod_parameters.param) list -> unit
val redirect_delete : string -> (string * Eliommod_parameters.param) list -> unit

type 'a result

val xml_result : Dom.element Dom.document Js.t result
val string_result : string result

val send :
  ?expecting_process_page:bool ->
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * Eliommod_parameters.param) list ->
  ?form_arg:((string * Form.form_elt) list) -> string ->
  'a result ->
  (string * 'a option) Lwt.t

val send_get_form :
  ?expecting_process_page:bool ->
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * Eliommod_parameters.param) list ->
  Dom_html.formElement Js.t ->
  string ->
  'a result ->
  (string * 'a option) Lwt.t

val send_post_form :
  ?expecting_process_page:bool ->
  ?cookies_info:bool * string list ->
  ?get_args:(string * string) list ->
  ?post_args:(string * Eliommod_parameters.param) list ->
  Dom_html.formElement Js.t ->
  string ->
  'a result ->
  (string * 'a option) Lwt.t

val http_get :
  ?expecting_process_page:bool ->
  ?cookies_info:bool * string list ->
  string ->
  (string * string) list ->
  'a result ->
  (string * 'a option) Lwt.t

val http_post :
  ?expecting_process_page:bool ->
  ?cookies_info:bool * string list ->
  string ->
  (string * Eliommod_parameters.param) list ->
  'a result ->
  (string * 'a option) Lwt.t

val http_put :
  ?expecting_process_page:bool ->
  ?cookies_info:bool * string list ->
  string ->
  (string * Eliommod_parameters.param) list ->
  'a result ->
  (string * 'a option) Lwt.t

val http_delete :
  ?expecting_process_page:bool ->
  ?cookies_info:bool * string list ->
  string ->
  (string * Eliommod_parameters.param) list ->
  'a result ->
  (string * 'a option) Lwt.t

val get_cookie_info_for_uri_js : Js.js_string Js.t -> bool * string list

val max_redirection_level : int

(**/**)
val nl_template:
  (string, [ `WithoutSuffix ],
   [ `One of string ] Eliom_parameter.param_name) Eliom_parameter.non_localized_params
val nl_template_string: string
