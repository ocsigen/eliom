(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessions.mli
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

(** This module contains the functions you need to get (or set)
    information about current request.
 *)

open Eliom_lib

(** {2 Getting information about the request} *)

val get_original_full_path_string : unit -> string
(** returns the full path of the URL as first sent by the browser
    (not changed by previous extensions like rewritemod) *)

val get_nl_get_params : unit -> (string * string) list String.Table.t
(** returns non localized parameters in the URL. *)

val get_site_dir : unit -> Url.path
(** returns the root of the site. *)

val get_ignored_get_params : unit -> (string * string) list
(** returns the GET parameters that have been ignored using
    <ignoredgetparams/> in config file. *)

val get_ignored_post_params : unit -> (string * string) list
(** returns the POST parameters that have been ignored using
    <ignoredpostparams/> in config file. *)

(*****************************************************************************)
(** {2 Other low level functions} *)

(** You probably don't need these functions. *)

(*****************************************************************************)
(** {3 Getting information about the URL of the client side process (csp)}

    Warning: it is different from the URL to which the request has been made.
*)

val get_csp_original_full_path : unit -> Url.path
(** returns the full path of the URL where the client-side process is running.
    If there is no client side process, same as
    {!get_original_full_path}.
*)

val get_csp_ssl : unit -> bool
(** returns true if https is used in the URL of the browser, false if http.
    If there is no client side process, same as {!get_ssl}.
*)

(**/**)

val get_request_template : unit -> string option
val ssl_ : bool

type raw_post_data = unit

val raw_post_data : unit -> _ Lwt.t
val client_app_initialised : bool ref
val get_request_data : unit -> Eliom_common.eliom_js_page_data
val get_request_cookies : unit -> Ocsigen_cookie_map.t
val get_si : Eliom_common.server_params -> Eliom_common.sess_info
val get_original_full_path_sp : Eliom_common.server_params -> Url.path
val get_original_full_path_string_sp : Eliom_common.server_params -> string
val get_csp_original_full_path_sp : Eliom_common.server_params -> Url.path
val get_csp_hostname_sp : Eliom_common.server_params -> string
val get_csp_server_port_sp : Eliom_common.server_params -> int
val get_csp_ssl_sp : Eliom_common.server_params -> bool

val get_nl_get_params_sp
  :  Eliom_common.server_params
  -> (string * string) list String.Table.t

val get_persistent_nl_get_params_sp
  :  Eliom_common.server_params
  -> (string * string) list String.Table.t

val get_sess_info : unit -> Eliom_common.sess_info

val set_session_info
  :  uri:string
  -> Eliom_common.sess_info
  -> (unit -> 'a Lwt.t)
  -> 'a Lwt.t

val update_session_info
  :  path:Url.path
  -> all_get_params:(string * string) list
  -> all_post_params:(string * string) list option
  -> (unit -> 'a Lwt.t)
  -> 'a Lwt.t
