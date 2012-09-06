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

open Eliom_lib

open Ocsigen_cookies

(******************************************************************)

type scope_name =
  [ `String of string
  | `Default_ref_name
  | `Default_comet_name ]

type user_scope = [ `Session_group of scope_name
		  | `Session of scope_name
		  | `Client_process of scope_name ]

type scope = [ `Site
	     | user_scope ]

type all_scope = [ scope
                 | `Global
		 | `Request ]

type global_scope = [`Global]
type site_scope = [`Site]
type session_group_scope = [`Session_group of scope_name]
type session_scope = [`Session of scope_name]
type client_process_scope = [`Client_process of scope_name]
type request_scope = [`Request]

(******************************************************************)
type cookie_scope = [ `Session | `Client_process ]

let cookie_scope_of_user_scope : [< user_scope ] -> [> cookie_scope ] = function
  | `Session n
  | `Session_group n -> `Session
  | `Client_process n -> `Client_process

let scope_name_of_scope : [< user_scope ] -> [> scope_name ] = function
  | `Session n
  | `Session_group n
  | `Client_process n -> n

type fullsessionname = cookie_scope * string

module Fullsessionname_Table = Map.Make(struct
  type t = fullsessionname
  let compare = compare
end)

(******************************************************************)
(* Service kinds: *)
type att_key_serv =
  | SAtt_no (* regular service *)
  | SAtt_named of string (* named coservice *)
  | SAtt_anon of string (* anonymous coservice *)
  | SAtt_csrf_safe of (int * user_scope * bool option)
      (* CSRF safe anonymous coservice *)
      (* CSRF safe service registration delayed until form/link creation *)
      (* the int is an unique id,
         the user_scope is used for delayed registration
         (if the service is registered in the global table),
         the bool option is the ?secure parameter for delayed registration
         (if the service is registered in the global table) *)

type na_key_serv =
  | SNa_no (* no na information *)
  | SNa_void_keep (* void coservice that keeps GET na parameters *)
  | SNa_void_dontkeep (* void coservice that does not keep GET na parameters *)
  | SNa_get_ of string (* named *)
  | SNa_post_ of string (* named *)
  | SNa_get' of string (* anonymous *)
  | SNa_post' of string (* anonymous *)
  | SNa_get_csrf_safe of (int * user_scope * bool option)
      (* CSRF safe anonymous coservice *)
  | SNa_post_csrf_safe of (int * user_scope * bool option)
      (* CSRF safe anonymous coservice *)

(* the same, for incoming requests: *)
type att_key_req =
  | RAtt_no (* no coservice information *)
  | RAtt_named of string (* named coservice *)
  | RAtt_anon of string (* anonymous coservice *)

type na_key_req =
  | RNa_no (* no na information *)
  | RNa_get_ of string (* named *)
  | RNa_post_ of string (* named *)
  | RNa_get' of string (* anonymous *)
  | RNa_post' of string (* anonymous *)

let att_key_serv_of_req = function
  | RAtt_no -> SAtt_no
  | RAtt_named s -> SAtt_named s
  | RAtt_anon s -> SAtt_anon s

let na_key_serv_of_req = function
  | RNa_no -> SNa_no
  | RNa_post' s -> SNa_post' s
  | RNa_get' s -> SNa_get' s
  | RNa_post_ s -> SNa_post_ s
  | RNa_get_ s -> SNa_get_ s

(*****************************************************************************)
let defaultpagename = "./"
(* should be "" but this does not work with firefox.
   "index" works but one page may have two different URLs *)

let eliom_suffix_name = "__eliom_suffix"
let eliom_suffix_internal_name = "__(suffix service)__"
let eliom_nosuffix_page = "__eliom_suffix__"
let naservice_num = "__eliom_na__num"
let naservice_name = "__eliom_na__name"
let get_state_param_name = "__eliom__"
let post_state_param_name = "__eliom_p__"
let get_numstate_param_name = "__eliom_n__"
let post_numstate_param_name = "__eliom_np__"
let co_param_prefix = "__co_eliom_"
let na_co_param_prefix = "__na_eliom_"
let nl_param_prefix = "__nl_"
let pnl_param_prefix = nl_param_prefix^"p_"
let npnl_param_prefix = nl_param_prefix^"n_"
let nodisplay_class_name = "eliom_nodisplay"
let inline_class_name = "eliom_inline"

let eliom_internal_nlp_prefix = "__eliom"
let tab_cookies_param_name = "__eliom_P_tab_cookies"
let to_be_considered_as_get_param_name = "__eliom_P_was_GET"
let appl_name_cookie_name = "__eliom_appl_name"
let appl_name_header_name = "X-Eliom-Application"
let full_xhr_redir_header = "X-Eliom-Location-Full"
let half_xhr_redir_header = "X-Eliom-Location-Half"
let response_url_header = "X-Eliom-Location"

let default_group_name = "__eliom$%@default_group"
(*204FORMS* old implementation of forms with 204 and change_page_event
let internal_form_name = "_internal_form"
let internal_form_bool_name = "b"

let internal_form_full_name =
  npnl_param_prefix^
    eliom_internal_nlp_prefix^"-"^
    internal_form_name^"."^
    internal_form_bool_name
*)

let set_tab_cookies_header_name = "X-Eliom-Set-Process-Cookies"
let tab_cookies_header_name = "X-Eliom-Process-Cookies"
let tab_cpi_header_name = "X-Eliom-Process-Info"
let expecting_process_page_name = "X-Eliom-Expecting-Process-Page"

let base_elt_id = "eliom_base_elt"

let nl_is_persistent n = n.[0] = 'p'

(*****************************************************************************)

type client_process_info =  {
  cpi_ssl : bool;
  cpi_hostname : string;
  cpi_server_port : int;
  cpi_original_full_path : Url.path;
}

type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list option;

     si_service_session_cookies: string Fullsessionname_Table.t;
     (* the session service cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_data_session_cookies: string Fullsessionname_Table.t;
     (* the session data cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_persistent_session_cookies: string Fullsessionname_Table.t;
     (* the persistent session cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_secure_cookie_info:
       (string Fullsessionname_Table.t *
          string Fullsessionname_Table.t *
          string Fullsessionname_Table.t) option;
     (* the same, but for secure cookies, if https *)

     (* now for tab cookies: *)
     si_service_session_cookies_tab: string Fullsessionname_Table.t;
     si_data_session_cookies_tab: string Fullsessionname_Table.t;
     si_persistent_session_cookies_tab: string Fullsessionname_Table.t;
     si_secure_cookie_info_tab:
       (string Fullsessionname_Table.t *
          string Fullsessionname_Table.t *
          string Fullsessionname_Table.t) option;

     si_tab_cookies: string CookiesTable.t;

     si_nonatt_info: na_key_req;
     si_state_info: (att_key_req * att_key_req);
     si_previous_extension_error: int;
     (* HTTP error code sent by previous extension (default: 404) *)

     si_na_get_params: (string * string) list Lazy.t;
     si_nl_get_params: (string * string) list String.Table.t;
     si_nl_post_params: (string * string) list String.Table.t;
     si_persistent_nl_get_params: (string * string) list String.Table.t Lazy.t;

     si_all_get_but_na_nl: (string * string) list Lazy.t;
     si_all_get_but_nl: (string * string) list;

     si_client_process_info : client_process_info option;
     si_expect_process_data : bool Lazy.t;

(*204FORMS*     si_internal_form: bool; *)
   }

(************ unwrapping identifiers *********************)

let tyxml_unwrap_id_int = Eliom_lib_base.tyxml_unwrap_id_int
let () = assert (tyxml_unwrap_id_int = 1)
let comet_channel_unwrap_id_int = 2
let react_up_unwrap_id_int = 3
let react_down_unwrap_id_int = 4
let signal_down_unwrap_id_int = 5
let bus_unwrap_id_int = 6
let client_value_unwrap_id_int = Eliom_lib_base.client_value_unwrap_id_int
let () = assert (client_value_unwrap_id_int = 7)
let client_value_data_unwrap_id_int = Eliom_lib_base.Client_value_data_base.unwrap_id_int
let () = assert (client_value_data_unwrap_id_int = 8)
let injection_data_unwrap_id_int = Eliom_lib_base.Injection_data_base.unwrap_id_int
let () = assert (injection_data_unwrap_id_int = 9)
let server_function_unwrap_id_int = 10

type node_ref = string


(****** *)

(* CCC take care: this must remain of the same syntax as non localised
   non persistent get parameter name *)
let nl_get_appl_parameter = "__nl_n_eliom-process.p"
