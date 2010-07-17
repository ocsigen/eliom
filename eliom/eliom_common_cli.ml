(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
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


(******************************************************************)
type cookie_type = CBrowser | CTab

type fullsessionname = cookie_type * string

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
  | SAtt_csrf_safe of (int * string option * cookie_type option * bool option)
      (* CSRF safe anonymous coservice *)
      (* CSRF safe service registration delayed until form/link creation *)
      (* the int is an unique id,
         the string option is the session name for delayed registration
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
  | SNa_get_csrf_safe of (int * string option * cookie_type option * bool option)
      (* CSRF safe anonymous coservice *)
  | SNa_post_csrf_safe of (int * string option * cookie_type option * bool option)
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
let appl_name_cookie_name = "__eliom_appl_name"
let eliom_internal_nlp_prefix = "__eliom"
let nodisplay_class_name = "eliom_nodisplay"
let inline_class_name = "eliom_inline"

let nl_is_persistent n = n.[0] = 'p'

(*****************************************************************************)



type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list;

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

     si_tab_cookies: string Ocsigen_lib.String_Table.t;

     si_nonatt_info: na_key_req;
     si_state_info: (att_key_req * att_key_req);
     si_previous_extension_error: int;
     (* HTTP error code sent by previous extension (default: 404) *)

     si_na_get_params: (string * string) list Lazy.t;
     si_nl_get_params: (string * string) list Ocsigen_lib.String_Table.t;
     si_nl_post_params: (string * string) list Ocsigen_lib.String_Table.t;
     si_persistent_nl_get_params: (string * string) list Ocsigen_lib.String_Table.t Lazy.t;

     si_all_get_but_na_nl: (string * string) list Lazy.t;
     si_all_get_but_nl: (string * string) list;
   }
