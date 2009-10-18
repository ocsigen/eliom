(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_common_obrowser.ml
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

(* Warning: the two following types are used both for service creation
   and for service identification. 
   Some cases may be useless in one case or another. *)

type att_key =
  | Att_no (* regular service *)
  | Att_named of string (* named coservice *)
  | Att_anon of string (* anonymous coservice *)
  | Att_csrf_safe (* CSRF safe anonymous coservice *)
      (* CSRF safe service registration delayed until form/link creation *)

type na_key =
  | Na_no (* no na information *)
  | Na_void_keep (* void coservice that keeps GET na parameters *)
  | Na_void_dontkeep (* void coservice that does not keep GET na parameters *)
  | Na_get_ of string (* named *)
  | Na_post_ of string (* named *)
  | Na_get' of string (* anonymous *)
  | Na_post' of string (* anonymous *)
  | Na_get_csrf_safe (* CSRF safe anonymous coservice *)
  | Na_post_csrf_safe (* CSRF safe anonymous coservice *)



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

let nl_is_persistent n = n.[0] = 'p'

(*****************************************************************************)


let eliom_link_too_old : bool Polytables.key = Polytables.make_key ()
(** The coservice does not exist any more *)

let eliom_service_session_expired : (string list) Polytables.key = 
  Polytables.make_key ()
(** If present in request data,  means that
    the service session cookies does not exist any more.
    The string lists are the list of names of expired sessions
*)



(*****************************************************************************)


type sess_info =
    {si_other_get_params: (string * string) list;
     si_all_get_params: (string * string) list;
     si_all_post_params: (string * string) list;

     si_service_session_cookies: string Ocsigen_lib.String_Table.t;
     (* the session service cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_data_session_cookies: string Ocsigen_lib.String_Table.t;
     (* the session data cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_persistent_session_cookies: string Ocsigen_lib.String_Table.t;
     (* the persistent session cookies sent by the request *)
     (* the key is the cookie name (or site dir) *)

     si_secure_cookie_info:
       (string Ocsigen_lib.String_Table.t *
          string Ocsigen_lib.String_Table.t *
          string Ocsigen_lib.String_Table.t) option;
     (* the same, but for secure cookies, if https *)

     si_nonatt_info: na_key;
     si_state_info: (att_key * att_key);
     si_previous_extension_error: int;
     (* HTTP error code sent by previous extension (default: 404) *)

     si_na_get_params: (string * string) list Lazy.t;
     si_nl_get_params: (string * string) list Ocsigen_lib.String_Table.t;
     si_nl_post_params: (string * string) list Ocsigen_lib.String_Table.t;
     si_persistent_nl_get_params: (string * string) list Ocsigen_lib.String_Table.t Lazy.t;

     si_all_get_but_na_nl: (string * string) list Lazy.t;
     si_all_get_but_nl: (string * string) list;
   }
