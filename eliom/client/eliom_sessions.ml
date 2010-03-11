(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_sessions.ml
 * Copyright (C) 2009 Vincent Balat
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

(* Part of sp is reconstructed client side
   instead of serialising it to the client
*)



include Eliom_client_types

(*
(* si is reconstructed client side *)
let si = {
  Eliom_common.si_other_get_params  = [];
  Eliom_common.si_all_get_params = [];
  Eliom_common.si_all_post_params = [];
  
  Eliom_common.si_service_session_cookies = Ocsigen_lib.String_Table.empty;
  Eliom_common.si_data_session_cookies = Ocsigen_lib.String_Table.empty;
  Eliom_common.si_persistent_session_cookies = Ocsigen_lib.String_Table.empty;
  Eliom_common.si_secure_cookie_info = None;
  
  Eliom_common.si_nonatt_info = Eliom_common.RNa_no;
  Eliom_common.si_state_info = (Eliom_common.RAtt_no, Eliom_common.RAtt_no);
  Eliom_common.si_previous_extension_error = 404;
  
  Eliom_common.si_na_get_params = lazy [];
  Eliom_common.si_nl_get_params = Ocsigen_lib.String_Table.empty;
  Eliom_common.si_nl_post_params = Ocsigen_lib.String_Table.empty;
  Eliom_common.si_persistent_nl_get_params = 
    lazy Ocsigen_lib.String_Table.empty;
  
  Eliom_common.si_all_get_but_na_nl = lazy [];
  Eliom_common.si_all_get_but_nl = [];
}

let get_si ~sp = si
*)


(* let get_user_agent ~sp = sp.sp_request.request_info.ri_user_agent *)
(* let get_full_url ~sp = sp.sp_request.request_info.ri_url_string *)
(* let get_remote_ip ~sp = sp.sp_request.request_info.ri_remote_ip *)
(* let get_remote_inet_addr ~sp = sp.sp_request.request_info.ri_remote_inet_addr *)
(*let get_get_params ~sp = Lazy.force sp.sp_request.request_info.ri_get_params
let get_all_current_get_params ~sp = sp.sp_si.Eliom_common.si_all_get_params
let get_initial_get_params ~sp = Lazy.force sp.sp_request.request_info.ri_initial_get_params
let get_get_params_string ~sp = sp.sp_request.request_info.ri_get_params_string
let get_post_params ~sp = Lazy.force sp.sp_request.request_info.ri_post_params
let get_all_post_params ~sp = sp.sp_si.Eliom_common.si_all_post_params
*)
let get_original_full_path_string ~sp =
  "AAAAAAAAAAAAAAAAAAAAA" (*VVV !!!!!!!!! *)
let get_original_full_path ~sp = ["AAAAAAAAAAAAAAAAAAAAA"] (*VVV !!!!!!!!! *)
(*
let get_current_full_path ~sp = sp.sp_request.request_info.ri_full_path
let get_current_full_path_string ~sp = sp.sp_request.request_info.ri_full_path_string
let get_current_sub_path ~sp = sp.sp_request.request_info.ri_sub_path
let get_current_sub_path_string ~sp = sp.sp_request.request_info.ri_sub_path_string
let get_header_hostname ~sp = sp.sp_request.request_info.ri_host
let get_default_hostname ~sp = sp.sp_request.request_config.default_hostname
*)

let get_hostname ~sp = "toto" (*VVV !!!!!!!!! *)
let get_default_port ~sp = 80 (*VVV !!!!!!!!! *)
let get_default_sslport ~sp = 443 (*VVV !!!!!!!!! *)
let get_server_port ~sp = 8888888 (*VVV !!!!!!!!! *)

let get_ssl ~sp = false (*VVV !!!!!!!!! *)
let get_other_get_params ~sp = 
  sp.Eliom_client_types.sp_si.Eliom_common.si_other_get_params
let get_nl_get_params ~sp = 
  sp.Eliom_client_types.sp_si.Eliom_common.si_nl_get_params
let get_persistent_nl_get_params ~sp =
  Lazy.force sp.Eliom_client_types.sp_si.Eliom_common.si_persistent_nl_get_params
let get_nl_post_params ~sp =
  sp.Eliom_client_types.sp_si.Eliom_common.si_nl_post_params
let get_suffix ~sp =
  sp.Eliom_client_types.sp_suffix
let get_session_name ~sp =
  sp.Eliom_client_types.sp_fullsessname


(* let get_request_cache ~sp = sp.sp_request.request_info.ri_request_cache
let clean_request_cache ~sp = sp.sp_request.request_info.ri_request_cache <- 
    Polytables.create ()
let get_link_too_old ~sp =
  try
    Polytables.get
      ~table:sp.sp_request.request_info.ri_request_cache
      ~key:Eliom_common.eliom_link_too_old
  with Not_found -> false
let get_expired_service_sessions ~sp =
  try
    Polytables.get
      ~table:sp.sp_request.request_info.ri_request_cache
      ~key:Eliom_common.eliom_service_session_expired
  with Not_found -> [] *)
(* let get_config_default_charset ~sp =
  Ocsigen_charset_mime.default_charset
    sp.sp_request.request_config.charset_assoc *)
(*
let get_cookies ~sp = Lazy.force sp.sp_request.request_info.ri_cookies
let get_data_cookies ~sp = sp.sp_si.Eliom_common.si_data_session_cookies
let get_persistent_cookies ~sp = sp.sp_si.Eliom_common.si_persistent_session_cookies
let get_previous_extension_error_code ~sp = sp.sp_si.Eliom_common.si_previous_extension_error
*)
let get_si ~sp = sp.Eliom_client_types.sp_si


(* let get_request ~sp = sp.sp_request
let get_ri ~sp = sp.sp_request.request_info *)
(* let get_config_info ~sp = sp.sp_request.request_config *)

let get_site_dir ~sp = sp.sp_sitedata.site_dir
let get_site_dir_string ~sp =
  sp.sp_sitedata.site_dir_string
