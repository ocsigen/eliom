(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

open Lwt
open Ocsigen_extensions

(*****************************************************************************)
(* Making accessible this type from Eliom_common:                            *)
type server_params = Eliom_common.server_params

let sp_of_esp = Ocsigen_lib.id
let esp_of_sp = Ocsigen_lib.id

(*****************************************************************************)
let find_sitedata fun_name = function
  | Some sp -> sp.Eliom_common.sp_sitedata
  | None ->
      match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata -> get_current_sitedata ()
      | _ ->
          raise
            (Eliom_common.Eliom_function_forbidden_outside_site_loading
               fun_name)

(*****************************************************************************)
let get_user_agent ~sp =
  sp.Eliom_common.sp_request.request_info.ri_user_agent
let get_full_url ~sp =
  sp.Eliom_common.sp_request.request_info.ri_url_string
let get_remote_ip ~sp =
  sp.Eliom_common.sp_request.request_info.ri_remote_ip
let get_remote_inet_addr ~sp =
  sp.Eliom_common.sp_request.request_info.ri_remote_inet_addr
let get_get_params ~sp =
  Lazy.force sp.Eliom_common.sp_request.request_info.ri_get_params
let get_all_current_get_params ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_all_get_params
let get_initial_get_params ~sp =
  Lazy.force sp.Eliom_common.sp_request.request_info.ri_initial_get_params
let get_get_params_string ~sp =
  sp.Eliom_common.sp_request.request_info.ri_get_params_string
let get_post_params ~sp =
  sp.Eliom_common.sp_request.request_info.ri_post_params
    sp.Eliom_common.sp_request.request_config
let get_all_post_params ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_all_post_params
let get_original_full_path_string ~sp =
  sp.Eliom_common.sp_request.request_info.ri_original_full_path_string
let get_original_full_path ~sp =
  sp.Eliom_common.sp_request.request_info.ri_original_full_path
let get_current_full_path ~sp =
  sp.Eliom_common.sp_request.request_info.ri_full_path
let get_current_full_path_string ~sp =
  sp.Eliom_common.sp_request.request_info.ri_full_path_string
let get_current_sub_path ~sp =
  sp.Eliom_common.sp_request.request_info.ri_sub_path
let get_current_sub_path_string ~sp =
  sp.Eliom_common.sp_request.request_info.ri_sub_path_string
let get_header_hostname ~sp =
  sp.Eliom_common.sp_request.request_info.ri_host
let get_timeofday ~sp =
  sp.Eliom_common.sp_request.request_info.ri_timeofday
let get_request_id ~sp = Int64.bits_of_float (get_timeofday ~sp)
let get_hostname ~sp =
  Ocsigen_extensions.get_hostname sp.Eliom_common.sp_request
let get_server_port ~sp =
  Ocsigen_extensions.get_port sp.Eliom_common.sp_request
let get_ssl ~sp =
  sp.Eliom_common.sp_request.request_info.ri_ssl
let get_other_get_params ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_other_get_params
let get_nl_get_params ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_nl_get_params
let get_persistent_nl_get_params ~sp =
  Lazy.force sp.Eliom_common.sp_si.Eliom_common.si_persistent_nl_get_params
let get_nl_post_params ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_nl_post_params
let get_suffix ~sp =
  sp.Eliom_common.sp_suffix
let get_state_name ~sp =
  sp.Eliom_common.sp_fullsessname
let get_request_cache ~sp =
  sp.Eliom_common.sp_request.request_info.ri_request_cache
let clean_request_cache ~sp =
  sp.Eliom_common.sp_request.request_info.ri_request_cache <- 
    Polytables.create ()
let get_link_too_old ~sp =
  try
    Polytables.get
      ~table:sp.Eliom_common.sp_request.request_info.ri_request_cache
      ~key:Eliom_common.eliom_link_too_old
  with Not_found -> false
let get_expired_service_sessions ~sp =
  try
    Polytables.get
      ~table:sp.Eliom_common.sp_request.request_info.ri_request_cache
      ~key:Eliom_common.eliom_service_session_expired
  with Not_found -> ([], [])

let get_cookies ?(cookie_scope = `Session) ~sp () =
  match cookie_scope with
    | `Session ->
      Lazy.force sp.Eliom_common.sp_request.request_info.ri_cookies
    | `Client_process ->
      sp.Eliom_common.sp_si.Eliom_common.si_tab_cookies

let get_data_cookies ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_data_session_cookies
let get_persistent_cookies ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_persistent_session_cookies
let get_previous_extension_error_code ~sp =
  sp.Eliom_common.sp_si.Eliom_common.si_previous_extension_error
let get_si ~sp =
  sp.Eliom_common.sp_si

let get_user_cookies ~sp = sp.Eliom_common.sp_user_cookies
let get_user_tab_cookies ~sp = sp.Eliom_common.sp_user_tab_cookies



(****)
let get_sp_tab_cookie_info ~sp = sp.Eliom_common.sp_tab_cookie_info
let get_sp_appl_name ~sp = sp.Eliom_common.sp_appl_name
let get_sp_content_only ~sp = sp.Eliom_common.sp_content_only
let set_sp_appl_name ~sp v = sp.Eliom_common.sp_appl_name <- v
let set_sp_content_only ~sp v = sp.Eliom_common.sp_content_only <- v
let get_sp_client_process_info ~sp = sp.Eliom_common.sp_client_process_info

(* *)

let get_site_dir ~sp = sp.Eliom_common.sp_sitedata.Eliom_common.site_dir
let get_site_dir_string ~sp =
  sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
let get_request ~sp = sp.Eliom_common.sp_request
let get_ri ~sp = sp.Eliom_common.sp_request.Ocsigen_extensions.request_info

let get_tmp_filename fi = fi.Ocsigen_lib.tmp_filename
let get_filesize fi = fi.Ocsigen_lib.filesize
let get_original_filename fi = fi.Ocsigen_lib.original_basename

let get_sitedata ~sp = sp.Eliom_common.sp_sitedata


(***)

(*VVV ici ? pour des raisons de typage... *)
let set_site_handler sitedata handler =
  sitedata.Eliom_common.exn_handler <- handler
