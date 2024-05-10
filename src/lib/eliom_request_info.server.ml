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

open Ocsigen_extensions

(*****************************************************************************)
let find_sitedata fun_name =
  match Eliom_common.get_sp_option () with
  | Some sp -> sp.Eliom_common.sp_sitedata
  | None -> (
    match Eliom_common.global_register_allowed () with
    | Some get_current_sitedata -> get_current_sitedata ()
    | _ -> raise (Eliom_common.Eliom_site_information_not_available fun_name))

(*****************************************************************************)
let get_http_method () =
  let sp = Eliom_common.get_sp () in
  Ocsigen_request.meth sp.Eliom_common.sp_request.request_info

let get_user_agent () =
  let sp = Eliom_common.get_sp () in
  match
    Ocsigen_request.header sp.Eliom_common.sp_request.request_info
      Ocsigen_header.Name.user_agent
  with
  | Some ua -> ua
  | None -> ""

let get_full_url_sp sp =
  Uri.to_string (Ocsigen_request.uri sp.Eliom_common.sp_request.request_info)

let get_full_url () =
  let sp = Eliom_common.get_sp () in
  get_full_url_sp sp

let get_remote_ip () =
  let sp = Eliom_common.get_sp () in
  Ocsigen_request.remote_ip sp.Eliom_common.sp_request.request_info

let get_get_params () =
  let sp = Eliom_common.get_sp () in
  Ocsigen_request.get_params sp.Eliom_common.sp_request.request_info

let get_all_current_get_params_sp sp =
  sp.Eliom_common.sp_si.Eliom_common.si_all_get_params

let get_all_current_get_params () =
  let sp = Eliom_common.get_sp () in
  get_all_current_get_params_sp sp

let get_post_params_sp sp =
  Ocsigen_request.post_params sp.Eliom_common.sp_request.request_info
    sp.Eliom_common.sp_request.request_config.uploaddir
    sp.Eliom_common.sp_request.request_config.maxuploadfilesize

let get_post_params () =
  let sp = Eliom_common.get_sp () in
  get_post_params_sp sp

let get_files_sp sp =
  Ocsigen_request.files sp.Eliom_common.sp_request.request_info
    sp.Eliom_common.sp_request.request_config.uploaddir
    sp.Eliom_common.sp_request.request_config.maxuploadfilesize

let get_all_files () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_all_file_params

let get_all_post_params () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_all_post_params

let get_ignored_get_params () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_ignored_get_params

let get_ignored_post_params () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_ignored_post_params

let get_original_full_path_string_sp sp =
  Ocsigen_request.original_full_path_string
    sp.Eliom_common.sp_request.request_info

let get_original_full_path_string () =
  let sp = Eliom_common.get_sp () in
  get_original_full_path_string_sp sp

let get_original_full_path_sp sp =
  Ocsigen_request.original_full_path sp.Eliom_common.sp_request.request_info

let get_original_full_path () =
  let sp = Eliom_common.get_sp () in
  get_original_full_path_sp sp

let get_current_sub_path () =
  let sp = Eliom_common.get_sp () in
  Ocsigen_request.sub_path sp.Eliom_common.sp_request.request_info

let get_current_sub_path_string () =
  let sp = Eliom_common.get_sp () in
  Ocsigen_request.sub_path_string sp.Eliom_common.sp_request.request_info

let get_header_hostname () =
  let sp = Eliom_common.get_sp () in
  Ocsigen_request.host sp.Eliom_common.sp_request.request_info

let get_timeofday_sp sp =
  Ocsigen_request.timeofday sp.Eliom_common.sp_request.request_info

let get_timeofday () =
  let sp = Eliom_common.get_sp () in
  get_timeofday_sp sp

let get_hostname_sp sp =
  Ocsigen_extensions.get_hostname sp.Eliom_common.sp_request

let get_hostname () =
  let sp = Eliom_common.get_sp () in
  get_hostname_sp sp

let get_server_port_sp sp =
  Ocsigen_extensions.get_port sp.Eliom_common.sp_request

let get_server_port () =
  let sp = Eliom_common.get_sp () in
  get_server_port_sp sp

let get_ssl_sp sp = Ocsigen_request.ssl sp.Eliom_common.sp_request.request_info

let get_ssl () =
  let sp = Eliom_common.get_sp () in
  get_ssl_sp sp

let get_accept_language_sp sp =
  Ocsigen_header.Accept_language.parse
    (Ocsigen_request.header_multi sp.Eliom_common.sp_request.request_info
       Ocsigen_header.Name.accept_language)

let get_accept_language () =
  let sp = Eliom_common.get_sp () in
  get_accept_language_sp sp

let get_other_get_params () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_other_get_params

let get_nl_get_params () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_nl_get_params

let get_persistent_nl_get_params () =
  let sp = Eliom_common.get_sp () in
  Lazy.force sp.Eliom_common.sp_si.Eliom_common.si_persistent_nl_get_params

let get_nl_post_params () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_nl_post_params

let get_other_get_params_sp sp =
  sp.Eliom_common.sp_si.Eliom_common.si_other_get_params

let get_nl_get_params_sp sp =
  sp.Eliom_common.sp_si.Eliom_common.si_nl_get_params

let get_persistent_nl_get_params_sp sp =
  Lazy.force sp.Eliom_common.sp_si.Eliom_common.si_persistent_nl_get_params

let get_nl_post_params_sp sp =
  sp.Eliom_common.sp_si.Eliom_common.si_nl_post_params

let get_suffix_sp sp = sp.Eliom_common.sp_suffix

let get_suffix () =
  let sp = Eliom_common.get_sp () in
  get_suffix_sp sp

let get_state_name () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_full_state_name

let get_request_cache_sp sp =
  Ocsigen_request.request_cache sp.Eliom_common.sp_request.request_info

let get_request_cache () =
  let sp = Eliom_common.get_sp () in
  get_request_cache_sp sp

let get_link_too_old () =
  let sp = Eliom_common.get_sp () in
  try
    Polytables.get
      ~table:
        (Ocsigen_request.request_cache sp.Eliom_common.sp_request.request_info)
      ~key:Eliom_common.eliom_link_too_old
  with Not_found -> false

let get_expired_service_sessions () =
  let sp = Eliom_common.get_sp () in
  try
    Polytables.get
      ~table:
        (Ocsigen_request.request_cache sp.Eliom_common.sp_request.request_info)
      ~key:Eliom_common.eliom_service_session_expired
  with Not_found -> [], []

let get_cookies ?(cookie_level = `Session) () =
  let sp = Eliom_common.get_sp () in
  match cookie_level with
  | `Session -> Ocsigen_request.cookies sp.Eliom_common.sp_request.request_info
  | `Client_process -> sp.Eliom_common.sp_si.Eliom_common.si_tab_cookies

let get_data_cookies () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_data_session_cookies

let get_persistent_cookies () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_persistent_session_cookies

let get_previous_extension_error_code () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_si.Eliom_common.si_previous_extension_error

let get_si sp = sp.Eliom_common.sp_si

let get_user_cookies () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_user_cookies

let get_user_tab_cookies () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_user_tab_cookies

(****)

let get_sp_client_appl_name () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_client_appl_name

let get_sp_client_process_info_sp sp = sp.Eliom_common.sp_client_process_info

let get_sp_client_process_info () =
  let sp = Eliom_common.get_sp () in
  get_sp_client_process_info_sp sp

let expecting_process_page () =
  let sp = Eliom_common.get_sp () in
  Lazy.force sp.Eliom_common.sp_si.Eliom_common.si_expect_process_data

let get_csp_original_full_path () =
  let cpi = get_sp_client_process_info () in
  cpi.Eliom_common.cpi_original_full_path

let get_csp_hostname () =
  let cpi = get_sp_client_process_info () in
  cpi.Eliom_common.cpi_hostname

let get_csp_server_port () =
  let cpi = get_sp_client_process_info () in
  cpi.Eliom_common.cpi_server_port

let get_csp_ssl () =
  let cpi = get_sp_client_process_info () in
  cpi.Eliom_common.cpi_ssl

let get_csp_original_full_path_sp sp =
  let cpi = get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_original_full_path

let get_csp_hostname_sp sp =
  let cpi = get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_hostname

let get_csp_server_port_sp sp =
  let cpi = get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_server_port

let get_csp_ssl_sp sp =
  let cpi = get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_ssl

(* *)

let get_site_dir () =
  let sitedata = find_sitedata "Eliom_request_info.get_site_dir" in
  Eliom_common.get_site_dir sitedata

let get_site_dir_option () =
  try Some (get_site_dir ())
  with
  | Eliom_common.Cannot_call_this_function_before_app_is_linked_to_a_site
  | Eliom_common.Eliom_site_information_not_available _
  ->
    None

let get_site_dir_sp sp = Eliom_common.get_site_dir sp.Eliom_common.sp_sitedata
let in_request_handler () = Lwt.get Eliom_common.sp_key <> None

let get_request () =
  let sp = Eliom_common.get_sp () in
  sp.Eliom_common.sp_request

let get_request_sp sp = sp.Eliom_common.sp_request
let get_ri_sp sp = sp.Eliom_common.sp_request.Ocsigen_extensions.request_info

let get_ri () =
  let sp = Eliom_common.get_sp () in
  get_ri_sp sp

let get_tmp_filename fi = fi.Ocsigen_extensions.tmp_filename
let get_filesize fi = fi.Ocsigen_extensions.filesize
let get_original_filename fi = fi.Ocsigen_extensions.raw_original_filename
let get_file_content_type fi = fi.file_content_type
let get_sitedata () = find_sitedata "get_sitedata"
let get_sitedata_sp ~sp = sp.Eliom_common.sp_sitedata

(***)

(*VVV ici ? pour des raisons de typage... *)
let set_site_handler sitedata handler =
  sitedata.Eliom_common.exn_handler <- handler

type raw_post_data =
  ((string * string) * (string * string) list) option * Cohttp_lwt.Body.t

let raw_post_data sp =
  let ri = get_ri_sp sp in
  Lwt.return (Ocsigen_request.content_type ri, Ocsigen_request.body ri)
