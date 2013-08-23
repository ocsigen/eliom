(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_sessions.ml
 * Copyright (C) 2009 Vincent Balat
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

(* TODO: add missing functions to get
   (almost) the same interface as server side *)

(* - Part of sp is reconstructed client side
   - Another part is sent as application parameter (sitedata)
   - Antother part is sent with each request
*)

open Eliom_lib

let (>>>) x f = f x

include Eliom_types

let client_app_initialised = ref false

let get_sess_info = ref (fun () ->
  failwith "Eliom_request_info.get_sess_info called before initialization")

let set_session_info si = get_sess_info := fun () -> si

let remove_first_slash path =
  match path with
  | ""::l -> l
  | l -> l

let path_re =
  jsnew Js.regExp (Js.bytestring "^/?([^\\?]*)(\\?.*)?$")

let current_path_ = ref (remove_first_slash Url.Current.path)
let set_current_path path =
  let path =
    Js.Opt.case (path_re##exec (Js.string path))
      (fun () -> [])
      (fun handle ->
	let res = Js.match_result handle in
	let path =
	  Js.to_bytestring
            (Js.Optdef.get
               (Js.array_get res 1)
               (fun () -> Js.bytestring ""))
	in
	Url.split_path path)
  in
  current_path_ := path


let get_original_full_path_string () =
  (* returns current path, not the one when application started *)
  if !client_app_initialised
  then
    (* in that case it is a mobile app.
       It probably does not have multiple URLs.
       I return the path that has been set manually
       by Eliom_client.init_client_app.
       If we want to make possible to have several URL,
       we must replace the prefix of the URL (file:///.../)
       by the prefix set in init_client_app.

       (same everywhere below)
    *)
    String.concat "/"
      (Eliom_process.get_info ()).Eliom_common.cpi_original_full_path
  else
    if Eliom_process.history_api
    then String.concat "/" (match Url.Current.get () with
    | Some (Url.Http url) | Some (Url.Https url) -> url.Url.hu_path
    | Some (Url.File url) -> (match url.Url.fu_path with
      | ""::l -> l
      | l -> l)
    | _ -> assert false)
    else String.concat "/" !current_path_

let get_original_full_path_string_sp = get_original_full_path_string

let get_original_full_path_sp sp =
  if !client_app_initialised
  then (Eliom_process.get_info ()).Eliom_common.cpi_original_full_path
  else
    if Eliom_process.history_api
    then match Url.Current.get () with
    | Some (Url.Http url) | Some (Url.Https url) -> url.Url.hu_path
    | Some (Url.File url) -> (match url.Url.fu_path with
      | ""::l -> l
      | l -> l)
    | None -> assert false
    else !current_path_

let get_other_get_params () =
  (!get_sess_info ()).Eliom_common.si_other_get_params
let get_nl_get_params () = (!get_sess_info ()).Eliom_common.si_nl_get_params
let get_nl_get_params_sp = get_nl_get_params

let get_persistent_nl_get_params () =
  Lazy.force (!get_sess_info ()).Eliom_common.si_persistent_nl_get_params
let get_persistent_nl_get_params_sp = get_persistent_nl_get_params

let get_nl_post_params () =
  (!get_sess_info ()).Eliom_common.si_nl_post_params

let get_si () = !get_sess_info ()

let get_site_dir () = (Eliom_process.get_sitedata ()).site_dir
let get_site_dir_sp () = (Eliom_process.get_sitedata ()).site_dir
let get_site_dir_string () =
  (Eliom_process.get_sitedata ()).site_dir_string

let get_sp_appl_name = Eliom_process.get_application_name

let ssl_ = match Url.Current.get () with
  | Some (Url.Https _) -> true
  | Some (Url.Http _) | Some (Url.File _) | None -> false

let get_csp_ssl () =
  if !client_app_initialised
  then (Eliom_process.get_info ()).Eliom_common.cpi_ssl
  else ssl_
let get_csp_ssl_sp = get_csp_ssl

let host_ = Url.Current.host

let get_csp_hostname () =
  if !client_app_initialised
  then (Eliom_process.get_info ()).Eliom_common.cpi_hostname
  else host_
let get_csp_hostname_sp = get_csp_hostname

let port_ = match Url.Current.port with
  | Some p -> p
  | None -> if ssl_ then 443 else 80

let get_csp_server_port () =
  if !client_app_initialised
  then (Eliom_process.get_info ()).Eliom_common.cpi_server_port
  else port_
let get_csp_server_port_sp = get_csp_server_port

let get_csp_original_full_path () =
  if !client_app_initialised || Eliom_process.history_api
  then (Eliom_process.get_info ()).Eliom_common.cpi_original_full_path
  else remove_first_slash Url.Current.path

let get_csp_original_full_path_sp = get_csp_original_full_path

let get_request_cookies = Eliom_process.get_request_cookies
let get_request_template = Eliom_process.get_request_template

(* The request data used when it is not sent by server
   (i.e. when the client side process is initiated by client (mobile app...)) *)
let default_request_data =
  {Eliom_common.ejs_global_data = None;
   ejs_request_data = [];
   ejs_event_handler_table = Eliom_content_core.Xml.ClosureMap.empty;
   ejs_sess_info =
      {Eliom_common.si_other_get_params = [];
       si_all_get_params = [];
       si_all_post_params = None;

       si_service_session_cookies = Eliom_common.Full_state_name_table.empty;
       si_data_session_cookies = Eliom_common.Full_state_name_table.empty;
       si_persistent_session_cookies = Eliom_common.Full_state_name_table.empty;

       si_secure_cookie_info = None;

       si_service_session_cookies_tab = Eliom_common.Full_state_name_table.empty;
       si_data_session_cookies_tab = Eliom_common.Full_state_name_table.empty;
       si_persistent_session_cookies_tab = Eliom_common.Full_state_name_table.empty;
       si_secure_cookie_info_tab = None;

       si_tab_cookies = Ocsigen_cookies.CookiesTable.empty;

       si_nonatt_info = Eliom_common.RNa_no;
       si_state_info = (Eliom_common.RAtt_no,
                        Eliom_common.RAtt_no);
       si_previous_extension_error = 404;

       si_na_get_params = lazy [];
       si_nl_get_params = Eliom_lib.String.Table.empty;
       si_nl_post_params = Eliom_lib.String.Table.empty;
       si_persistent_nl_get_params = lazy String.Table.empty;

       si_all_get_but_na_nl = lazy [];
       si_all_get_but_nl = [];

       si_client_process_info = None;
       si_expect_process_data = lazy false;
      }

  }

let get_request_data, set_request_data, reset_request_data =
  let eliom_data = ref None in
  let get () =
    match !eliom_data with
      | Some data -> data
      | None ->
        let name = "__eliom_request_data" in
        Js.Optdef.case (Js.def (Js.Unsafe.variable ("this."^name)))
          (fun () -> eliom_data := Some default_request_data;
            default_request_data)
          (fun _ ->
            let data = Eliom_unwrap.unwrap_js_var name in
            eliom_data := Some data;
            data)
  in
  let reset () =
    eliom_data := None
  in
  let set (v : Eliom_common.eliom_js_page_data) =
    eliom_data := Some v
  in
  get, set, reset
