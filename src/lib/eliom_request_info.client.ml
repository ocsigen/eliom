open Eio.Std

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

open Js_of_ocaml
open Eliom_lib
include Eliom_types

let client_app_initialised = ref false

type t = {path : string list; si : Eliom_common.sess_info}

let default_ri = ref None
let ri_key = Fiber.create_key ()

let get_ri () =
  match Fiber.get ri_key with
  | Some p -> p
  | None -> (
    match !default_ri with
    | Some p -> p
    | None ->
        failwith "Eliom_request_info.get_sess_info called before initialization"
    )

let get_sess_info () = (get_ri ()).si

let set_session_info ~uri si f =
  let path = Url.path_of_url_string (if uri = "./" then "" else uri) in
  let ri = Some {path; si} in
  default_ri := ri;
  (Stdlib.Option.fold ~none:Fiber.without_binding
     ~some:(Fun.flip Fiber.with_binding)
     ri)
    ri_key f

let matches_regexp name re =
  try
    let _ = Re.exec re name in
    true
  with Not_found -> false

let matches_regexps regexps (name, _) =
  List.exists (matches_regexp name) regexps

let update_session_info ~path ~all_get_params ~all_post_params cont =
  let ignored_get, all_get_params =
    List.partition
      (matches_regexps !Eliom_process.ignored_get_params)
      all_get_params
  in
  let ignored_post, all_post_params =
    match all_post_params with
    | None -> [], None
    | Some p ->
        List.partition (matches_regexps !Eliom_process.ignored_post_params) p
        |> fun (a, b) -> a, Some b
  in
  let nl_get_params, all_get_but_nl =
    Eliom_common.split_nl_prefix_param all_get_params
  in
  let all_get_but_na_nl =
    lazy (Eliom_common.remove_na_prefix_params all_get_but_nl)
  and na_get_params = lazy (Eliom_common.filter_na_get_params all_get_but_nl) in
  let {si; _} = get_ri () in
  let si =
    { si with
      Eliom_common.si_other_get_params = []
    ; si_all_get_params = all_get_params
    ; si_na_get_params = na_get_params
    ; si_nl_get_params = nl_get_params
    ; si_nl_post_params = Eliom_lib.String.Table.empty
    ; si_all_post_params = all_post_params
    ; si_all_get_but_nl = all_get_but_nl
    ; si_all_get_but_na_nl = all_get_but_na_nl
    ; si_ignored_get_params = ignored_get
    ; si_ignored_post_params = ignored_post }
  in
  let ri = Some {path; si} in
  default_ri := ri;
  (Stdlib.Option.fold ~none:Fiber.without_binding
     ~some:(Fun.flip Fiber.with_binding)
     ri)
    ri_key cont

let remove_first_slash path = match path with "" :: l -> l | l -> l

let get_original_full_path_sp _sp =
  (* returns current path, not the one when application started *)
  if not (Eliom_process.history_api || !client_app_initialised)
  then
    match Url.Current.get () with
    | Some (Url.Http url) | Some (Url.Https url) -> url.Url.hu_path
    | Some (Url.File url) -> (
      match url.Url.fu_path with "" :: l -> l | l -> l)
    | None -> assert false
  else (get_ri ()).path

let get_original_full_path_string () =
  String.concat "/" (get_original_full_path_sp sp)

let get_original_full_path_string_sp = get_original_full_path_string
let get_nl_get_params () = (get_sess_info ()).Eliom_common.si_nl_get_params
let get_nl_get_params_sp = get_nl_get_params

let get_persistent_nl_get_params () =
  Lazy.force (get_sess_info ()).Eliom_common.si_persistent_nl_get_params

let get_persistent_nl_get_params_sp = get_persistent_nl_get_params
let get_si () = get_sess_info ()
let get_site_dir () = (Eliom_process.get_sitedata ()).site_dir
let get_site_dir_option () = Some (get_site_dir ())

let ssl_ =
  match Url.Current.get () with
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

let port_ =
  match Url.Current.port with Some p -> p | None -> if ssl_ then 443 else 80

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
  { Eliom_common.ejs_global_data = None
  ; ejs_request_data = [||]
  ; ejs_event_handler_table = Eliom_runtime.RawXML.ClosureMap.empty
  ; ejs_client_attrib_table = Eliom_runtime.RawXML.ClosureMap.empty
  ; ejs_sess_info =
      { Eliom_common.si_other_get_params = []
      ; si_all_get_params = []
      ; si_all_post_params = None
      ; si_all_file_params = None
      ; si_service_session_cookies = Eliom_common.Full_state_name_table.empty
      ; si_data_session_cookies = Eliom_common.Full_state_name_table.empty
      ; si_persistent_session_cookies = Eliom_common.Full_state_name_table.empty
      ; si_secure_cookie_info =
          ( Eliom_common.Full_state_name_table.empty
          , Eliom_common.Full_state_name_table.empty
          , Eliom_common.Full_state_name_table.empty )
      ; si_service_session_cookies_tab =
          Eliom_common.Full_state_name_table.empty
      ; si_data_session_cookies_tab = Eliom_common.Full_state_name_table.empty
      ; si_persistent_session_cookies_tab =
          Eliom_common.Full_state_name_table.empty
      ; si_secure_cookie_info_tab =
          ( Eliom_common.Full_state_name_table.empty
          , Eliom_common.Full_state_name_table.empty
          , Eliom_common.Full_state_name_table.empty )
      ; si_tab_cookies = Ocsigen_cookie_map.Map_inner.empty
      ; si_nonatt_info = Eliom_common.RNa_no
      ; si_state_info = Eliom_common.RAtt_no, Eliom_common.RAtt_no
      ; si_previous_extension_error = 404
      ; si_na_get_params = lazy []
      ; si_nl_get_params = Eliom_lib.String.Table.empty
      ; si_nl_post_params = Eliom_lib.String.Table.empty
      ; si_nl_file_params = Eliom_lib.String.Table.empty
      ; si_persistent_nl_get_params = lazy String.Table.empty
      ; si_all_get_but_na_nl = lazy []
      ; si_all_get_but_nl = []
      ; si_ignored_get_params = []
      ; si_ignored_post_params = []
      ; si_client_process_info = None
      ; si_expect_process_data = lazy false } }

let get_request_data () =
  let eliom_request_data = Js.Unsafe.global##.___eliom_request_data_ in
  Js.Optdef.case eliom_request_data
    (fun () -> default_request_data)
    (fun var -> Eliom_unwrap.unwrap_js var)

exception Eliom_no_raw_post_data_on_client

let raw_post_data _ = raise Eliom_no_raw_post_data_on_client

type raw_post_data = unit

let get_ignored_get_params () = (get_sess_info ()).si_ignored_get_params
let get_ignored_post_params () = (get_sess_info ()).si_ignored_post_params
