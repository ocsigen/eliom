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

open Lwt
open Ocsigen_extensions

(*****************************************************************************)
(* Making accessible some types from Eliom_common: *)

type server_params = Eliom_common.server_params


let get_config () =
  match Eliom_common.global_register_allowed () with
  | Some _ -> !Eliommod.config
  | None ->
      raise
        (Eliom_common.Eliom_function_forbidden_outside_site_loading
           "get_config")

let find_sitedata fun_name = function
  | Some sp -> sp.Eliom_common.sp_sitedata
  | None ->
      match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata -> get_current_sitedata ()
      | _ ->
          raise
            (Eliom_common.Eliom_function_forbidden_outside_site_loading
               fun_name)


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
let get_default_hostname ?sp () =
  let sitedata = find_sitedata "get_default_hostname" sp in
  sitedata.Eliom_common.config_info.Ocsigen_extensions.default_hostname
let get_hostname ~sp =
  Ocsigen_extensions.get_hostname sp.Eliom_common.sp_request
let get_default_port ?sp () =
  let sitedata = find_sitedata "get_default_port" sp in
  sitedata.Eliom_common.config_info.Ocsigen_extensions.default_httpport
let get_default_sslport ?sp () =
  let sitedata = find_sitedata "get_default_sslport" sp in
  sitedata.Eliom_common.config_info.Ocsigen_extensions.default_httpsport
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
let get_config_default_charset ~sp =
  Ocsigen_charset_mime.default_charset
    sp.Eliom_common.sp_request.request_config.charset_assoc

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

let get_service_cookie
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  try
    let c = Eliommod_sersess.find_service_cookie_only
      ?state_name ~cookie_scope ~secure ~sp () in
    Some c.Eliom_common.sc_value
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_volatile_data_cookie
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  try
    let c = Eliommod_datasess.find_data_cookie_only ?state_name ~cookie_scope ~secure ~sp () in
    Some c.Eliom_common.dc_value
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_persistent_data_cookie
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
      return (Some c.Eliom_common.pc_value)
    )
    (function
       | Not_found | Eliom_common.Eliom_Session_expired -> return None
       | e -> fail e)


let get_sp_tab_cookie_info ~sp = sp.Eliom_common.sp_tab_cookie_info
let get_sp_appl_name ~sp = sp.Eliom_common.sp_appl_name
let get_sp_content_only ~sp = sp.Eliom_common.sp_content_only
let set_sp_appl_name ~sp v = sp.Eliom_common.sp_appl_name <- v
let set_sp_content_only ~sp v = sp.Eliom_common.sp_content_only <- v

(*
let get_default_service_session_timeout = Eliommod_timeouts.get_default_service_timeout
let set_default_service_session_timeout = Eliommod_timeouts.set_default_service_timeout

let get_default_volatile_data_session_timeout =
  Eliommod_timeouts.get_default_data_timeout

let set_default_volatile_data_session_timeout =
  Eliommod_timeouts.set_default_data_timeout

let set_default_volatile_session_timeout =
  Eliommod_timeouts.set_default_volatile_timeout

let get_default_persistent_data_session_timeout =
  Eliommod_timeouts.get_default_persistent_timeout

let set_default_persistent_data_session_timeout =
  Eliommod_timeouts.set_default_persistent_timeout
  *)

let set_global_service_state_timeout
    ?state_name ?(cookie_scope = `Session)
    ?sp ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = find_sitedata "set_global_service_timeout" sp in
  match state_name with
    | Some state_name ->
        Eliommod_timeouts.set_global_service_timeout
          ~state_name ~cookie_scope ~recompute_expdates
          override_configfile sitedata timeout
    | None ->
        Eliommod_timeouts.set_default_global_service_timeout
          cookie_scope override_configfile false sitedata timeout

let set_global_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session)
    ?sp ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = find_sitedata "set_global_data_timeout" sp in
  match state_name with
    | Some state_name ->
        Eliommod_timeouts.set_global_data_timeout
          ~state_name ~cookie_scope
          ~recompute_expdates override_configfile sitedata timeout
    | None ->
        Eliommod_timeouts.set_default_global_data_timeout
          cookie_scope override_configfile false sitedata timeout

let set_global_volatile_state_timeout
    ?state_name ?(cookie_scope = `Session)
    ?sp ?(recompute_expdates = false) 
    ?(override_configfile = false) timeout =
  let sitedata = find_sitedata "set_global_volatile_timeouts" sp in
  match state_name with
    | Some state_name ->
        Eliommod_timeouts.set_global_service_timeout
          ~state_name ~cookie_scope ~recompute_expdates
          override_configfile sitedata timeout;
        Eliommod_timeouts.set_global_data_timeout
          ~state_name ~cookie_scope ~recompute_expdates
          override_configfile sitedata timeout
    | None ->
        Eliommod_timeouts.set_default_global_service_timeout
          cookie_scope override_configfile false sitedata timeout;
        Eliommod_timeouts.set_default_global_data_timeout
          cookie_scope override_configfile false sitedata timeout

let set_global_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session)
    ?sp ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = find_sitedata "set_global_persistent_timeout" sp in
  match state_name with
    | Some state_name ->
        Eliommod_timeouts.set_global_persistent_timeout
          ~state_name ~cookie_scope ~recompute_expdates
          override_configfile sitedata timeout
    | None ->
        Eliommod_timeouts.set_default_global_service_timeout
          cookie_scope override_configfile false sitedata timeout


let get_global_service_state_timeout
    ?state_name ?(cookie_scope = `Session) ?sp () =
  let sitedata = find_sitedata "get_global_timeout" sp in
  Eliommod_timeouts.get_global_service_timeout ?state_name ~cookie_scope sitedata

let get_global_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?sp () =
  let sitedata = find_sitedata "get_global_timeout" sp in
  Eliommod_timeouts.get_global_data_timeout
    ?state_name ~cookie_scope sitedata

let get_global_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?sp () =
  let sitedata = find_sitedata "get_global_persistent_timeout" sp in
  Eliommod_timeouts.get_global_persistent_timeout ?state_name ~cookie_scope sitedata



(* Now for current session *)
let set_service_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp t =
  let c = 
    Eliommod_sersess.find_or_create_service_cookie ?state_name ~cookie_scope ~secure ~sp () 
  in
  let tor = c.Eliom_common.sc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t

let set_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp t =
  let c = 
    Eliommod_datasess.find_or_create_data_cookie
      ?state_name ~cookie_scope ~secure ~sp () 
  in
  let tor = c.Eliom_common.dc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t


let unset_service_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  try
    let c = 
      Eliommod_sersess.find_service_cookie_only ?state_name ~cookie_scope ~secure ~sp () 
    in
    let tor = c.Eliom_common.sc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()

let unset_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  try
    let c = 
      Eliommod_datasess.find_data_cookie_only ?state_name ~cookie_scope ~secure ~sp () 
    in
    let tor = c.Eliom_common.dc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()


let get_service_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  try
    let c = 
      Eliommod_sersess.find_service_cookie_only ?state_name ~cookie_scope ~secure ~sp () 
    in
    let tor = c.Eliom_common.sc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global_service_timeout 
          ?state_name ~cookie_scope sp.Eliom_common.sp_sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global_service_timeout
      ?state_name ~cookie_scope sp.Eliom_common.sp_sitedata

let get_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  try
    let c = 
      Eliommod_datasess.find_data_cookie_only ?state_name ~cookie_scope ~secure ~sp () 
    in
    let tor = c.Eliom_common.dc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global_data_timeout
          ?state_name ~cookie_scope sp.Eliom_common.sp_sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global_data_timeout
      ?state_name ~cookie_scope sp.Eliom_common.sp_sitedata







let set_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp t =
  Eliommod_persess.find_or_create_persistent_cookie
    ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
  let tor = c.Eliom_common.pc_timeout in
  return
      (match t with
      | None -> tor := Eliom_common.TNone
      | Some t -> tor := Eliom_common.TSome t)

let unset_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
      let tor = c.Eliom_common.pc_timeout in
      tor := Eliom_common.TGlobal;
      return ()
    )
    (function
       | Not_found | Eliom_common.Eliom_Session_expired -> return () 
       | e -> fail e)

let get_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
      let tor = c.Eliom_common.pc_timeout in
      return
        (match !tor with
        | Eliom_common.TGlobal ->
            Eliommod_timeouts.get_global_persistent_timeout
              ~state_name ~cookie_scope sp.Eliom_common.sp_sitedata
        | Eliom_common.TNone -> None
        | Eliom_common.TSome t -> Some t)
    )
    (function
      | Not_found | Eliom_common.Eliom_Session_expired ->
          return
            (Eliommod_timeouts.get_global_persistent_timeout
               ~state_name ~cookie_scope sp.Eliom_common.sp_sitedata)
      | e -> fail e)


(* Preventing memory leaks: we must close empty sessions *)

let rec close_service_state_if_empty
    ?state_name ?(scope = `Session) ?secure ~sp () =
  (* Close the session if it has not services inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sitedata = sp.Eliom_common.sp_sitedata in
    let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
    let c = Eliommod_sersess.find_service_cookie_only
      ?state_name ~cookie_scope ~secure ~sp ()
    in
    match scope with
      | `Session ->
(*VVV ???        (match !(c.Eliom_common.sc_session_group) with
          | (_, _, Ocsigen_lib.Right _) (* no group *)
              when *)
        if
          (Eliommod_sessiongroups.Data.group_size
             (sitedata.Eliom_common.site_dir_string, `Client_process,
              Ocsigen_lib.Left c.Eliom_common.sc_value)
           = 0) (* no tab sessions *)
          &&
            (Eliom_common.service_tables_are_empty !(c.Eliom_common.sc_table))
        then
          Eliommod_sessiongroups.Data.remove
            c.Eliom_common.sc_session_group_node
      | `Client_process ->
        if (Eliom_common.service_tables_are_empty !(c.Eliom_common.sc_table))
        then Eliommod_sessiongroups.Data.remove
          c.Eliom_common.sc_session_group_node
      | `Session_group -> (* There is a browser session, we do not close the group,
                     but we may close the browser session (this will close
                     the group if it is empty). *)
        close_service_state_if_empty
          ~sp ~scope:`Session ?state_name ?secure ()
  with Not_found -> ()



let rec close_volatile_state_if_empty
    ?state_name ?(scope = `Session) ?secure ~sp () =
  (* Close the session if it has not data inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sitedata = sp.Eliom_common.sp_sitedata in
    let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
    let c = Eliommod_datasess.find_data_cookie_only
      ?state_name ~cookie_scope ~secure ~sp ()
    in
    match scope with
      | `Session ->
        (match !(c.Eliom_common.dc_session_group) with
          | (_, _, Ocsigen_lib.Right _) (* no group *)
              when
                (Eliommod_sessiongroups.Data.group_size
                   (sitedata.Eliom_common.site_dir_string, `Client_process,
                    Ocsigen_lib.Left c.Eliom_common.dc_value)
                 = 0) (* no tab sessions *)
                &&
                  (sitedata.Eliom_common.not_bound_in_data_tables
                     c.Eliom_common.dc_value)
                ->
            Eliommod_sessiongroups.Data.remove
              c.Eliom_common.dc_session_group_node
          | _ -> ())
      | `Client_process -> ()
(* This should never occure, because we always have tab session data
   when we have a tab session (at least the change_page_event).
        if (sitedata.Eliom_common.not_bound_in_data_tables
              c.Eliom_common.dc_value)
        then Eliommod_sessiongroups.Data.remove
          c.Eliom_common.dc_session_group_node *)
      | `Session_group -> (* There is a browser session, we do not close the group,
                     but we may close the browser session (this will close
                     the group if it is empty). *)
        close_volatile_state_if_empty
          ~sp ~scope:`Session ?state_name ?secure ()
  with Not_found -> ()




let rec close_persistent_state_if_empty
    ?state_name ?(scope = `Session) ?secure ~sp () =
  Lwt.return ()
(*VVV Can we implement this function? *)


(* session groups *)

type 'a session_data =
  | No_data
  | Data_session_expired
  | Data of 'a


let set_service_session_group ?set_max
    ?state_name ?secure ~sp session_group =
  let cookie_scope = `Session in
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ~set_session_group:session_group
      ?state_name ~cookie_scope ~secure ~sp ()
  in
  match set_max with
    | None -> ()
    | Some m -> 
        Eliommod_sessiongroups.Data.set_max
          c.Eliom_common.sc_session_group_node m

let unset_service_session_group ?set_max
    ?state_name ?secure ~sp () =
  let cookie_scope = `Session in
  try
    let c = 
      Eliommod_sersess.find_service_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () 
    in
    let sitedata = sp.Eliom_common.sp_sitedata in
    let n =
      Eliommod_sessiongroups.make_full_group_name
        ~cookie_scope
        sp.Eliom_common.sp_request.Ocsigen_extensions.request_info
        sitedata.Eliom_common.site_dir_string
        (Eliom_common.get_mask4 sitedata)
        (Eliom_common.get_mask6 sitedata)
        None
    in
    let node = Eliommod_sessiongroups.Serv.move ?set_max
      sitedata
      c.Eliom_common.sc_session_group_node n
    in
    c.Eliom_common.sc_session_group_node <- node;
    c.Eliom_common.sc_session_group := n;

    (* Now we want to close the session if it has not data inside
       and no tab sessions *)
    close_service_state_if_empty ~scope:`Session 
      ?state_name ?secure ~sp ()

  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> ()

let get_service_session_group
    ?state_name ?secure ~sp () =
  let cookie_scope = `Session in
  try
    let c = 
      Eliommod_sersess.find_service_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () 
    in
    match !(c.Eliom_common.sc_session_group) with
      | _, _, Ocsigen_lib.Right _ -> No_data
      | _, _, Ocsigen_lib.Left v -> Data v
  with
    | Not_found -> No_data
    | Eliom_common.Eliom_Session_expired -> Data_session_expired



let set_volatile_data_session_group
    ?set_max ?state_name
    ?secure ~sp session_group =
  let cookie_scope = `Session in
  let c = 
    Eliommod_datasess.find_or_create_data_cookie
      ~set_session_group:session_group
      ?state_name ~cookie_scope ~secure ~sp () 
  in
  match set_max with
    | None -> ()
    | Some m -> 
        Eliommod_sessiongroups.Data.set_max
          c.Eliom_common.dc_session_group_node m

let unset_volatile_data_session_group ?set_max
    ?state_name ?secure ~sp () =
  let cookie_scope = `Session in
  try
    let c = 
      Eliommod_datasess.find_data_cookie_only
        ?state_name ~cookie_scope ~secure ~sp ()
    in
    let sitedata = sp.Eliom_common.sp_sitedata in
    let n =
      Eliommod_sessiongroups.make_full_group_name
        ~cookie_scope
        sp.Eliom_common.sp_request.Ocsigen_extensions.request_info
        sitedata.Eliom_common.site_dir_string 
        (Eliom_common.get_mask4 sitedata)
        (Eliom_common.get_mask6 sitedata)
        None
    in
    let node = Eliommod_sessiongroups.Data.move ?set_max
      sitedata c.Eliom_common.dc_session_group_node n
    in
    c.Eliom_common.dc_session_group_node <- node;
    c.Eliom_common.dc_session_group := n;

    (* Now we want to close the session if it has not data inside
       and no tab sessions *)
    close_volatile_state_if_empty ~scope:`Session 
      ?state_name ?secure ~sp ()

  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> ()

let get_volatile_data_session_group ?state_name ?secure ~sp () =
  let cookie_scope = `Session in
  try
    let c =
      Eliommod_datasess.find_data_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () 
    in
    match !(c.Eliom_common.dc_session_group) with
      | _, _, Ocsigen_lib.Right _ -> No_data
      | _, _, Ocsigen_lib.Left v -> Data v
  with
    | Not_found -> No_data
    | Eliom_common.Eliom_Session_expired -> Data_session_expired

let set_persistent_data_session_group ?set_max
    ?state_name ?secure ~sp n =
  let cookie_scope = `Session in
  Eliommod_persess.find_or_create_persistent_cookie
    ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
  let n =
    Eliommod_sessiongroups.make_persistent_full_group_name
      ~cookie_scope
      sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string (Some n)
  in
  let grp = c.Eliom_common.pc_session_group in
  Eliommod_sessiongroups.Pers.move
    sp.Eliom_common.sp_sitedata
    ?set_max
    (fst sp.Eliom_common.sp_sitedata.Eliom_common.max_persistent_data_sessions_per_group)
    c.Eliom_common.pc_value !grp n >>= fun l ->
  Lwt_util.iter
    (Eliommod_persess.close_persistent_session2
       ~cookie_scope sp.Eliom_common.sp_sitedata None) l >>= fun () ->
  grp := n;
  Lwt.return ()

let unset_persistent_data_session_group
    ?state_name ?secure ~sp () =
  let cookie_scope = `Session in
  Lwt.catch
    (fun () ->
       Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
       let grp = c.Eliom_common.pc_session_group in
       Eliommod_sessiongroups.Pers.remove
         sp.Eliom_common.sp_sitedata c.Eliom_common.pc_value !grp >>= fun () ->
       grp := None;

       close_persistent_state_if_empty ~scope:`Session 
         ?state_name ?secure ~sp ()

    )
    (function
       | Not_found
       | Eliom_common.Eliom_Session_expired -> Lwt.return ()
       | e -> fail e)


let get_persistent_data_session_group ?state_name ?secure ~sp () =
  let cookie_scope = `Session in
  catch
    (fun () ->
       Eliommod_persess.find_persistent_cookie_only
         ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
       Lwt.return (match !(c.Eliom_common.pc_session_group) with
                     | None -> No_data
                     | Some v ->
                       match Eliommod_sessiongroups.getperssessgrp v with
                         | (_, _, Ocsigen_lib.Left s) -> Data s
                         | _ -> No_data
       )
    )
    (function
       | Not_found -> Lwt.return No_data
       | Eliom_common.Eliom_Session_expired -> Lwt.return Data_session_expired
       | e -> fail e)





(* max *)
let set_default_max_service_sessions_per_group
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_service_sessions_per_group" sp 
  in
  let b = snd sitedata.Eliom_common.max_service_sessions_per_group in 
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_service_sessions_per_group <- (n, b)

let set_default_max_volatile_data_sessions_per_group
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_volatile_data_sessions_per_group" sp 
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_sessions_per_group in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_volatile_data_sessions_per_group <- (n, b)

let set_default_max_persistent_data_sessions_per_group
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_persistent_data_sessions_per_group" sp 
  in
  let b = snd sitedata.Eliom_common.max_persistent_data_sessions_per_group in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_persistent_data_sessions_per_group <- (n, b)

let set_default_max_service_sessions_per_subnet
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_service_sessions_per_subnet" sp 
  in
  let b = snd sitedata.Eliom_common.max_service_sessions_per_subnet in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_service_sessions_per_subnet <- (n, b)

let set_default_max_volatile_data_sessions_per_subnet
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_volatile_data_sessions_per_subnet" sp 
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_sessions_per_subnet in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_volatile_data_sessions_per_subnet <- (n, b)

let set_default_max_volatile_sessions_per_group ?sp ?override_configfile n =
  set_default_max_service_sessions_per_group ?sp ?override_configfile n;
  set_default_max_volatile_data_sessions_per_group ?sp ?override_configfile n

let set_default_max_volatile_sessions_per_subnet ?sp ?override_configfile n =
  set_default_max_service_sessions_per_subnet ?sp ?override_configfile n;
  set_default_max_volatile_data_sessions_per_subnet ?sp ?override_configfile n



let set_default_max_service_tab_sessions_per_group
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_service_tab_sessions_per_group" sp 
  in
  let b = snd sitedata.Eliom_common.max_service_tab_sessions_per_group in 
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_service_tab_sessions_per_group <- (n, b)

let set_default_max_volatile_data_tab_sessions_per_group
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_volatile_data_tab_sessions_per_group" sp 
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group <- (n, b)

let set_default_max_persistent_data_tab_sessions_per_group
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_persistent_data_tab_sessions_per_group" sp 
  in
  let b = snd sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group <- (n, b)

let set_default_max_volatile_tab_sessions_per_group ?sp ?override_configfile n =
  set_default_max_service_tab_sessions_per_group ?sp ?override_configfile n;
  set_default_max_volatile_data_tab_sessions_per_group ?sp ?override_configfile n


let set_default_max_anonymous_services_per_session
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_anonymous_services_per_session" sp 
  in
  let b = snd sitedata.Eliom_common.max_anonymous_services_per_session in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_anonymous_services_per_session <- (n, b)

let set_default_max_anonymous_services_per_subnet
    ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_default_max_anonymous_services_per_subnet" sp 
  in
  let b = snd sitedata.Eliom_common.max_anonymous_services_per_subnet in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.max_anonymous_services_per_subnet <- (n, b)


let set_ipv4_subnet_mask ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_ipv4_subnet_mask" sp 
  in
  let b = snd sitedata.Eliom_common.ipv4mask in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.ipv4mask <- (Some n, b)

let set_ipv6_subnet_mask ?sp ?(override_configfile = false) n =
  let sitedata = 
    find_sitedata "set_ipv6_subnet_mask" sp 
  in
  let b = snd sitedata.Eliom_common.ipv6mask in
  if override_configfile || not b
  then 
    sitedata.Eliom_common.ipv6mask <- (Some n, b)




let set_max_service_sessions_for_group_or_subnet
    ?state_name ?(scope = `Session) ?secure ~sp m =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ?state_name ~cookie_scope ~secure ~sp ()
  in
  match scope with
    | `Session_group ->
      (match 
          Eliommod_sessiongroups.Data.find_node_in_group_of_groups
            !(c.Eliom_common.sc_session_group)
       with
         | Some node -> Eliommod_sessiongroups.Data.set_max node m
         | _ -> ())
    | _ ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.sc_session_group_node m

let set_max_volatile_data_sessions_for_group_or_subnet
    ?state_name ?(scope = `Session) ?secure ~sp m =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let c =
    Eliommod_datasess.find_or_create_data_cookie
      ?state_name ~cookie_scope ~secure ~sp ()
  in
  match scope with
    | `Session_group ->
      (match 
          Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
            !(c.Eliom_common.dc_session_group)
       with
         | Some (_, node) -> Eliommod_sessiongroups.Data.set_max node m
         | _ -> ())
    | _ ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.dc_session_group_node m

let set_max_volatile_sessions_for_group_or_subnet
    ?state_name ?scope ?secure ~sp m =
  set_max_service_sessions_for_group_or_subnet
    ?scope ?state_name ?secure ~sp m;
  set_max_volatile_data_sessions_for_group_or_subnet
    ?scope ?state_name ?secure ~sp m

(*VVV No version for persistent sessions? Why? *)






(* expiration dates *)
let set_service_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure ~sp t =
  let c = 
    Eliommod_sersess.find_or_create_service_cookie ?state_name ~cookie_scope ~secure ~sp () 
  in
  let exp = c.Eliom_common.sc_cookie_exp in
  match t with
  | None -> exp := Eliom_common.CEBrowser
  | Some t -> exp := Eliom_common.CESome t

(*
let get_service_cookie_exp_date ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  try
    let (_, _, _, _, exp) = find_service_cookie_only ?state_name ~cookie_scope ~secure ~sp () in
  let exp = c.Eliom_common.sc_cookie_exp in
    !exp
  with Not_found | Eliom_common.Eliom_Session_expired -> Eliom_common.CEBrowser
*)

let set_volatile_data_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure ~sp t =
  let c = 
    Eliommod_datasess.find_or_create_data_cookie
      ?state_name ~cookie_scope ~secure ~sp () 
  in
  let exp = c.Eliom_common.dc_cookie_exp in
  match t with
  | None -> exp := Eliom_common.CEBrowser
  | Some t -> exp := Eliom_common.CESome t



let set_persistent_data_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure ~sp t =
  Eliommod_persess.find_or_create_persistent_cookie ?state_name ~cookie_scope ~secure ~sp ()
  >>= fun c ->
  let exp = c.Eliom_common.pc_cookie_exp in
  return
      (match t with
      | None -> exp := Eliom_common.CEBrowser
      | Some t -> exp := Eliom_common.CESome t)

let get_persistent_data_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure ~sp () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
      return !(c.Eliom_common.pc_cookie_exp))
    (function
       | Not_found | Eliom_common.Eliom_Session_expired -> 
           return Eliom_common.CEBrowser
       | e -> fail e)





(* *)

let get_site_dir ~sp = sp.Eliom_common.sp_sitedata.Eliom_common.site_dir
let get_site_dir_string ~sp =
  sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
let get_request ~sp = sp.Eliom_common.sp_request
let get_ri ~sp = sp.Eliom_common.sp_request.Ocsigen_extensions.request_info
let get_config_info ~sp = sp.Eliom_common.sp_request.Ocsigen_extensions.request_config

let get_tmp_filename fi = fi.Ocsigen_lib.tmp_filename
let get_filesize fi = fi.Ocsigen_lib.filesize
let get_original_filename fi = fi.Ocsigen_lib.original_basename

let get_global_table ?sp () =
  let sitedata = find_sitedata "get_global_table" sp in
  sitedata.Eliom_common.global_services

let get_sitedata ~sp = sp.Eliom_common.sp_sitedata

(** If the session does not exist, we create it
   (new cookie, new session service table) *)
let get_session_service_table 
    ?state_name ?(scope = `Session) ?secure ~sp () =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
  let c = 
    Eliommod_sersess.find_or_create_service_cookie
      ?state_name ~cookie_scope ~secure ~sp () 
  in
  if scope = `Session_group
  then match
      Eliommod_sessiongroups.Serv.find_node_in_group_of_groups 
        !(c.Eliom_common.sc_session_group)
    with None -> raise Not_found
      | Some (t, _) -> t
  else c.Eliom_common.sc_table

(** If the session does not exist, we raise Not_found *)
let get_session_service_table_if_exists
    ?state_name ?(scope = `Session) ?secure ~sp () =
  try
    let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
    let c = 
      Eliommod_sersess.find_service_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () 
    in
    if scope = `Session_group
    then match
        Eliommod_sessiongroups.Serv.find_node_in_group_of_groups 
          !(c.Eliom_common.sc_session_group)
      with None -> raise Not_found
        | Some (t, _) -> t
    else c.Eliom_common.sc_table
  with Eliom_common.Eliom_Session_expired -> raise Not_found


let set_site_handler sitedata handler =
  sitedata.Eliom_common.exn_handler <- handler




(*****************************************************************************)
(** {2 persistent sessions} *)

open Ocsipersist

type 'a persistent_table =
    (Eliom_common.user_scope * 
       string option *
       bool *
       (int64 * 'a) Ocsipersist.table)

let create_persistent_table
    ?state_name ?(scope = `Session) ?(secure = false) name =
  let t = Eliom_common.create_persistent_table name in
  (scope, state_name, secure, t)


let get_table_key_
    ~table:(scope, (state_name : string option), secure, table) ~sp 
    (find_cookie : ?state_name:string ->
     ?cookie_scope:Eliom_common.cookie_scope ->
     secure:bool option ->
     sp:Eliom_common.server_params ->
     unit -> Eliom_common.one_persistent_cookie_info Lwt.t) =
  (match scope with
    | `Session_group ->
      (get_persistent_data_session_group ?state_name ~secure ~sp ()
       >>= function
         | Data a -> Lwt.return a
         | _ -> Lwt.return Eliom_common.default_group_name)
    | _ ->
      let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
      find_cookie ?state_name ~cookie_scope ~secure:(Some secure) ~sp () 
      >>= fun c -> Lwt.return c.Eliom_common.pc_value)
  >>= fun key ->
  Lwt.return (table, key)

let get_persistent_data ~table ~sp () =
  catch
    (fun () ->
      get_table_key_ ~table ~sp Eliommod_persess.find_persistent_cookie_only
      >>= fun (table, key) ->
      Ocsipersist.find table key >>= fun (_, v) ->
      Lwt.return (Data v)
    )
    (function
      | Eliom_common.Eliom_Session_expired -> return Data_session_expired
      | Not_found -> return No_data
      | e -> fail e)

let f__ ?state_name ?cookie_scope ~secure ~sp () =
  Eliommod_persess.find_or_create_persistent_cookie
    ?state_name ?cookie_scope ~secure ~sp ()

let set_persistent_data ~table ~sp value =
  get_table_key_ ~table ~sp f__ >>= fun (table, key) ->
  Ocsipersist.add table key (Int64.zero, value)

let remove_persistent_data ~table ~sp () =
  catch
    (fun () ->
      let (scope, state_name, secure, _) = table in
      get_table_key_ ~table ~sp Eliommod_persess.find_persistent_cookie_only
      >>= fun (table, key) ->
      Ocsipersist.remove table key >>= fun () ->

      close_persistent_state_if_empty
        ~sp ~scope ?state_name ~secure ()
    )
    (function
      | Not_found | Eliom_common.Eliom_Session_expired -> return ()
      | e -> fail e)


(*****************************************************************************)
(** {2 session data in memory} *)

type 'a volatile_table =
    (Eliom_common.user_scope * 
       string option *
       bool *
       'a Eliom_common.SessionCookies.t)

let create_volatile_table
    ?state_name ?(scope = `Session) ?(secure = false) ?sp () =
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata -> 
        Eliommod_datasess.create_volatile_table ~scope ~state_name ~secure
      | None -> raise
            (Eliom_common.Eliom_function_forbidden_outside_site_loading
               "create_volatile_table"))
  | Some sp ->
    Eliommod_datasess.create_volatile_table_during_session
      ~scope ~state_name ~secure sp

let get_table_key_ ~table:(scope, (state_name : string option), secure, table) ~sp 
    (find_cookie : ?state_name:string ->
     ?cookie_scope:Eliom_common.cookie_scope ->
     secure:bool option ->
     sp:Eliom_common.server_params ->
     unit -> Eliom_common.one_data_cookie_info) =
  let key = match scope with
    | `Session_group ->
      (match get_volatile_data_session_group ?state_name ~secure ~sp () 
       with Data a -> a
         | _ -> Eliom_common.default_group_name)
    | _ -> 
      let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
      let c =
        find_cookie ?state_name ~cookie_scope ~secure:(Some secure) ~sp () in
      c.Eliom_common.dc_value
  in
  (table, key)


let get_volatile_data ~table ~sp () =
  try
    let (table, key) =
      get_table_key_ ~table ~sp Eliommod_datasess.find_data_cookie_only
    in
    Data (Eliom_common.SessionCookies.find table key)
  with
  | Not_found -> No_data
  | Eliom_common.Eliom_Session_expired -> Data_session_expired

let f__ ?state_name ?cookie_scope ~secure ~sp () =
  Eliommod_datasess.find_or_create_data_cookie
    ?state_name ?cookie_scope ~secure ~sp ()

let set_volatile_data ~table ~sp value =
  let (table, key) =
    get_table_key_ ~table ~sp f__
  in
  Eliom_common.SessionCookies.replace table key value


let remove_volatile_data ~table ~sp () =
  try
    let (scope, state_name, secure, _) = table in
    let (table, key) =
      get_table_key_ ~table ~sp Eliommod_datasess.find_data_cookie_only
    in
    Eliom_common.SessionCookies.remove table key;

    (* Now we want to close the session if it has not data inside
       and no group and no sub sessions *)
    close_volatile_state_if_empty ~sp ~scope ?state_name ~secure ()

  with Not_found | Eliom_common.Eliom_Session_expired -> ()





(*****************************************************************************)
(** {2 Eliom references} *)

module Eref = struct
  type 'a eref =
      'a * ('a volatile_table, 'a persistent_table) Ocsigen_lib.leftright

  let eref
      ?state_name ?scope ?secure ?persistent ?sp value =
    match persistent with
      | None ->
        (value,
         Ocsigen_lib.Left 
           (create_volatile_table ?state_name ?scope ?secure ?sp ()))
      | Some name ->
        (value,
         Ocsigen_lib.Right 
           (create_persistent_table ?state_name ?scope ?secure name))

  let get ~sp (value, table) =
    match table with
      | Ocsigen_lib.Left t ->
        (match get_volatile_data ~table:t ~sp () with
          | Data d -> Lwt.return d
          | _ -> Lwt.return value)
      | Ocsigen_lib.Right t ->
        (get_persistent_data ~table:t ~sp () >>= function
          | Data d -> Lwt.return d
          | _ -> Lwt.return value)

  let set ~sp (_, table) value =
    match table with
      | Ocsigen_lib.Left t -> set_volatile_data ~table:t ~sp value;
        Lwt.return ()
      | Ocsigen_lib.Right t -> set_persistent_data ~table:t ~sp value

  let unset ~sp (_, table) =
    match table with
      | Ocsigen_lib.Left t -> remove_volatile_data ~table:t ~sp ();
        Lwt.return ()
      | Ocsigen_lib.Right t -> remove_persistent_data ~table:t ~sp ()
end

(*****************************************************************************)
(** Close a state *)
let discard_persistent_data ?state_name ?(scope = `Session) ?secure ~sp () = 
  match secure with
    | None ->
        Eliommod_persess.close_persistent_session ?state_name ~scope 
          ~secure:(Some true) ~sp () >>=
        Eliommod_persess.close_persistent_session ?state_name ~scope 
          ~secure:(Some false) ~sp
    | _ ->
        Eliommod_persess.close_persistent_session ?state_name ~scope 
          ~secure ~sp ()

let discard_services ?state_name ?(scope = `Session) ?secure ~sp () = 
  match secure with
    | None ->
        Eliommod_sersess.close_service_session 
          ?state_name ~scope ~secure:(Some true) ~sp ();
        Eliommod_sersess.close_service_session 
          ?state_name ~scope ~secure:(Some false) ~sp ()
    | _ ->
        Eliommod_sersess.close_service_session 
          ?state_name ~scope ~secure ~sp ()

let discard_volatile_data ?state_name ?(scope = `Session) ?secure ~sp () =
  match secure with
    | None ->
        Eliommod_datasess.close_data_session 
          ?state_name ~scope ~secure:(Some true) ~sp ();
        Eliommod_datasess.close_data_session 
          ?state_name ~scope ~secure:(Some false) ~sp ()
    | _ ->
        Eliommod_datasess.close_data_session 
          ?state_name ~scope ~secure ~sp ()

let discard_data ?persistent ?state_name ?scope ?secure ~sp () =
  (match persistent with
    | None | Some false ->
      discard_volatile_data ?state_name ?scope ?secure ~sp ()
    | _ -> ());
  (match persistent with
    | None | Some true ->
      discard_persistent_data ?state_name ?scope ?secure ~sp ()
    | _ -> Lwt.return ())

let discard ?state_name ?(scope = `Session) ?secure ~sp () =
  discard_services ?state_name ~scope ?secure ~sp ();
  discard_data ?state_name ~scope ?secure ~sp ()

let close_session ?state_name ?secure ~sp () =
  discard ?state_name ?secure ~sp ()

let close_group ?state_name ?secure ~sp () =
  discard ?state_name ~scope:`Session_group ?secure ~sp ()

let discard_all_volatile_data
    ?state_name ?(cookie_scope = `Session) ?sp () =
  let sitedata = find_sitedata "close_all_data_sessions" sp in
  Eliommod_sessadmin.close_all_data_sessions
    ?state_name ~cookie_scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all_persistent_data
    ?state_name ?(cookie_scope = `Session) ?sp () =
  let sitedata = find_sitedata "close_all_persistent_sessions" sp in
  Eliommod_sessadmin.close_all_persistent_sessions
    ?state_name ~cookie_scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all_data ?persistent ?state_name ?cookie_scope ?sp () =
  (match persistent with
    | None | Some false ->
      discard_all_volatile_data ?state_name ?cookie_scope ?sp ()
    | _ -> Lwt.return ()) >>= fun () ->
  (match persistent with
    | None | Some true ->
      discard_all_persistent_data ?state_name ?cookie_scope ?sp ()
    | _ -> Lwt.return ())
(*VVV missing ~secure? *)

let discard_all_services
    ?state_name ?(cookie_scope = `Session) ?sp () =
  let sitedata = find_sitedata "close_all_service_sessions" sp in
  Eliommod_sessadmin.close_all_service_sessions
    ?state_name ~cookie_scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all ?state_name ?cookie_scope ?sp () =
 discard_all_services ?state_name ?cookie_scope ?sp () >>=
 discard_all_data ?state_name ?cookie_scope ?sp
(*VVV missing ~secure? *)


(*****************************************************************************)
(* Administration *)

module Session_admin = struct

  (** Type used to describe session timeouts *)

  type timeout = Eliom_common.timeout =
    | TGlobal (** see global setting *)
    | TNone   (** explicitely set no timeout *)
    | TSome of float (** timeout duration in seconds *)

  type service_session =
      string (* cookie value *)
        *
      (Eliom_common.fullsessionname (* fullsessname *) *
       Eliom_common.tables     (* session table *) *
       float option ref        (* expiration date by timeout
                                  (server side) *) *
       Eliom_common.timeout ref    (* user timeout *) *
       Eliom_common.cookie_scope Eliom_common.sessgrp ref (* session group *) *
       string Ocsigen_cache.Dlist.node) *
       Eliom_common.sitedata

  type data_session =
      string (* cookie value *)
        *
      (Eliom_common.fullsessionname (* fullsessname *) *
       float option ref        (* expiration date by timeout
                                  (server side) *) *
       Eliom_common.timeout ref   (* user timeout *) *
       Eliom_common.cookie_scope Eliom_common.sessgrp ref (* session group *) *
       string Ocsigen_cache.Dlist.node) *
       Eliom_common.sitedata

  type persistent_session =
      string (* cookie value *)
        *
      (Eliom_common.fullsessionname (* fullsessname *) *
       float option            (* expiration date by timeout
                                  (server side) *) *
       Eliom_common.timeout        (* user timeout *) *
       Eliom_common.perssessgrp option           (* session group *))


  let close_service_session ?(close_group = false)
(*VVV Is it the right interface for closing group? *)
      ~session:(cookie, (_, _, _, _, sgr, sgrnode), sitedata) =
    if close_group then
      match Eliommod_sessiongroups.Serv.find_node_in_group_of_groups !sgr with
        | Some (_, node) -> Eliommod_sessiongroups.Serv.remove node
        | None -> (* We want to close the group of a tab session,
                     that is, the browser session associated. *)
          let grp = Eliommod_sessiongroups.make_full_named_group_name_
            ~cookie_scope:`Client_process sitedata cookie
          in
(*VVV à vérifier *)
          Eliommod_sessiongroups.Serv.remove_group grp
    else
      Eliommod_sessiongroups.Serv.remove sgrnode

  let close_volatile_data_session ?(close_group = false)
(*VVV Is it the right interface for closing group? *)
      ~session:(cookie, (_, _, _, sgr, sgrnode), sitedata) =
    if close_group then
      match Eliommod_sessiongroups.Data.find_node_in_group_of_groups !sgr with
        | Some node -> Eliommod_sessiongroups.Data.remove node
        | None -> (* We want to close the group of a tab session,
                     that is, the browser session associated. *)
          let grp = Eliommod_sessiongroups.make_full_named_group_name_
            ~cookie_scope:`Client_process sitedata cookie
          in
(*VVV à vérifier *)
          Eliommod_sessiongroups.Data.remove_group grp
    else
      Eliommod_sessiongroups.Data.remove sgrnode

  let close_persistent_data_session ?(close_group = false)
(*VVV Is it the right interface for closing group? *)
      ~session:(cookie, ((cookie_scope, _), _, _, sg)) =
    failwith "Eliom_state.close_persistent_data_session not implemented"
(*    if close_group then
      Eliommod_sessiongroups.Pers.remove_group sg
    else
      Eliommod_persess.close_persistent_session2
        ~cookie_scope ???
        sg cookie
*)

  let get_volatile_session_data ~session:(cookie, _, _) ~table:(_, _, _, t) =
    Eliom_common.SessionCookies.find t cookie

  let get_persistent_session_data ~session:(cookie, _) ~table:(_, _, _, t) =
    Ocsipersist.find t cookie >>= fun (_, a) -> Lwt.return a

  let remove_volatile_session_data ~session:(cookie, _, _) ~table:(_, _, _, t) =
    Eliom_common.SessionCookies.remove t cookie

  let remove_persistent_session_data ~session:(cookie, _) ~table:(_, _, _, t) =
    Ocsipersist.remove t cookie

  let get_service_state_name ~session:(_, ((_, s), _, _, _, _, _), _) =
    try
      Some (snd (Ocsigen_lib.sep '|' s))
    with Not_found -> None

  let get_volatile_data_state_name ~session:(_, ((_, s), _, _, _, _), _) =
    try
      Some (snd (Ocsigen_lib.sep '|' s))
    with Not_found -> None

  let get_persistent_data_state_name ~session:(_, ((_, s), _, _, _)) =
    try
      Some (snd (Ocsigen_lib.sep '|' s))
    with Not_found -> None

  let get_service_session_cookie_scope ~session:(_, ((ct, _), _, _, _, _, _), _) =
    ct

  let get_volatile_data_session_cookie_scope ~session:(_, ((ct, _), _, _, _, _), _) =
    ct

  let get_persistent_data_session_cookie_scope ~session:(_, ((ct, _), _, _, _)) =
    ct

  let set_service_session_timeout ~session:(_, (_, _, _, r, _, _), _) t =
    match t with
    | None -> r := TNone
    | Some t -> r := TSome t

  let set_volatile_data_session_timeout ~session:(_, (_, _, r, _, _), _) t =
    match t with
    | None -> r := TNone
    | Some t -> r := TSome t

  let set_persistent_data_session_timeout
      ~session:(cookie, (fullsessname, exp, _, sessgrp)) t =
    let ti = match t with
    | None -> TNone
    | Some t -> TSome t
    in
    Ocsipersist.add
      (Lazy.force Eliom_common.persistent_cookies_table)
      cookie
      (fullsessname, exp, ti, sessgrp)

  let get_service_session_timeout ~session:(_, (_, _, _, r, _, _), _) =
    !r

  let get_volatile_data_session_timeout ~session:(_, (_, _, r, _, _), _) =
    !r

  let get_persistent_data_session_timeout ~session:(_, (_, _, r, _)) =
    r


  let unset_service_session_timeout ~session:(_, (_, _, _, r, _, _), _) =
    r := TGlobal

  let unset_volatile_data_session_timeout ~session:(cookie, (_, _, r, _, _), _) =
    r := TGlobal

  let unset_persistent_data_session_timeout
      ~session:(cookie, (fullsessname, exp, _, sessgrp)) =
    Ocsipersist.add
      (Lazy.force Eliom_common.persistent_cookies_table)
      cookie
      (fullsessname, exp, TGlobal, sessgrp)


  (** Iterator on service sessions *)
  let iter_service_sessions ?sp f =
    let sitedata =
      find_sitedata "Admin.iter_service_sessions" sp
    in
    Eliommod_sessexpl.iter_service_sessions sitedata f

  (** Iterator on data sessions *)
  let iter_volatile_data_sessions ?sp f =
    let sitedata = find_sitedata "Admin.iter_volatile_data_sessions" sp in
    Eliommod_sessexpl.iter_data_sessions sitedata f

  (** Iterator on persistent sessions *)
  let iter_persistent_data_sessions = Eliommod_sessexpl.iter_persistent_sessions

  (** Iterator on service sessions *)
  let fold_service_sessions ?sp f beg =
  let sitedata = find_sitedata "Admin.fold_service_sessions" sp in
  Eliommod_sessexpl.fold_service_sessions sitedata f beg

  (** Iterator on data sessions *)
  let fold_volatile_data_sessions ?sp f beg =
    let sitedata =
      find_sitedata "Admin.fold_volatile_data_sessions" sp
    in
    Eliommod_sessexpl.fold_data_sessions sitedata f beg

  (** Iterator on persistent sessions *)
  let fold_persistent_data_sessions = Eliommod_sessexpl.fold_persistent_sessions

end



(*****************************************************************************)
(* Exploration *)

let number_of_service_sessions = Eliommod_sessexpl.number_of_service_sessions

let number_of_volatile_data_sessions = Eliommod_sessexpl.number_of_data_sessions

let number_of_tables = Eliommod_sessexpl.number_of_tables

let number_of_table_elements = Eliommod_sessexpl.number_of_table_elements

let number_of_persistent_data_sessions =
  Eliommod_sessexpl.number_of_persistent_sessions

let number_of_persistent_tables = Eliommod_persess.number_of_persistent_tables
  (* One table is the main table of sessions *)

let number_of_persistent_table_elements () =
  Eliommod_persess.number_of_persistent_table_elements ()

(*****************************************************************************)
let sp_of_esp = Ocsigen_lib.id
let esp_of_sp = Ocsigen_lib.id




(*****************************************************************************)
(** {2 User cookies} *)

let change_pathopt_ sp = function
  | None -> sp.Eliom_common.sp_sitedata.Eliom_common.site_dir
    (* Not possible to set a cookie for another site (?) *)
  | Some p -> sp.Eliom_common.sp_sitedata.Eliom_common.site_dir@p

let set_cookie
    ~sp ?(cookie_scope = `Session) ?path ?exp ~name ~value 
    ?(secure = false) () =
  let path = change_pathopt_ sp path in
  match cookie_scope with
    | `Session ->
      sp.Eliom_common.sp_user_cookies <- Ocsigen_cookies.add_cookie
        path name (Ocsigen_cookies.OSet (exp, value, secure))
        sp.Eliom_common.sp_user_cookies
    | `Client_process ->
      sp.Eliom_common.sp_user_tab_cookies <- Ocsigen_cookies.add_cookie
        path name (Ocsigen_cookies.OSet (exp, value, secure))
        sp.Eliom_common.sp_user_tab_cookies

let unset_cookie
    ~sp ?(cookie_scope = `Session) ?path ~name () =
  let path = change_pathopt_ sp path in
  match cookie_scope with
    | `Session ->
      sp.Eliom_common.sp_user_cookies <- Ocsigen_cookies.add_cookie
        path name Ocsigen_cookies.OUnset sp.Eliom_common.sp_user_cookies
    | `Client_process ->
      sp.Eliom_common.sp_user_tab_cookies <- Ocsigen_cookies.add_cookie
        path name Ocsigen_cookies.OUnset sp.Eliom_common.sp_user_tab_cookies

