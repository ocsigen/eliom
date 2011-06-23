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

open Eliom_pervasives

open Lwt
open Ocsigen_extensions

let get_csp_original_full_path () =
  let cpi = Eliom_request_info.get_sp_client_process_info () in
  cpi.Eliom_common.cpi_original_full_path

let get_csp_hostname () =
  let cpi = Eliom_request_info.get_sp_client_process_info () in
  cpi.Eliom_common.cpi_hostname

let get_csp_server_port () =
  let cpi = Eliom_request_info.get_sp_client_process_info () in
  cpi.Eliom_common.cpi_server_port

let get_csp_ssl () =
  let cpi = Eliom_request_info.get_sp_client_process_info () in
  cpi.Eliom_common.cpi_ssl

let get_csp_original_full_path_sp sp =
  let cpi = Eliom_request_info.get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_original_full_path

let get_csp_hostname_sp sp =
  let cpi = Eliom_request_info.get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_hostname

let get_csp_server_port_sp sp =
  let cpi = Eliom_request_info.get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_server_port

let get_csp_ssl_sp sp =
  let cpi = Eliom_request_info.get_sp_client_process_info_sp sp in
  cpi.Eliom_common.cpi_ssl


(* Expired session? *)
type state_status = Alive_state | Empty_state | Expired_state

let service_state_status ~scope ?secure () =
  try
    ignore (Eliommod_sersess.find_service_cookie_only
               ~scope ~secure ());
    Alive_state
  with
    | Not_found -> Empty_state
    | Eliom_common.Eliom_Session_expired -> Expired_state

let volatile_data_state_status ~scope ?secure () =
  try
    ignore (Eliommod_datasess.find_data_cookie_only
               ~scope ~secure ());
    Alive_state
  with
    | Not_found -> Empty_state
    | Eliom_common.Eliom_Session_expired -> Expired_state

let persistent_data_state_status ~scope ?secure () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only ~scope ~secure () >>= fun _ ->
      return Alive_state
    )
    (function
       | Not_found -> Lwt.return Empty_state
       | Eliom_common.Eliom_Session_expired -> Lwt.return Expired_state
       | e -> fail e)

(************)
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

let set_default_global_service_state_timeout ~cookie_scope
    ?(override_configfile = false) timeout =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_service_timeout"
  in
  Eliommod_timeouts.set_default_global_service_timeout
    cookie_scope override_configfile false sitedata timeout


let set_global_service_state_timeout
    ~scope
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_service_timeout"
  in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  Eliommod_timeouts.set_global_service_timeout
    ~scope_name ~cookie_scope ~recompute_expdates
    override_configfile sitedata timeout

let set_default_global_volatile_data_state_timeout ~cookie_scope
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_data_timeout" in
  Eliommod_timeouts.set_default_global_data_timeout
    cookie_scope override_configfile false sitedata timeout

let set_global_volatile_data_state_timeout
    ~scope
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_data_timeout" in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  Eliommod_timeouts.set_global_data_timeout
    ~scope_name ~cookie_scope
    ~recompute_expdates override_configfile sitedata timeout

let set_default_global_volatile_state_timeout ~cookie_scope
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_volatile_timeouts" in
  Eliommod_timeouts.set_default_global_service_timeout
    cookie_scope override_configfile false sitedata timeout;
  Eliommod_timeouts.set_default_global_data_timeout
    cookie_scope override_configfile false sitedata timeout

let set_global_volatile_state_timeout
    ~scope
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_volatile_timeouts" in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  Eliommod_timeouts.set_global_service_timeout
    ~scope_name ~cookie_scope ~recompute_expdates
    override_configfile sitedata timeout;
  Eliommod_timeouts.set_global_data_timeout
    ~scope_name ~cookie_scope ~recompute_expdates
    override_configfile sitedata timeout

let set_default_global_persistent_data_state_timeout ~cookie_scope
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_persistent_timeout" in
  Eliommod_timeouts.set_default_global_service_timeout
    cookie_scope override_configfile false sitedata timeout

let set_global_persistent_data_state_timeout
    ~scope
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let sitedata = Eliom_request_info.find_sitedata "set_global_persistent_timeout" in
  Eliommod_timeouts.set_global_persistent_timeout
    ~scope_name ~cookie_scope ~recompute_expdates
    override_configfile sitedata timeout


let get_global_service_state_timeout
    ~scope () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_timeout" in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  Eliommod_timeouts.get_global_service_timeout ~scope_name ~cookie_scope sitedata

let get_global_volatile_data_state_timeout
    ~scope () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_timeout" in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  Eliommod_timeouts.get_global_data_timeout
    ~scope_name ~cookie_scope sitedata

let get_global_persistent_data_state_timeout
    ~scope () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_persistent_timeout" in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  Eliommod_timeouts.get_global_persistent_timeout ~scope_name ~cookie_scope sitedata



(* Now for current session *)
let set_service_state_timeout ~scope ?secure t =
  let c =
    Eliommod_sersess.find_or_create_service_cookie ~scope ~secure ()
  in
  let tor = c.Eliom_common.sc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t

let set_volatile_data_state_timeout ~scope ?secure t =
  let c =
    Eliommod_datasess.find_or_create_data_cookie ~scope ~secure ()
  in
  let tor = c.Eliom_common.dc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t


let unset_service_state_timeout ~scope ?secure () =
  try
    let c =
      Eliommod_sersess.find_service_cookie_only ~scope ~secure ()
    in
    let tor = c.Eliom_common.sc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()

let unset_volatile_data_state_timeout ~scope ?secure () =
  try
    let c =
      Eliommod_datasess.find_data_cookie_only ~scope ~secure ()
    in
    let tor = c.Eliom_common.dc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()


let get_service_state_timeout ~scope ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  try
    let c =
      Eliommod_sersess.find_service_cookie_only ~scope ~secure ~sp ()
    in
    let tor = c.Eliom_common.sc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global_service_timeout
          ~scope_name ~cookie_scope sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global_service_timeout
      ~scope_name ~cookie_scope sitedata

let get_volatile_data_state_timeout ~scope ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  try
    let c =
      Eliommod_datasess.find_data_cookie_only ~scope ~secure ~sp ()
    in
    let tor = c.Eliom_common.dc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global_data_timeout
          ~scope_name ~cookie_scope sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global_data_timeout ~scope_name ~cookie_scope sitedata







let set_persistent_data_state_timeout ~scope ?secure t =
  lwt c = Eliommod_persess.find_or_create_persistent_cookie
    ~scope ~secure () in
  let tor = c.Eliom_common.pc_timeout in
  return
      (match t with
      | None -> tor := Eliom_common.TNone
      | Some t -> tor := Eliom_common.TSome t)

let unset_persistent_data_state_timeout ~scope ?secure () =
  try_lwt
    lwt c = Eliommod_persess.find_persistent_cookie_only
      ~scope ~secure () in
    let tor = c.Eliom_common.pc_timeout in
    tor := Eliom_common.TGlobal;
    return ()
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> return ()

let get_persistent_data_state_timeout ~scope ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  let scope_name = Eliom_common_base.scope_name_of_scope scope in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  try_lwt
    lwt c = Eliommod_persess.find_persistent_cookie_only
      ~scope ~secure ~sp () in
    let tor = c.Eliom_common.pc_timeout in
    return
      (match !tor with
        | Eliom_common.TGlobal ->
          Eliommod_timeouts.get_global_persistent_timeout
            ~scope_name ~cookie_scope sitedata
        | Eliom_common.TNone -> None
        | Eliom_common.TSome t -> Some t)
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired ->
      return
        (Eliommod_timeouts.get_global_persistent_timeout
           ~scope_name ~cookie_scope sitedata)


(* Preventing memory leaks: we must close empty sessions *)

let rec close_service_state_if_empty
    ~scope ?secure () =
  (* Close the session if it has not services inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c = Eliommod_sersess.find_service_cookie_only ~scope ~secure ~sp ()
    in
    match scope with
      | `Session _ ->
(*VVV ???        (match !(c.Eliom_common.sc_session_group) with
          | (_, _, Right _) (* no group *)
              when *)
        if
          (Eliommod_sessiongroups.Data.group_size
             (sitedata.Eliom_common.site_dir_string, `Client_process,
              Left c.Eliom_common.sc_value)
           = 0) (* no tab sessions *)
          &&
            (Eliom_common.service_tables_are_empty !(c.Eliom_common.sc_table))
        then
          Eliommod_sessiongroups.Data.remove
            c.Eliom_common.sc_session_group_node
      | `Client_process _ ->
        if (Eliom_common.service_tables_are_empty !(c.Eliom_common.sc_table))
        then Eliommod_sessiongroups.Data.remove
          c.Eliom_common.sc_session_group_node
      | `Session_group scope_name -> (* There is a browser session, we do not close the group,
                     but we may close the browser session (this will close
                     the group if it is empty). *)
        close_service_state_if_empty
          ~scope:(`Session scope_name) ?secure ()
  with Not_found -> ()



let rec close_volatile_state_if_empty ~scope ?secure () =
  (* Close the session if it has not data inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c = Eliommod_datasess.find_data_cookie_only ~scope ~secure ~sp ()
    in
    match scope with
      | `Session _ ->
        (match !(c.Eliom_common.dc_session_group) with
          | (_, _, Right _) (* no group *)
              when
                (Eliommod_sessiongroups.Data.group_size
                   (sitedata.Eliom_common.site_dir_string, `Client_process,
                    Left c.Eliom_common.dc_value)
                 = 0) (* no tab sessions *)
                &&
                  (sitedata.Eliom_common.not_bound_in_data_tables
                     c.Eliom_common.dc_value)
                ->
            Eliommod_sessiongroups.Data.remove
              c.Eliom_common.dc_session_group_node
          | _ -> ())
      | `Client_process _ -> ()
(* This should never occure, because we always have tab session data
   when we have a tab session (at least the change_page_event).
        if (sitedata.Eliom_common.not_bound_in_data_tables
              c.Eliom_common.dc_value)
        then Eliommod_sessiongroups.Data.remove
          c.Eliom_common.dc_session_group_node *)
      | `Session_group scope_name -> (* There is a browser session, we do not close the group,
                     but we may close the browser session (this will close
                     the group if it is empty). *)
        close_volatile_state_if_empty
          ~scope:(`Session scope_name) ?secure ()
  with Not_found -> ()




let rec close_persistent_state_if_empty
    ~scope ?secure () =
  Lwt.return ()
(*VVV Can we implement this function? *)


(* session groups *)

type 'a state_data =
  | No_data
  | Data_session_expired
  | Data of 'a


let set_service_session_group ?set_max ~(scope:Eliom_common.session_scope) ?secure session_group =
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ~set_session_group:session_group
      ~scope:(scope:>Eliom_common.user_scope) ~secure ()
  in
  match set_max with
    | None -> ()
    | Some m ->
        Eliommod_sessiongroups.Data.set_max
          c.Eliom_common.sc_session_group_node m

let unset_service_session_group ?set_max ~(scope:Eliom_common.session_scope) ?secure () =
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c =
      Eliommod_sersess.find_service_cookie_only
         ~scope:(scope:>Eliom_common.user_scope) ~secure ~sp ()
    in
    let n =
      Eliommod_sessiongroups.make_full_group_name
        ~cookie_scope:`Session
        (Eliom_request_info.get_request_sp sp).Ocsigen_extensions.request_info
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
    close_service_state_if_empty
      ~scope:(scope:>Eliom_common.user_scope) ?secure ()

  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> ()

let get_service_session_group ~(scope:Eliom_common.session_scope) ?secure () =
  try
    let c =
      Eliommod_sersess.find_service_cookie_only
	~scope:(scope:>Eliom_common.user_scope) ~secure ()
    in
    match !(c.Eliom_common.sc_session_group) with
      | _, _, Right _ -> None
      | _, _, Left v -> Some v
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> None



let set_volatile_data_session_group
    ?set_max ~(scope:Eliom_common.session_scope)
    ?secure session_group =
  let c =
    Eliommod_datasess.find_or_create_data_cookie
      ~set_session_group:session_group
      ~scope:(scope:>Eliom_common.user_scope)
      ~secure ()
  in
  match set_max with
    | None -> ()
    | Some m ->
        Eliommod_sessiongroups.Data.set_max
          c.Eliom_common.dc_session_group_node m

let unset_volatile_data_session_group ?set_max
    ~(scope:Eliom_common.session_scope) ?secure () =
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c =
      Eliommod_datasess.find_data_cookie_only
        ~scope:(scope:>Eliom_common.user_scope)
	~secure ~sp ()
    in
    let n =
      Eliommod_sessiongroups.make_full_group_name
        ~cookie_scope:`Session
        (Eliom_request_info.get_request_sp sp).Ocsigen_extensions.request_info
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
    close_volatile_state_if_empty
      ~scope:(scope:>Eliom_common.user_scope) ?secure ()

  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> ()

let get_volatile_data_session_group ~(scope:Eliom_common.session_scope) ?secure () =
  try
    let c =
      Eliommod_datasess.find_data_cookie_only
        ~scope:(scope:>Eliom_common.user_scope) ~secure ()
    in
    match !(c.Eliom_common.dc_session_group) with
      | _, _, Right _ -> None
      | _, _, Left v -> Some v
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> None

let set_persistent_data_session_group ?set_max
    ~(scope:Eliom_common.session_scope) ?secure n =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  lwt c = Eliommod_persess.find_or_create_persistent_cookie
    ~scope:(scope:>Eliom_common.user_scope) ~secure ~sp () in
  let n =
    Eliommod_sessiongroups.make_persistent_full_group_name
      ~cookie_scope:`Session
      sitedata.Eliom_common.site_dir_string (Some n)
  in
  let grp = c.Eliom_common.pc_session_group in
  lwt l = Eliommod_sessiongroups.Pers.move
    sitedata
    ?set_max
    (fst sitedata.Eliom_common.max_persistent_data_sessions_per_group)
    c.Eliom_common.pc_value !grp n in
  lwt () = Lwt_util.iter
    (Eliommod_persess.close_persistent_session2
       ~cookie_scope:`Session sitedata None) l in
  grp := n;
  Lwt.return ()

let unset_persistent_data_session_group ~(scope:Eliom_common.session_scope)
    ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  try_lwt
    lwt c = Eliommod_persess.find_persistent_cookie_only
      ~scope:(scope:>Eliom_common.user_scope)
      ~secure ~sp () in
    let grp = c.Eliom_common.pc_session_group in
    lwt () = Eliommod_sessiongroups.Pers.remove
      sitedata c.Eliom_common.pc_value !grp in
    grp := None;

    close_persistent_state_if_empty
      ~scope:(scope:>Eliom_common.user_scope) ?secure ()
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> Lwt.return ()

let get_persistent_data_session_group ~(scope:Eliom_common.session_scope)
    ?secure () =
  try_lwt
    lwt c = Eliommod_persess.find_persistent_cookie_only
      ~scope:(scope:>Eliom_common.user_scope) ~secure () in
    Lwt.return (match !(c.Eliom_common.pc_session_group) with
      | None -> None
      | Some v ->
        match Eliommod_sessiongroups.getperssessgrp v with
          | (_, _, Left s) -> Some s
          | _ -> None)
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> Lwt.return None




(* max *)
let set_default_max_service_sessions_per_group
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_service_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_service_sessions_per_group in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_service_sessions_per_group <- (n, b)

let set_default_max_volatile_data_sessions_per_group
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_volatile_data_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_sessions_per_group in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_volatile_data_sessions_per_group <- (n, b)

let set_default_max_persistent_data_sessions_per_group
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_persistent_data_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_persistent_data_sessions_per_group in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_persistent_data_sessions_per_group <- (n, b)

let set_default_max_service_sessions_per_subnet
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_service_sessions_per_subnet"
  in
  let b = snd sitedata.Eliom_common.max_service_sessions_per_subnet in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_service_sessions_per_subnet <- (n, b)

let set_default_max_volatile_data_sessions_per_subnet
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_volatile_data_sessions_per_subnet"
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_sessions_per_subnet in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_volatile_data_sessions_per_subnet <- (n, b)

let set_default_max_volatile_sessions_per_group ?override_configfile n =
  set_default_max_service_sessions_per_group ?override_configfile n;
  set_default_max_volatile_data_sessions_per_group ?override_configfile n

let set_default_max_volatile_sessions_per_subnet ?override_configfile n =
  set_default_max_service_sessions_per_subnet ?override_configfile n;
  set_default_max_volatile_data_sessions_per_subnet ?override_configfile n



let set_default_max_service_tab_sessions_per_group
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_service_tab_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_service_tab_sessions_per_group in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_service_tab_sessions_per_group <- (n, b)

let set_default_max_volatile_data_tab_sessions_per_group
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_volatile_data_tab_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group <- (n, b)

let set_default_max_persistent_data_tab_sessions_per_group
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_persistent_data_tab_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group <- (n, b)

let set_default_max_volatile_tab_sessions_per_group ?override_configfile n =
  set_default_max_service_tab_sessions_per_group ?override_configfile n;
  set_default_max_volatile_data_tab_sessions_per_group ?override_configfile n


let set_default_max_anonymous_services_per_session
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_anonymous_services_per_session"
  in
  let b = snd sitedata.Eliom_common.max_anonymous_services_per_session in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_anonymous_services_per_session <- (n, b)

let set_default_max_anonymous_services_per_subnet
    ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_default_max_anonymous_services_per_subnet"
  in
  let b = snd sitedata.Eliom_common.max_anonymous_services_per_subnet in
  if override_configfile || not b
  then
    sitedata.Eliom_common.max_anonymous_services_per_subnet <- (n, b)


let set_ipv4_subnet_mask ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_ipv4_subnet_mask"
  in
  let b = snd sitedata.Eliom_common.ipv4mask in
  if override_configfile || not b
  then
    sitedata.Eliom_common.ipv4mask <- (Some n, b)

let set_ipv6_subnet_mask ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata "set_ipv6_subnet_mask"
  in
  let b = snd sitedata.Eliom_common.ipv6mask in
  if override_configfile || not b
  then
    sitedata.Eliom_common.ipv6mask <- (Some n, b)




let set_max_service_states_for_group_or_subnet ~scope ?secure m =
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ~scope ~secure ()
  in
  match scope with
    | `Session_group _ ->
      (match
          Eliommod_sessiongroups.Data.find_node_in_group_of_groups
            !(c.Eliom_common.sc_session_group)
       with
         | Some node -> Eliommod_sessiongroups.Data.set_max node m
         | _ -> ())
    | _ ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.sc_session_group_node m

let set_max_volatile_data_states_for_group_or_subnet
    ~scope ?secure m =
  let c =
    Eliommod_datasess.find_or_create_data_cookie ~scope ~secure ()
  in
  match scope with
    | `Session_group _ ->
      (match
          Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
            !(c.Eliom_common.dc_session_group)
       with
         | Some (_, node) -> Eliommod_sessiongroups.Data.set_max node m
         | _ -> ())
    | _ ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.dc_session_group_node m

let set_max_volatile_states_for_group_or_subnet
    ~scope ?secure m =
  set_max_service_states_for_group_or_subnet
    ~scope ?secure m;
  set_max_volatile_data_states_for_group_or_subnet
    ~scope ?secure m

(*VVV No version for persistent sessions? Why? *)






(* expiration dates *)
let set_service_cookie_exp_date ~scope ?secure t =
  let c = Eliommod_sersess.find_or_create_service_cookie ~scope ~secure () in
  let exp = c.Eliom_common.sc_cookie_exp in
  match t with
  | None -> exp := Eliom_common.CEBrowser
  | Some t -> exp := Eliom_common.CESome t

(*
let get_service_cookie_exp_date ?state_name ?(cookie_scope = `Session) ?secure () =
  try
    let (_, _, _, _, exp) = find_service_cookie_only ?state_name ~cookie_scope ~secure () in
  let exp = c.Eliom_common.sc_cookie_exp in
    !exp
  with Not_found | Eliom_common.Eliom_Session_expired -> Eliom_common.CEBrowser
*)

let set_volatile_data_cookie_exp_date ~scope ?secure t =
  let c = Eliommod_datasess.find_or_create_data_cookie ~scope ~secure () in
  let exp = c.Eliom_common.dc_cookie_exp in
  match t with
  | None -> exp := Eliom_common.CEBrowser
  | Some t -> exp := Eliom_common.CESome t



let set_persistent_data_cookie_exp_date ~scope ?secure t =
  lwt c = Eliommod_persess.find_or_create_persistent_cookie ~scope ~secure () in
  let exp = c.Eliom_common.pc_cookie_exp in
  return
      (match t with
      | None -> exp := Eliom_common.CEBrowser
      | Some t -> exp := Eliom_common.CESome t)

let get_persistent_data_cookie_exp_date ~scope ?secure () =
  try_lwt
    lwt c = Eliommod_persess.find_persistent_cookie_only
      ~scope ~secure () in
    return !(c.Eliom_common.pc_cookie_exp)
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired ->
      return Eliom_common.CEBrowser





(* *)
let get_global_table () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_table" in
  sitedata.Eliom_common.global_services


(** If the session does not exist, we create it
   (new cookie, new session service table) *)
let get_session_service_table ~sp ~scope ?secure () =
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ~scope ~secure ~sp ()
  in
  match scope with
    | `Session_group _ ->
      begin
	match
	  Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
            !(c.Eliom_common.sc_session_group)
	with None -> raise Not_found
	  | Some (t, _) -> t
      end
    | _ -> c.Eliom_common.sc_table

(** If the session does not exist, we raise Not_found *)
let get_session_service_table_if_exists ~sp
    ~scope ?secure () =
  try
    let c =
      Eliommod_sersess.find_service_cookie_only
        ~scope ~secure ~sp ()
    in
    match scope with
      | `Session_group _ ->
	begin
	  match
            Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
              !(c.Eliom_common.sc_session_group)
	  with None -> raise Not_found
            | Some (t, _) -> t
	end
      | _ -> c.Eliom_common.sc_table
  with Eliom_common.Eliom_Session_expired -> raise Not_found





(*****************************************************************************)
(** {2 persistent sessions} *)

open Ocsipersist

type 'a persistent_table =
    (Eliom_common.user_scope *
       bool *
       (int64 * 'a) Ocsipersist.table)

let create_persistent_table
    ~scope ?(secure = false) name : 'a persistent_table =
  let t = Eliom_common.create_persistent_table name in
  (scope, secure, t)


let get_table_key_
    ~table:(scope, secure, table)
    (find_cookie : scope:Eliom_common.user_scope ->
     secure:bool option ->
     ?sp:Eliom_common.server_params ->
     unit -> Eliom_common.one_persistent_cookie_info Lwt.t) =
  lwt key = match scope with
    | `Session_group state_name ->
      begin
        match_lwt get_persistent_data_session_group ~scope:(`Session state_name) ~secure () with
          | Some a -> Lwt.return a
          | _ -> Lwt.return Eliom_common.default_group_name
      end
    | _ ->
      lwt c = find_cookie ~scope ~secure:(Some secure) () in
      Lwt.return c.Eliom_common.pc_value
  in
  Lwt.return (table, key)

let get_persistent_data ~table () =
  catch
    (fun () ->
      get_table_key_ ~table Eliommod_persess.find_persistent_cookie_only
      >>= fun (table, key) ->
      Ocsipersist.find table key >>= fun (_, v) ->
      Lwt.return (Data v)
    )
    (function
      | Eliom_common.Eliom_Session_expired -> return Data_session_expired
      | Not_found -> return No_data
      | e -> fail e)

let set_persistent_data ~table value =
  let f__ ~scope ~secure ?sp () =
    Eliommod_persess.find_or_create_persistent_cookie
      ~scope ~secure ?sp ()
  in
  get_table_key_ ~table f__ >>= fun (table, key) ->
  Ocsipersist.add table key (Int64.zero, value)

let remove_persistent_data ~table () =
  try_lwt
    let (scope, secure, _) = table in
    lwt (table, key) = get_table_key_ ~table Eliommod_persess.find_persistent_cookie_only in
    lwt () = Ocsipersist.remove table key in

    close_persistent_state_if_empty ~scope ~secure ()
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> return ()


(*****************************************************************************)
(** {2 session data in memory} *)

type 'a volatile_table =
    (Eliom_common.user_scope *
       bool *
       'a Eliom_common.SessionCookies.t)

let create_volatile_table_during_session_ =
  Eliommod_datasess.create_volatile_table_during_session

let create_volatile_table ~scope ?(secure = false) () =
  match Eliom_common.get_sp_option () with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
        Eliommod_datasess.create_volatile_table ~scope ~secure
      | None -> raise
            (Eliom_common.Eliom_site_information_not_available
               "create_volatile_table"))
  | Some sp ->
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    create_volatile_table_during_session_
      ~scope ~secure sitedata

let get_table_key_ ~table:(scope, secure, table)
    (find_cookie : scope:Eliom_common.user_scope ->
     secure:bool option ->
     ?sp:Eliom_common.server_params ->
     unit -> Eliom_common.one_data_cookie_info) =
  let key = match scope with
    | `Session_group state_name ->
      (match get_volatile_data_session_group ~scope:(`Session state_name) ~secure ()
       with Some a -> a
         | _ -> Eliom_common.default_group_name)
    | _ ->
      let c =
        find_cookie ~scope ~secure:(Some secure) () in
      c.Eliom_common.dc_value
  in
  (table, key)


let get_volatile_data ~table () =
  try
    let (table, key) =
      get_table_key_ ~table Eliommod_datasess.find_data_cookie_only
    in
    Data (Eliom_common.SessionCookies.find table key)
  with
  | Not_found -> No_data
  | Eliom_common.Eliom_Session_expired -> Data_session_expired


let set_volatile_data ~table value =
  let f__ ~scope ~secure ?sp () =
    Eliommod_datasess.find_or_create_data_cookie
      ~scope ~secure ?sp ()
  in
  let (table, key) =
    get_table_key_ ~table f__
  in
  Eliom_common.SessionCookies.replace table key value


let remove_volatile_data ~table () =
  try
    let (scope, secure, _) = table in
    let (table, key) =
      get_table_key_ ~table Eliommod_datasess.find_data_cookie_only
    in
    Eliom_common.SessionCookies.remove table key;

    (* Now we want to close the session if it has not data inside
       and no group and no sub sessions *)
    close_volatile_state_if_empty ~scope ~secure ()

  with Not_found | Eliom_common.Eliom_Session_expired -> ()






(*****************************************************************************)
(** Close a state *)
let discard_persistent_data ~scope ?secure () =
  match secure with
    | None ->
        Eliommod_persess.close_persistent_session ~scope
          ~secure:(Some true) () >>= fun () ->
        Eliommod_persess.close_persistent_session ~scope
          ~secure:(Some false) ()
    | _ ->
        Eliommod_persess.close_persistent_session ~scope
          ~secure ()

let discard_services ~scope ?secure () =
  match secure with
    | None ->
        Eliommod_sersess.close_service_session
          ~scope ~secure:(Some true) ();
        Eliommod_sersess.close_service_session
          ~scope ~secure:(Some false) ()
    | _ ->
        Eliommod_sersess.close_service_session ~scope ~secure ()

let discard_volatile_data ~scope ?secure () =
  match secure with
    | None ->
        Eliommod_datasess.close_data_session
          ~scope ~secure:(Some true) ();
        Eliommod_datasess.close_data_session
          ~scope ~secure:(Some false) ()
    | _ ->
        Eliommod_datasess.close_data_session ~scope ~secure ()

let discard_data ?persistent ~scope ?secure () =
  (match persistent with
    | None | Some false ->
      discard_volatile_data ~scope ?secure ()
    | _ -> ());
  (match persistent with
    | None | Some true ->
      discard_persistent_data ~scope ?secure ()
    | _ -> Lwt.return ())

let discard ~scope ?secure () =
  discard_services ~scope:(scope:>[< Eliom_common.user_scope ]) ?secure ();
  discard_data ~scope:(scope:>[< Eliom_common.user_scope ]) ?secure ()

let discard_all_scopes  ?secure() =
  let discard_name scope_name =
    lwt () = discard ?secure ~scope:(`Session_group scope_name) () in
    lwt () = discard ?secure ~scope:(`Session scope_name) () in
    discard ?secure ~scope:(`Client_process scope_name) ()
  in
  Lwt_list.iter_p discard_name (Eliom_common.list_scope_names ())

let discard_all_volatile_data ~scope () =
  let sitedata = Eliom_request_info.find_sitedata "close_all_data_sessions" in
  Eliommod_sessadmin.close_all_data_sessions
    ~scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all_persistent_data ~scope () =
  let sitedata = Eliom_request_info.find_sitedata "close_all_persistent_sessions" in
  Eliommod_sessadmin.close_all_persistent_sessions
    ~scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all_data ?persistent ~scope () =
  lwt () = match persistent with
    | None | Some false ->
      discard_all_volatile_data ~scope ()
    | _ -> Lwt.return () in
  (match persistent with
    | None | Some true ->
      discard_all_persistent_data ~scope ()
    | _ -> Lwt.return ())
(*VVV missing ~secure? *)

let discard_all_services ~scope () =
  let sitedata = Eliom_request_info.find_sitedata "close_all_service_sessions" in
  Eliommod_sessadmin.close_all_service_sessions ~scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all ~scope () =
  lwt () = discard_all_services ~scope () in
  discard_all_data ~scope ()
(*VVV missing ~secure? *)

let discard_everything () =
  let discard_name scope_name =
    lwt () = discard_all ~scope:(`Session_group scope_name) () in
    lwt () = discard_all ~scope:(`Session scope_name) () in
    discard_all ~scope:(`Client_process scope_name) ()
  in
  Lwt_list.iter_p discard_name (Eliom_common.list_scope_names ())


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

  let get_volatile_session_data
      ~session:(cookie, _, _:data_session)
      ~table:(_, _, t:'a volatile_table) =
    Eliom_common.SessionCookies.find t cookie

  let get_persistent_session_data
      ~session:(cookie, _:persistent_session)
      ~table:(_, _, t:'a persistent_table) =
    Ocsipersist.find t cookie >>= fun (_, a) -> Lwt.return a

  let remove_volatile_session_data
      ~session:(cookie, _, _:data_session)
      ~table:(_, _, t:'a volatile_table) =
    Eliom_common.SessionCookies.remove t cookie

  let remove_persistent_session_data
      ~session:(cookie, _:persistent_session)
      ~table:(_, _, t:'a persistent_table) =
    Ocsipersist.remove t cookie

  let split_name s =
    try
      let _,name = String.sep '|' s in
      let p,q = String.sep '|' name in
      match p with
	| "ref" -> `Default_ref_name
	| "comet" -> `Default_comet_name
	| "" -> `String q
	| _ -> raise Not_found
    with
      | Not_found -> failwith ("Eliom_state.split_name: malformed name: " ^ s)

  let get_service_scope_name ~session:(_, ((_, s), _, _, _, _, _), _) =
    split_name s

  let get_volatile_data_scope_name ~session:(_, ((_, s), _, _, _, _), _) =
    split_name s

  let get_persistent_data_scope_name ~session:(_, ((_, s), _, _, _)) =
    split_name s

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
  let iter_service_sessions f =
    let sitedata =
      Eliom_request_info.find_sitedata "Admin.iter_service_sessions"
    in
    Eliommod_sessexpl.iter_service_sessions sitedata f

  (** Iterator on data sessions *)
  let iter_volatile_data_sessions f =
    let sitedata = Eliom_request_info.find_sitedata "Admin.iter_volatile_data_sessions" in
    Eliommod_sessexpl.iter_data_sessions sitedata f

  (** Iterator on persistent sessions *)
  let iter_persistent_data_sessions = Eliommod_sessexpl.iter_persistent_sessions

  (** Iterator on service sessions *)
  let fold_service_sessions f beg =
  let sitedata = Eliom_request_info.find_sitedata "Admin.fold_service_sessions" in
  Eliommod_sessexpl.fold_service_sessions sitedata f beg

  (** Iterator on data sessions *)
  let fold_volatile_data_sessions f beg =
    let sitedata =
      Eliom_request_info.find_sitedata "Admin.fold_volatile_data_sessions"
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
let get_service_cookie ~scope ?secure () =
  try
    let c = Eliommod_sersess.find_service_cookie_only
      ~scope ~secure () in
    Some c.Eliom_common.sc_value
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_volatile_data_cookie ~scope ?secure () =
  try
    let c = Eliommod_datasess.find_data_cookie_only ~scope ~secure () in
    Some c.Eliom_common.dc_value
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_persistent_data_cookie ~scope ?secure () =
  try_lwt
    lwt c = Eliommod_persess.find_persistent_cookie_only
      ~scope ~secure () in
    return (Some c.Eliom_common.pc_value)
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> return None

(*****************************************************************************)
(** {2 User cookies} *)

let change_pathopt_ sp = function
  | None -> (Eliom_request_info.get_sitedata_sp ~sp).Eliom_common.site_dir
    (* Not possible to set a cookie for another site (?) *)
  | Some p -> (Eliom_request_info.get_sitedata_sp ~sp).Eliom_common.site_dir@p

let set_cookie
    ?(cookie_scope = `Session) ?path ?exp ?(secure = false) ~name ~value () =
  let sp = Eliom_common.get_sp () in
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
    ?(cookie_scope = `Session) ?path ~name () =
  let sp = Eliom_common.get_sp () in
  let path = change_pathopt_ sp path in
  match cookie_scope with
    | `Session ->
      sp.Eliom_common.sp_user_cookies <- Ocsigen_cookies.add_cookie
        path name Ocsigen_cookies.OUnset sp.Eliom_common.sp_user_cookies
    | `Client_process ->
      sp.Eliom_common.sp_user_tab_cookies <- Ocsigen_cookies.add_cookie
        path name Ocsigen_cookies.OUnset sp.Eliom_common.sp_user_tab_cookies
