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

open Lwt
open Ocsigen_extensions

let get_csp_original_full_path () =
   match Eliom_request_info.get_sp_client_process_info () with
     | None -> Eliom_request_info.get_original_full_path ()
     | Some cpi -> cpi.Eliom_common.cpi_original_full_path

let get_csp_hostname () =
   match Eliom_request_info.get_sp_client_process_info () with
     | None -> Eliom_request_info.get_hostname ()
     | Some cpi -> cpi.Eliom_common.cpi_hostname

let get_csp_server_port () =
   match Eliom_request_info.get_sp_client_process_info () with
     | None -> Eliom_request_info.get_server_port ()
     | Some cpi -> cpi.Eliom_common.cpi_server_port

let get_csp_ssl () =
   match Eliom_request_info.get_sp_client_process_info () with
     | None -> Eliom_request_info.get_ssl ()
     | Some cpi -> cpi.Eliom_common.cpi_ssl

let get_csp_original_full_path_sp sp =
   match Eliom_request_info.get_sp_client_process_info_sp sp with
     | None -> Eliom_request_info.get_original_full_path_sp sp
     | Some cpi -> cpi.Eliom_common.cpi_original_full_path

let get_csp_hostname_sp sp =
   match Eliom_request_info.get_sp_client_process_info_sp sp with
     | None -> Eliom_request_info.get_hostname_sp sp
     | Some cpi -> cpi.Eliom_common.cpi_hostname

let get_csp_server_port_sp sp =
   match Eliom_request_info.get_sp_client_process_info_sp sp with
     | None -> Eliom_request_info.get_server_port_sp sp
     | Some cpi -> cpi.Eliom_common.cpi_server_port

let get_csp_ssl_sp sp =
   match Eliom_request_info.get_sp_client_process_info_sp sp with
     | None -> Eliom_request_info.get_ssl_sp sp
     | Some cpi -> cpi.Eliom_common.cpi_ssl


(* Expired session? *)
type state_status = Alive_state | Empty_state | Expired_state

let service_state_status
    ?state_name ?(cookie_scope = `Session) ?secure () =
  try
    ignore (Eliommod_sersess.find_service_cookie_only
              ?state_name ~cookie_scope ~secure ());
    Alive_state
  with
    | Not_found -> Empty_state
    | Eliom_common.Eliom_Session_expired -> Expired_state

let volatile_data_state_status
    ?state_name ?(cookie_scope = `Session) ?secure () =
  try
    ignore (Eliommod_datasess.find_data_cookie_only
              ?state_name ~cookie_scope ~secure ());
    Alive_state
  with
    | Not_found -> Empty_state
    | Eliom_common.Eliom_Session_expired -> Expired_state

let persistent_data_state_status
    ?state_name ?(cookie_scope = `Session) ?secure () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure () >>= fun _ ->
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

let set_global_service_state_timeout
    ?state_name ?(cookie_scope = `Session)
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_service_timeout"
  in
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
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_data_timeout" in
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
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_volatile_timeouts" in
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
    ?(recompute_expdates = false)
    ?(override_configfile = false) timeout =
  let sitedata = Eliom_request_info.find_sitedata "set_global_persistent_timeout" in
  match state_name with
    | Some state_name ->
        Eliommod_timeouts.set_global_persistent_timeout
          ~state_name ~cookie_scope ~recompute_expdates
          override_configfile sitedata timeout
    | None ->
        Eliommod_timeouts.set_default_global_service_timeout
          cookie_scope override_configfile false sitedata timeout


let get_global_service_state_timeout
    ?state_name ?(cookie_scope = `Session) () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_timeout" in
  Eliommod_timeouts.get_global_service_timeout ?state_name ~cookie_scope sitedata

let get_global_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_timeout" in
  Eliommod_timeouts.get_global_data_timeout
    ?state_name ~cookie_scope sitedata

let get_global_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_persistent_timeout" in
  Eliommod_timeouts.get_global_persistent_timeout ?state_name ~cookie_scope sitedata



(* Now for current session *)
let set_service_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure t =
  let c = 
    Eliommod_sersess.find_or_create_service_cookie ?state_name ~cookie_scope ~secure ()
  in
  let tor = c.Eliom_common.sc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t

let set_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure t =
  let c = 
    Eliommod_datasess.find_or_create_data_cookie
      ?state_name ~cookie_scope ~secure ()
  in
  let tor = c.Eliom_common.dc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t


let unset_service_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure () =
  try
    let c = 
      Eliommod_sersess.find_service_cookie_only ?state_name ~cookie_scope ~secure ()
    in
    let tor = c.Eliom_common.sc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()

let unset_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure () =
  try
    let c = 
      Eliommod_datasess.find_data_cookie_only ?state_name ~cookie_scope ~secure ()
    in
    let tor = c.Eliom_common.dc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()


let get_service_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  try
    let c = 
      Eliommod_sersess.find_service_cookie_only ?state_name ~cookie_scope ~secure ~sp () 
    in
    let tor = c.Eliom_common.sc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global_service_timeout 
          ?state_name ~cookie_scope sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global_service_timeout
      ?state_name ~cookie_scope sitedata

let get_volatile_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  try
    let c = 
      Eliommod_datasess.find_data_cookie_only ?state_name ~cookie_scope ~secure ~sp () 
    in
    let tor = c.Eliom_common.dc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global_data_timeout
          ?state_name ~cookie_scope sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global_data_timeout ?state_name ~cookie_scope sitedata







let set_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure t =
  Eliommod_persess.find_or_create_persistent_cookie
    ?state_name ~cookie_scope ~secure () >>= fun c ->
  let tor = c.Eliom_common.pc_timeout in
  return
      (match t with
      | None -> tor := Eliom_common.TNone
      | Some t -> tor := Eliom_common.TSome t)

let unset_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure () >>= fun c ->
      let tor = c.Eliom_common.pc_timeout in
      tor := Eliom_common.TGlobal;
      return ()
    )
    (function
       | Not_found | Eliom_common.Eliom_Session_expired -> return () 
       | e -> fail e)

let get_persistent_data_state_timeout
    ?state_name ?(cookie_scope = `Session) ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
      let tor = c.Eliom_common.pc_timeout in
      return
        (match !tor with
        | Eliom_common.TGlobal ->
            Eliommod_timeouts.get_global_persistent_timeout
              ~state_name ~cookie_scope sitedata
        | Eliom_common.TNone -> None
        | Eliom_common.TSome t -> Some t)
    )
    (function
      | Not_found | Eliom_common.Eliom_Session_expired ->
          return
            (Eliommod_timeouts.get_global_persistent_timeout
               ~state_name ~cookie_scope sitedata)
      | e -> fail e)


(* Preventing memory leaks: we must close empty sessions *)

let rec close_service_state_if_empty
    ?state_name ?(scope = `Session) ?secure () =
  (* Close the session if it has not services inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
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
          ~scope:`Session ?state_name ?secure ()
  with Not_found -> ()



let rec close_volatile_state_if_empty
    ?state_name ?(scope = `Session) ?secure () =
  (* Close the session if it has not data inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
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
          ~scope:`Session ?state_name ?secure ()
  with Not_found -> ()




let rec close_persistent_state_if_empty
    ?state_name ?(scope = `Session) ?secure () =
  Lwt.return ()
(*VVV Can we implement this function? *)


(* session groups *)

type 'a state_data =
  | No_data
  | Data_session_expired
  | Data of 'a


let set_service_session_group ?set_max ?state_name ?secure session_group =
  let cookie_scope = `Session in
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ~set_session_group:session_group ?state_name ~cookie_scope ~secure ()
  in
  match set_max with
    | None -> ()
    | Some m -> 
        Eliommod_sessiongroups.Data.set_max
          c.Eliom_common.sc_session_group_node m

let unset_service_session_group ?set_max ?state_name ?secure () =
  let cookie_scope = `Session in
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c = 
      Eliommod_sersess.find_service_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () 
    in
    let n =
      Eliommod_sessiongroups.make_full_group_name
        ~cookie_scope
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
    close_service_state_if_empty ~scope:`Session 
      ?state_name ?secure ()

  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> ()

let get_service_session_group ?state_name ?secure () =
  let cookie_scope = `Session in
  try
    let c = 
      Eliommod_sersess.find_service_cookie_only
        ?state_name ~cookie_scope ~secure () 
    in
    match !(c.Eliom_common.sc_session_group) with
      | _, _, Ocsigen_lib.Right _ -> None
      | _, _, Ocsigen_lib.Left v -> Some v
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> None



let set_volatile_data_session_group
    ?set_max ?state_name
    ?secure session_group =
  let cookie_scope = `Session in
  let c = 
    Eliommod_datasess.find_or_create_data_cookie
      ~set_session_group:session_group
      ?state_name ~cookie_scope ~secure () 
  in
  match set_max with
    | None -> ()
    | Some m -> 
        Eliommod_sessiongroups.Data.set_max
          c.Eliom_common.dc_session_group_node m

let unset_volatile_data_session_group ?set_max
    ?state_name ?secure () =
  let cookie_scope = `Session in
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c = 
      Eliommod_datasess.find_data_cookie_only
        ?state_name ~cookie_scope ~secure ~sp ()
    in
    let n =
      Eliommod_sessiongroups.make_full_group_name
        ~cookie_scope
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
    close_volatile_state_if_empty ~scope:`Session 
      ?state_name ?secure ()

  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> ()

let get_volatile_data_session_group ?state_name ?secure () =
  let cookie_scope = `Session in
  try
    let c =
      Eliommod_datasess.find_data_cookie_only
        ?state_name ~cookie_scope ~secure () 
    in
    match !(c.Eliom_common.dc_session_group) with
      | _, _, Ocsigen_lib.Right _ -> None
      | _, _, Ocsigen_lib.Left v -> Some v
  with
    | Not_found
    | Eliom_common.Eliom_Session_expired -> None

let set_persistent_data_session_group ?set_max
    ?state_name ?secure n =
  let cookie_scope = `Session in
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  Eliommod_persess.find_or_create_persistent_cookie
    ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
  let n =
    Eliommod_sessiongroups.make_persistent_full_group_name
      ~cookie_scope
      sitedata.Eliom_common.site_dir_string (Some n)
  in
  let grp = c.Eliom_common.pc_session_group in
  Eliommod_sessiongroups.Pers.move
    sitedata
    ?set_max
    (fst sitedata.Eliom_common.max_persistent_data_sessions_per_group)
    c.Eliom_common.pc_value !grp n >>= fun l ->
  Lwt_util.iter
    (Eliommod_persess.close_persistent_session2
       ~cookie_scope sitedata None) l >>= fun () ->
  grp := n;
  Lwt.return ()

let unset_persistent_data_session_group ?state_name ?secure () =
  let cookie_scope = `Session in
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  Lwt.catch
    (fun () ->
       Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure ~sp () >>= fun c ->
       let grp = c.Eliom_common.pc_session_group in
       Eliommod_sessiongroups.Pers.remove
         sitedata c.Eliom_common.pc_value !grp >>= fun () ->
       grp := None;

       close_persistent_state_if_empty ~scope:`Session 
         ?state_name ?secure ()

    )
    (function
       | Not_found
       | Eliom_common.Eliom_Session_expired -> Lwt.return ()
       | e -> fail e)


let get_persistent_data_session_group ?state_name ?secure () =
  let cookie_scope = `Session in
  catch
    (fun () ->
       Eliommod_persess.find_persistent_cookie_only
         ?state_name ~cookie_scope ~secure () >>= fun c ->
       Lwt.return (match !(c.Eliom_common.pc_session_group) with
                     | None -> None
                     | Some v ->
                       match Eliommod_sessiongroups.getperssessgrp v with
                         | (_, _, Ocsigen_lib.Left s) -> Some s
                         | _ -> None
       )
    )
    (function
       | Not_found
       | Eliom_common.Eliom_Session_expired -> Lwt.return None
       | e -> fail e)





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




let set_max_service_states_for_group_or_subnet
    ?state_name ?(scope = `Session) ?secure m =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ?state_name ~cookie_scope ~secure ()
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

let set_max_volatile_data_states_for_group_or_subnet
    ?state_name ?(scope = `Session) ?secure m =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let c =
    Eliommod_datasess.find_or_create_data_cookie
      ?state_name ~cookie_scope ~secure ()
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

let set_max_volatile_states_for_group_or_subnet
    ?state_name ?scope ?secure m =
  set_max_service_states_for_group_or_subnet
    ?scope ?state_name ?secure m;
  set_max_volatile_data_states_for_group_or_subnet
    ?scope ?state_name ?secure m

(*VVV No version for persistent sessions? Why? *)






(* expiration dates *)
let set_service_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure t =
  let c = 
    Eliommod_sersess.find_or_create_service_cookie ?state_name ~cookie_scope ~secure () 
  in
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

let set_volatile_data_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure t =
  let c = 
    Eliommod_datasess.find_or_create_data_cookie
      ?state_name ~cookie_scope ~secure () 
  in
  let exp = c.Eliom_common.dc_cookie_exp in
  match t with
  | None -> exp := Eliom_common.CEBrowser
  | Some t -> exp := Eliom_common.CESome t



let set_persistent_data_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure t =
  Eliommod_persess.find_or_create_persistent_cookie ?state_name ~cookie_scope ~secure ()
  >>= fun c ->
  let exp = c.Eliom_common.pc_cookie_exp in
  return
      (match t with
      | None -> exp := Eliom_common.CEBrowser
      | Some t -> exp := Eliom_common.CESome t)

let get_persistent_data_cookie_exp_date
    ?state_name ?(cookie_scope = `Session) ?secure () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure () >>= fun c ->
      return !(c.Eliom_common.pc_cookie_exp))
    (function
       | Not_found | Eliom_common.Eliom_Session_expired -> 
           return Eliom_common.CEBrowser
       | e -> fail e)





(* *)
let get_global_table () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_table" in
  sitedata.Eliom_common.global_services


(** If the session does not exist, we create it
   (new cookie, new session service table) *)
let get_session_service_table ~sp
    ?state_name ?(scope = `Session) ?secure () =
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
let get_session_service_table_if_exists ~sp
    ?state_name ?(scope = `Session) ?secure () =
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
    ~table:(scope, (state_name : string option), secure, table)
    (find_cookie : ?state_name:string ->
     ?cookie_scope:Eliom_common.cookie_scope ->
     secure:bool option ->
     ?sp:Eliom_common.server_params ->
     unit -> Eliom_common.one_persistent_cookie_info Lwt.t) =
  (match scope with
    | `Session_group ->
      (get_persistent_data_session_group ?state_name ~secure ()
       >>= function
         | Some a -> Lwt.return a
         | _ -> Lwt.return Eliom_common.default_group_name)
    | _ ->
      let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
      find_cookie ?state_name ~cookie_scope ~secure:(Some secure) () 
      >>= fun c -> Lwt.return c.Eliom_common.pc_value)
  >>= fun key ->
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

let f__ ?state_name ?cookie_scope ~secure ?sp () =
  Eliommod_persess.find_or_create_persistent_cookie
    ?state_name ?cookie_scope ~secure ?sp ()

let set_persistent_data ~table value =
  get_table_key_ ~table f__ >>= fun (table, key) ->
  Ocsipersist.add table key (Int64.zero, value)

let remove_persistent_data ~table () =
  catch
    (fun () ->
      let (scope, state_name, secure, _) = table in
      get_table_key_ ~table Eliommod_persess.find_persistent_cookie_only
      >>= fun (table, key) ->
      Ocsipersist.remove table key >>= fun () ->

      close_persistent_state_if_empty ~scope ?state_name ~secure ()
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

let create_volatile_table_during_session_ = 
  Eliommod_datasess.create_volatile_table_during_session

let create_volatile_table
    ?state_name ?(scope = `Session) ?(secure = false) () =
  match Eliom_common.get_sp_option () with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata -> 
        Eliommod_datasess.create_volatile_table ~scope ~state_name ~secure
      | None -> raise
            (Eliom_common.Eliom_site_information_not_available
               "create_volatile_table"))
  | Some sp ->
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    create_volatile_table_during_session_
      ~scope ~state_name ~secure sitedata

let get_table_key_ ~table:(scope, (state_name : string option), secure, table)
    (find_cookie : ?state_name:string ->
     ?cookie_scope:Eliom_common.cookie_scope ->
     secure:bool option ->
     ?sp:Eliom_common.server_params ->
     unit -> Eliom_common.one_data_cookie_info) =
  let key = match scope with
    | `Session_group ->
      (match get_volatile_data_session_group ?state_name ~secure () 
       with Some a -> a
         | _ -> Eliom_common.default_group_name)
    | _ -> 
      let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in 
      let c =
        find_cookie ?state_name ~cookie_scope ~secure:(Some secure) () in
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

let f__ ?state_name ?cookie_scope ~secure ?sp () =
  Eliommod_datasess.find_or_create_data_cookie
    ?state_name ?cookie_scope ~secure ?sp ()

let set_volatile_data ~table value =
  let (table, key) =
    get_table_key_ ~table f__
  in
  Eliom_common.SessionCookies.replace table key value


let remove_volatile_data ~table () =
  try
    let (scope, state_name, secure, _) = table in
    let (table, key) =
      get_table_key_ ~table Eliommod_datasess.find_data_cookie_only
    in
    Eliom_common.SessionCookies.remove table key;

    (* Now we want to close the session if it has not data inside
       and no group and no sub sessions *)
    close_volatile_state_if_empty ~scope ?state_name ~secure ()

  with Not_found | Eliom_common.Eliom_Session_expired -> ()






(*****************************************************************************)
(** Close a state *)
let discard_persistent_data ?state_name ?(scope = `Session) ?secure () = 
  match secure with
    | None ->
        Eliommod_persess.close_persistent_session ?state_name ~scope 
          ~secure:(Some true) () >>= fun () ->
        Eliommod_persess.close_persistent_session ?state_name ~scope 
          ~secure:(Some false) ()
    | _ ->
        Eliommod_persess.close_persistent_session ?state_name ~scope 
          ~secure ()

let discard_services ?state_name ?(scope = `Session) ?secure () = 
  match secure with
    | None ->
        Eliommod_sersess.close_service_session 
          ?state_name ~scope ~secure:(Some true) ();
        Eliommod_sersess.close_service_session 
          ?state_name ~scope ~secure:(Some false) ()
    | _ ->
        Eliommod_sersess.close_service_session ?state_name ~scope ~secure ()

let discard_volatile_data ?state_name ?(scope = `Session) ?secure () =
  match secure with
    | None ->
        Eliommod_datasess.close_data_session 
          ?state_name ~scope ~secure:(Some true) ();
        Eliommod_datasess.close_data_session 
          ?state_name ~scope ~secure:(Some false) ()
    | _ ->
        Eliommod_datasess.close_data_session ?state_name ~scope ~secure ()

let discard_data ?persistent ?state_name ?scope ?secure () =
  (match persistent with
    | None | Some false ->
      discard_volatile_data ?state_name ?scope ?secure ()
    | _ -> ());
  (match persistent with
    | None | Some true ->
      discard_persistent_data ?state_name ?scope ?secure ()
    | _ -> Lwt.return ())

let discard ?state_name ?(scope = `Session) ?secure () =
  discard_services ?state_name ~scope ?secure ();
  discard_data ?state_name ~scope ?secure ()

let close_session ?state_name ?secure () =
  discard ?state_name ?secure ()

let close_group ?state_name ?secure () =
  discard ?state_name ~scope:`Session_group ?secure ()

let discard_all_volatile_data ?state_name ?(cookie_scope = `Session) () =
  let sitedata = Eliom_request_info.find_sitedata "close_all_data_sessions" in
  Eliommod_sessadmin.close_all_data_sessions
    ?state_name ~cookie_scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all_persistent_data ?state_name ?(cookie_scope = `Session) () =
  let sitedata = Eliom_request_info.find_sitedata "close_all_persistent_sessions" in
  Eliommod_sessadmin.close_all_persistent_sessions
    ?state_name ~cookie_scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all_data ?persistent ?state_name ?cookie_scope () =
  (match persistent with
    | None | Some false ->
      discard_all_volatile_data ?state_name ?cookie_scope ()
    | _ -> Lwt.return ()) >>= fun () ->
  (match persistent with
    | None | Some true ->
      discard_all_persistent_data ?state_name ?cookie_scope ()
    | _ -> Lwt.return ())
(*VVV missing ~secure? *)

let discard_all_services
    ?state_name ?(cookie_scope = `Session) () =
  let sitedata = Eliom_request_info.find_sitedata "close_all_service_sessions" in
  Eliommod_sessadmin.close_all_service_sessions
    ?state_name ~cookie_scope sitedata
(*VVV missing: scope group *)
(*VVV missing ~secure? *)

let discard_all ?state_name ?cookie_scope () =
 discard_all_services ?state_name ?cookie_scope () >>=
 discard_all_data ?state_name ?cookie_scope
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
let get_service_cookie
    ?state_name ?(cookie_scope = `Session) ?secure () =
  try
    let c = Eliommod_sersess.find_service_cookie_only
      ?state_name ~cookie_scope ~secure () in
    Some c.Eliom_common.sc_value
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_volatile_data_cookie
    ?state_name ?(cookie_scope = `Session) ?secure () =
  try
    let c = Eliommod_datasess.find_data_cookie_only ?state_name ~cookie_scope ~secure () in
    Some c.Eliom_common.dc_value
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_persistent_data_cookie
    ?state_name ?(cookie_scope = `Session) ?secure () =
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only
        ?state_name ~cookie_scope ~secure () >>= fun c ->
      return (Some c.Eliom_common.pc_value)
    )
    (function
       | Not_found | Eliom_common.Eliom_Session_expired -> return None
       | e -> fail e)

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

(*****************************************************************************)
(* Client process info *)

let make_server_params sitedata i suffix fullsessname =
  let sp = Eliom_common.make_server_params_ sitedata i suffix fullsessname in
  Lwt.with_value Eliom_common.sp_key (Some sp)
    (fun () ->
      let get =
        sp.Eliom_common.sp_sitedata.Eliom_common.get_client_process_info
      in
      let cpi = lazy (match get () with
        | Some cpi -> cpi
        | None ->
          let cpi = 
            {Eliom_common.cpi_ssl = Eliom_request_info.get_ssl_sp sp;
             Eliom_common.cpi_hostname = Eliom_request_info.get_hostname_sp sp;
             Eliom_common.cpi_server_port =
                Eliom_request_info.get_server_port_sp sp;
             Eliom_common.cpi_original_full_path =
                Eliom_request_info.get_original_full_path_sp sp;
             Eliom_common.cpi_references = Polytables.create ()}
          in
          sp.Eliom_common.sp_sitedata.Eliom_common.set_client_process_info cpi;
          cpi)
      in
      sp.Eliom_common.sp_client_process_info <- cpi;
      Lwt.return sp)
