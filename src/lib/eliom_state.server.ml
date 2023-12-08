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

open Eliom_lib
open Lwt

(* Expired session? *)
type state_status = Alive_state | Empty_state | Expired_state

let service_state_status ~scope ?secure () =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  try
    ignore
      (Eliommod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure
         ());
    Alive_state
  with
  | Not_found -> Empty_state
  | Eliom_common.Eliom_Session_expired -> Expired_state

let volatile_data_state_status ~scope ?secure () =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  try
    ignore
      (Eliommod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ());
    Alive_state
  with
  | Not_found -> Empty_state
  | Eliom_common.Eliom_Session_expired -> Expired_state

let persistent_data_state_status ~scope ?secure () =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  catch
    (fun () ->
      Eliommod_persess.find_persistent_cookie_only ~cookie_scope
        ~secure_o:secure ()
      >>= fun _ -> return Alive_state)
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

let set_default_global_service_state_timeout ~cookie_level
    ?(override_configfile = false) timeout
  =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_service_timeout"
  in
  Eliommod_timeouts.set_default_global `Service cookie_level override_configfile
    false sitedata timeout

let set_global_service_state_timeout ~cookie_scope ?secure
    ?(recompute_expdates = false) ?(override_configfile = false) timeout
  =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_service_timeout"
  in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_timeouts.set_global ~kind:`Service ~cookie_scope ~secure
    ~recompute_expdates override_configfile sitedata timeout

let set_default_global_volatile_data_state_timeout ~cookie_level
    ?(override_configfile = false) timeout
  =
  let sitedata = Eliom_request_info.find_sitedata "set_global_data_timeout" in
  Eliommod_timeouts.set_default_global `Data cookie_level override_configfile
    false sitedata timeout

let set_global_volatile_data_state_timeout ~cookie_scope ?secure
    ?(recompute_expdates = false) ?(override_configfile = false) timeout
  =
  let sitedata = Eliom_request_info.find_sitedata "set_global_data_timeout" in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_timeouts.set_global ~kind:`Data ~cookie_scope ~secure
    ~recompute_expdates override_configfile sitedata timeout

let set_global_volatile_state_timeout ~cookie_scope ?secure
    ?(recompute_expdates = false) ?(override_configfile = false) timeout
  =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_volatile_timeouts"
  in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_timeouts.set_global ~kind:`Service ~cookie_scope ~secure
    ~recompute_expdates override_configfile sitedata timeout;
  Eliommod_timeouts.set_global ~kind:`Data ~cookie_scope ~secure
    ~recompute_expdates override_configfile sitedata timeout

let set_default_global_persistent_data_state_timeout ~cookie_level
    ?(override_configfile = false) timeout
  =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_persistent_timeout"
  in
  Eliommod_timeouts.set_default_global `Service cookie_level override_configfile
    false sitedata timeout

let set_global_persistent_data_state_timeout ~cookie_scope ?secure
    ?(recompute_expdates = false) ?(override_configfile = false) timeout
  =
  let sitedata =
    Eliom_request_info.find_sitedata "set_global_persistent_timeout"
  in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_timeouts.set_global ~kind:`Persistent ~cookie_scope ~secure
    ~recompute_expdates override_configfile sitedata timeout

let get_global_service_state_timeout ?secure ~cookie_scope () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_timeout" in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_timeouts.get_global ~kind:`Service ~cookie_scope ~secure sitedata

let get_global_volatile_data_state_timeout ?secure ~cookie_scope () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_timeout" in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_timeouts.get_global ~kind:`Data ~cookie_scope ~secure sitedata

let get_global_persistent_data_state_timeout ?secure ~cookie_scope () =
  let sitedata =
    Eliom_request_info.find_sitedata "get_global_persistent_timeout"
  in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_timeouts.get_global ~kind:`Persistent ~cookie_scope ~secure sitedata

(* Now for current session *)
let set_service_state_timeout ~cookie_scope ?secure t =
  let c =
    Eliommod_sersess.find_or_create_service_cookie ~cookie_scope
      ~secure_o:secure ()
  in
  let tor = c.Eliom_common.sc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t

let set_volatile_data_state_timeout ~cookie_scope ?secure t =
  let c =
    Eliommod_datasess.find_or_create_data_cookie ~cookie_scope ~secure_o:secure
      ()
  in
  let tor = c.Eliom_common.dc_timeout in
  match t with
  | None -> tor := Eliom_common.TNone
  | Some t -> tor := Eliom_common.TSome t

let unset_service_state_timeout ~cookie_scope ?secure () =
  try
    let c =
      Eliommod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure
        ()
    in
    let tor = c.Eliom_common.sc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()

let unset_volatile_data_state_timeout ~cookie_scope ?secure () =
  try
    let c =
      Eliommod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ()
    in
    let tor = c.Eliom_common.dc_timeout in
    tor := Eliom_common.TGlobal
  with Not_found | Eliom_common.Eliom_Session_expired -> ()

let get_service_state_timeout ~cookie_scope ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  try
    let c =
      Eliommod_sersess.find_service_cookie_only ~cookie_scope
        ~secure_o:(Some secure) ~sp ()
    in
    let tor = c.Eliom_common.sc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global ~kind:`Service ~cookie_scope ~secure
          sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global ~kind:`Service ~cookie_scope ~secure sitedata

let get_volatile_data_state_timeout ~cookie_scope ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  try
    let c =
      Eliommod_datasess.find_data_cookie_only ~cookie_scope
        ~secure_o:(Some secure) ~sp ()
    in
    let tor = c.Eliom_common.dc_timeout in
    match !tor with
    | Eliom_common.TGlobal ->
        Eliommod_timeouts.get_global ~kind:`Data ~cookie_scope ~secure sitedata
    | Eliom_common.TNone -> None
    | Eliom_common.TSome t -> Some t
  with Not_found | Eliom_common.Eliom_Session_expired ->
    Eliommod_timeouts.get_global ~kind:`Data ~cookie_scope ~secure sitedata

let set_persistent_data_state_timeout ~cookie_scope ?secure t =
  let%lwt c =
    Eliommod_persess.find_or_create_persistent_cookie ~cookie_scope
      ~secure_o:secure ()
  in
  let tor = c.Eliom_common.pc_timeout in
  return
    (match t with
    | None -> tor := Eliom_common.TNone
    | Some t -> tor := Eliom_common.TSome t)

let unset_persistent_data_state_timeout ~cookie_scope ?secure () =
  try%lwt
    let%lwt c =
      Eliommod_persess.find_persistent_cookie_only ~cookie_scope
        ~secure_o:secure ()
    in
    let tor = c.Eliom_common.pc_timeout in
    tor := Eliom_common.TGlobal;
    return_unit
  with Not_found | Eliom_common.Eliom_Session_expired -> return_unit

let get_persistent_data_state_timeout ~cookie_scope ?secure () =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  try%lwt
    let%lwt c =
      Eliommod_persess.find_persistent_cookie_only ~cookie_scope
        ~secure_o:(Some secure) ~sp ()
    in
    let tor = c.Eliom_common.pc_timeout in
    return
      (match !tor with
      | Eliom_common.TGlobal ->
          Eliommod_timeouts.get_global ~kind:`Persistent ~cookie_scope ~secure
            sitedata
      | Eliom_common.TNone -> None
      | Eliom_common.TSome t -> Some t)
  with Not_found | Eliom_common.Eliom_Session_expired ->
    return
      (Eliommod_timeouts.get_global ~kind:`Persistent ~cookie_scope ~secure
         sitedata)

(* Preventing memory leaks: we must close empty sessions *)

let rec close_service_state_if_empty ~scope ?secure () =
  (* Close the session if it has not services inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
    let c =
      Eliommod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure
        ~sp ()
    in
    match scope with
    | `Session _ ->
        (*VVV ???        (match !(c.Eliom_common.sc_session_group) with
          | (_, _, Right _) (* no group *)
              when *)
        if Eliommod_sessiongroups.Data.group_size
             ( sitedata.Eliom_common.site_dir_string
             , `Client_process
             , Left Eliom_common.(Hashed_cookies.to_string c.sc_hvalue) )
           = 0
           (* no tab sessions *)
           && Eliom_common.service_tables_are_empty !(c.Eliom_common.sc_table)
        then
          Eliommod_sessiongroups.Data.remove
            c.Eliom_common.sc_session_group_node
    | `Client_process _ ->
        if Eliom_common.service_tables_are_empty !(c.Eliom_common.sc_table)
        then
          Eliommod_sessiongroups.Data.remove
            c.Eliom_common.sc_session_group_node
    | `Session_group scope_hierarchy ->
        (* There is a browser session, we do not close the group,
           but we may close the browser session (this will close
           the group if it is empty). *)
        close_service_state_if_empty ~scope:(`Session scope_hierarchy) ?secure
          ()
  with Not_found -> ()

let rec close_volatile_state_if_empty ~scope ?secure () =
  (* Close the session if it has not data inside
     and no group and no sub sessions *)
  (* See also in Eliommod_gc and in Eliommod_sessiongroups. *)
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
    let c =
      Eliommod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ~sp
        ()
    in
    match scope with
    | `Session _ -> (
      match !(c.Eliom_common.dc_session_group) with
      | _, _, Right _
      (* no group *)
        when Eliommod_sessiongroups.Data.group_size
               ( sitedata.Eliom_common.site_dir_string
               , `Client_process
               , Left Eliom_common.(Hashed_cookies.to_string c.dc_hvalue) )
             = 0
             (* no tab sessions *)
             && sitedata.Eliom_common.not_bound_in_data_tables
                  Eliom_common.(Hashed_cookies.to_string c.dc_hvalue) ->
          Eliommod_sessiongroups.Data.remove
            c.Eliom_common.dc_session_group_node
      | _ -> ())
    | `Client_process _ -> ()
    (* This should never occur, because we always have tab session data
   when we have a tab session (at least the change_page_event).
        if (sitedata.Eliom_common.not_bound_in_data_tables
              c.Eliom_common.dc_hvalue)
        then Eliommod_sessiongroups.Data.remove
          c.Eliom_common.dc_session_group_node *)
    | `Session_group scope_hierarchy ->
        (* There is a browser session, we do not close the group,
           but we may close the browser session (this will close
           the group if it is empty). *)
        close_volatile_state_if_empty ~scope:(`Session scope_hierarchy) ?secure
          ()
  with Not_found -> ()

let close_persistent_state_if_empty ~scope:_ ?secure:_ () = Lwt.return_unit
(*VVV Can we implement this function? *)

(* session groups *)

type 'a state_data = No_data | Data_session_expired | Data of 'a

let set_service_session_group ?set_max
    ?(scope = Eliom_common.default_session_scope) ?secure session_group
  =
  let c =
    Eliommod_sersess.find_or_create_service_cookie
      ~set_session_group:session_group
      ~cookie_scope:(scope :> Eliom_common.cookie_scope)
      ~secure_o:secure ()
  in
  match set_max with
  | None -> ()
  | Some m ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.sc_session_group_node m

let unset_service_session_group ?set_max
    ?(scope = Eliom_common.default_session_scope) ?secure ()
  =
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c =
      Eliommod_sersess.find_service_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ~sp ()
    in
    let n =
      Eliommod_sessiongroups.make_full_group_name ~cookie_level:`Session
        (Eliom_request_info.get_request_sp sp).Ocsigen_extensions.request_info
        sitedata.Eliom_common.site_dir_string
        (Eliom_common.get_mask4 sitedata)
        (Eliom_common.get_mask6 sitedata)
        None
    in
    let node =
      Eliommod_sessiongroups.Serv.move ?set_max sitedata
        c.Eliom_common.sc_session_group_node n
    in
    c.Eliom_common.sc_session_group_node <- node;
    c.Eliom_common.sc_session_group := n;
    (* Now we want to close the session if it has not data inside
       and no tab sessions *)
    close_service_state_if_empty
      ~scope:(scope :> Eliom_common.user_scope)
      ?secure ()
  with Not_found | Eliom_common.Eliom_Session_expired -> ()

let get_service_session_group ?(scope = Eliom_common.default_session_scope)
    ?secure ()
  =
  try
    let c =
      Eliommod_sersess.find_service_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Eliom_common.sc_session_group) with
    | _, _, Right _ -> None
    | _, _, Left v -> Some v
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_service_session_group_size ?(scope = Eliom_common.default_session_scope)
    ?secure ()
  =
  try
    let c =
      Eliommod_sersess.find_service_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Eliom_common.sc_session_group) with
    | _, _, Right _ -> None
    | _, _, Left _ ->
        Some
          (Eliommod_sessiongroups.Serv.group_size
             !(c.Eliom_common.sc_session_group))
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let set_volatile_data_session_group ?set_max
    ?(scope = Eliom_common.default_session_scope) ?secure session_group
  =
  let c =
    Eliommod_datasess.find_or_create_data_cookie
      ~set_session_group:session_group
      ~cookie_scope:(scope :> Eliom_common.cookie_scope)
      ~secure_o:secure ()
  in
  match set_max with
  | None -> ()
  | Some m ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.dc_session_group_node m

let unset_volatile_data_session_group ?set_max
    ?(scope = Eliom_common.default_session_scope) ?secure ()
  =
  try
    let sp = Eliom_common.get_sp () in
    let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
    let c =
      Eliommod_datasess.find_data_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ~sp ()
    in
    let n =
      Eliommod_sessiongroups.make_full_group_name ~cookie_level:`Session
        (Eliom_request_info.get_request_sp sp).Ocsigen_extensions.request_info
        sitedata.Eliom_common.site_dir_string
        (Eliom_common.get_mask4 sitedata)
        (Eliom_common.get_mask6 sitedata)
        None
    in
    let node =
      Eliommod_sessiongroups.Data.move ?set_max sitedata
        c.Eliom_common.dc_session_group_node n
    in
    c.Eliom_common.dc_session_group_node <- node;
    c.Eliom_common.dc_session_group := n;
    (* Now we want to close the session if it has not data inside
       and no tab sessions *)
    close_volatile_state_if_empty
      ~scope:(scope :> Eliom_common.user_scope)
      ?secure ()
  with Not_found | Eliom_common.Eliom_Session_expired -> ()

let get_volatile_data_session_group
    ?(scope = Eliom_common.default_session_scope) ?secure ()
  =
  try
    let c =
      Eliommod_datasess.find_data_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Eliom_common.dc_session_group) with
    | _, _, Right _ -> None
    | _, _, Left v -> Some v
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_volatile_data_session_group_size
    ?(scope = Eliom_common.default_session_scope) ?secure ()
  =
  try
    let c =
      Eliommod_datasess.find_data_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Eliom_common.dc_session_group) with
    | _, _, Right _ -> None
    | _, _, Left _ ->
        Some
          (Eliommod_sessiongroups.Data.group_size
             !(c.Eliom_common.dc_session_group))
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let set_persistent_data_session_group ?set_max
    ?(scope = Eliom_common.default_session_scope) ?secure n
  =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  let%lwt c =
    Eliommod_persess.find_or_create_persistent_cookie
      ~cookie_scope:(scope :> Eliom_common.cookie_scope)
      ~secure_o:secure ~sp ()
  in
  let n =
    Eliommod_sessiongroups.make_persistent_full_group_name
      ~cookie_level:`Session sitedata.Eliom_common.site_dir_string (Some n)
  in
  let grp = c.Eliom_common.pc_session_group in
  let%lwt l =
    Eliommod_sessiongroups.Pers.move sitedata ?set_max
      (fst sitedata.Eliom_common.max_persistent_data_sessions_per_group)
      Eliom_common.(Hashed_cookies.to_string c.pc_hvalue)
      !grp n
  in
  let%lwt () =
    Lwt_list.iter_p
      (Eliommod_persess.close_persistent_state2
         ~scope:(scope :> Eliom_common.user_scope)
         sitedata None)
      l
  in
  grp := n;
  Lwt.return_unit

let unset_persistent_data_session_group
    ?(scope = Eliom_common.default_session_scope) ?secure ()
  =
  let sp = Eliom_common.get_sp () in
  let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
  try%lwt
    let%lwt c =
      Eliommod_persess.find_persistent_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ~sp ()
    in
    let grp = c.Eliom_common.pc_session_group in
    let%lwt () =
      Eliommod_sessiongroups.Pers.remove sitedata
        Eliom_common.(Hashed_cookies.to_string c.pc_hvalue)
        !grp
    in
    grp := None;
    close_persistent_state_if_empty
      ~scope:(scope :> Eliom_common.user_scope)
      ?secure ()
  with Not_found | Eliom_common.Eliom_Session_expired -> Lwt.return_unit

let get_persistent_data_session_group
    ?(scope = Eliom_common.default_session_scope) ?secure ()
  =
  try%lwt
    let%lwt c =
      Eliommod_persess.find_persistent_cookie_only
        ~cookie_scope:(scope :> Eliom_common.cookie_scope)
        ~secure_o:secure ()
    in
    Lwt.return
      (match !(c.Eliom_common.pc_session_group) with
      | None -> None
      | Some v -> (
        match Eliommod_sessiongroups.getperssessgrp v with
        | _, _, Left s -> Some s
        | _ -> None))
  with Not_found | Eliom_common.Eliom_Session_expired -> Lwt.return_none

(* max *)
let set_default_max_service_sessions_per_group ?(override_configfile = false) n =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_service_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_service_sessions_per_group in
  if override_configfile || not b
  then sitedata.Eliom_common.max_service_sessions_per_group <- n, b

let set_default_max_volatile_data_sessions_per_group
    ?(override_configfile = false) n
  =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_volatile_data_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_sessions_per_group in
  if override_configfile || not b
  then sitedata.Eliom_common.max_volatile_data_sessions_per_group <- n, b

let set_default_max_persistent_data_sessions_per_group
    ?(override_configfile = false) n
  =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_persistent_data_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_persistent_data_sessions_per_group in
  if override_configfile || not b
  then sitedata.Eliom_common.max_persistent_data_sessions_per_group <- n, b

let set_default_max_service_sessions_per_subnet ?(override_configfile = false) n
  =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_service_sessions_per_subnet"
  in
  let b = snd sitedata.Eliom_common.max_service_sessions_per_subnet in
  if override_configfile || not b
  then sitedata.Eliom_common.max_service_sessions_per_subnet <- n, b

let set_default_max_volatile_data_sessions_per_subnet
    ?(override_configfile = false) n
  =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_volatile_data_sessions_per_subnet"
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_sessions_per_subnet in
  if override_configfile || not b
  then sitedata.Eliom_common.max_volatile_data_sessions_per_subnet <- n, b

let set_default_max_volatile_sessions_per_group ?override_configfile n =
  set_default_max_service_sessions_per_group ?override_configfile n;
  set_default_max_volatile_data_sessions_per_group ?override_configfile n

let set_default_max_volatile_sessions_per_subnet ?override_configfile n =
  set_default_max_service_sessions_per_subnet ?override_configfile n;
  set_default_max_volatile_data_sessions_per_subnet ?override_configfile n

let set_default_max_service_tab_sessions_per_group
    ?(override_configfile = false) n
  =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_service_tab_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_service_tab_sessions_per_group in
  if override_configfile || not b
  then sitedata.Eliom_common.max_service_tab_sessions_per_group <- n, b

let set_default_max_volatile_data_tab_sessions_per_group
    ?(override_configfile = false) n
  =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_volatile_data_tab_sessions_per_group"
  in
  let b = snd sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group in
  if override_configfile || not b
  then sitedata.Eliom_common.max_volatile_data_tab_sessions_per_group <- n, b

let set_default_max_persistent_data_tab_sessions_per_group
    ?(override_configfile = false) n
  =
  let sitedata =
    Eliom_request_info.find_sitedata
      "set_default_max_persistent_data_tab_sessions_per_group"
  in
  let b =
    snd sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group
  in
  if override_configfile || not b
  then sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group <- n, b

let set_default_max_volatile_tab_sessions_per_group ?override_configfile n =
  set_default_max_service_tab_sessions_per_group ?override_configfile n;
  set_default_max_volatile_data_tab_sessions_per_group ?override_configfile n

let set_ipv4_subnet_mask ?(override_configfile = false) n =
  let sitedata = Eliom_request_info.find_sitedata "set_ipv4_subnet_mask" in
  let b = snd sitedata.Eliom_common.ipv4mask in
  if override_configfile || not b
  then sitedata.Eliom_common.ipv4mask <- Some n, b

let set_ipv6_subnet_mask ?(override_configfile = false) n =
  let sitedata = Eliom_request_info.find_sitedata "set_ipv6_subnet_mask" in
  let b = snd sitedata.Eliom_common.ipv6mask in
  if override_configfile || not b
  then sitedata.Eliom_common.ipv6mask <- Some n, b

let set_max_service_states_for_group_or_subnet ~scope ?secure m =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let c =
    Eliommod_sersess.find_or_create_service_cookie ~secure_o:secure
      ~cookie_scope ()
  in
  match scope with
  | `Session_group _ -> (
    match
      Eliommod_sessiongroups.Data.find_node_in_group_of_groups
        !(c.Eliom_common.sc_session_group)
    with
    | Some node -> Eliommod_sessiongroups.Data.set_max node m
    | _ -> ())
  | _ ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.sc_session_group_node m

let set_max_volatile_data_states_for_group_or_subnet ~scope ?secure m =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let c =
    Eliommod_datasess.find_or_create_data_cookie ~cookie_scope ~secure_o:secure
      ()
  in
  match scope with
  | `Session_group _ -> (
    match
      Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
        !(c.Eliom_common.dc_session_group)
    with
    | Some (_, node) -> Eliommod_sessiongroups.Data.set_max node m
    | _ -> ())
  | _ ->
      Eliommod_sessiongroups.Data.set_max c.Eliom_common.dc_session_group_node m

let set_max_volatile_states_for_group_or_subnet ~scope ?secure m =
  set_max_service_states_for_group_or_subnet ~scope ?secure m;
  set_max_volatile_data_states_for_group_or_subnet ~scope ?secure m

(*VVV No version for persistent sessions? Why? *)

(* expiration dates *)
let set_service_cookie_exp_date ~cookie_scope ?secure t =
  let c =
    Eliommod_sersess.find_or_create_service_cookie ~cookie_scope
      ~secure_o:secure ()
  in
  let exp = c.Eliom_common.sc_cookie_exp in
  match t with
  | None -> exp := Eliom_common.CEBrowser
  | Some t -> exp := Eliom_common.CESome t

(*
let get_service_cookie_exp_date ?state_name ?(cookie_level = `Session) ?secure () =
  try
    let (_, _, _, _, exp) = find_service_cookie_only ?state_name ~cookie_level ~secure () in
  let exp = c.Eliom_common.sc_cookie_exp in
    !exp
  with Not_found | Eliom_common.Eliom_Session_expired -> Eliom_common.CEBrowser
*)

let set_volatile_data_cookie_exp_date ~cookie_scope ?secure t =
  let c =
    Eliommod_datasess.find_or_create_data_cookie ~cookie_scope ~secure_o:secure
      ()
  in
  let exp = c.Eliom_common.dc_cookie_exp in
  match t with
  | None -> exp := Eliom_common.CEBrowser
  | Some t -> exp := Eliom_common.CESome t

let set_persistent_data_cookie_exp_date ~cookie_scope ?secure t =
  let%lwt c =
    Eliommod_persess.find_or_create_persistent_cookie ~cookie_scope
      ~secure_o:secure ()
  in
  let exp = c.Eliom_common.pc_cookie_exp in
  return
    (match t with
    | None -> exp := Eliom_common.CEBrowser
    | Some t -> exp := Eliom_common.CESome t)

(* *)
let get_global_table () =
  let sitedata = Eliom_request_info.find_sitedata "get_global_table" in
  sitedata.Eliom_common.global_services

(** If the session does not exist, we create it
   (new cookie, new session service table) *)
let get_session_service_table ~sp ~scope ?secure () =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let c =
    Eliommod_sersess.find_or_create_service_cookie ~cookie_scope
      ~secure_o:secure ~sp ()
  in
  match scope with
  | `Session_group _ -> (
    match
      Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
        !(c.Eliom_common.sc_session_group)
    with
    | None -> raise Not_found
    | Some (t, _) -> t)
  | _ -> c.Eliom_common.sc_table

(** If the session does not exist, we raise Not_found *)
let get_session_service_table_if_exists ~sp ~scope ?secure () =
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  try
    let c =
      Eliommod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure
        ~sp ()
    in
    match scope with
    | `Session_group _ -> (
      match
        Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
          !(c.Eliom_common.sc_session_group)
      with
      | None -> raise Not_found
      | Some (t, _) -> t)
    | _ -> c.Eliom_common.sc_table
  with Eliom_common.Eliom_Session_expired -> raise Not_found

(*****************************************************************************)
(** {2 persistent sessions} *)

module Ocsipersist = Eliom_common.Ocsipersist.Polymorphic

type 'a persistent_table =
  Eliom_common.user_scope * bool * (int64 * 'a) Ocsipersist.table

let create_persistent_table ~scope ?secure name : 'a persistent_table Lwt.t =
  let sitedata = Eliom_request_info.find_sitedata "create_persistent_table" in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliom_common.Persistent_tables.create name >>= fun t ->
  Lwt.return (scope, secure, t)

let get_p_table_key_ ~table:(scope, secure, table)
    (find_cookie :
      cookie_scope:Eliom_common.cookie_scope
      -> secure_o:bool option
      -> ?sp:Eliom_common.server_params
      -> unit
      -> Eliom_common.one_persistent_cookie_info Lwt.t)
  =
  let get_cookie () =
    let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
    let%lwt c = find_cookie ~cookie_scope ~secure_o:(Some secure) () in
    Lwt.return Eliom_common.(Hashed_cookies.to_string c.pc_hvalue)
  in
  let%lwt key =
    match scope with
    | `Session_group state_name -> (
        match%lwt
          get_persistent_data_session_group ~scope:(`Session state_name) ~secure
            ()
        with
        | Some a -> Lwt.return a
        | None ->
            (* No session group. We use the session cookie as key. *)
            get_cookie ())
    | _ -> get_cookie ()
  in
  Lwt.return (table, key)

let get_persistent_data ~table () =
  catch
    (fun () ->
      get_p_table_key_ ~table Eliommod_persess.find_persistent_cookie_only
      >>= fun (table, key) ->
      Ocsipersist.find table key >>= fun (_, v) -> Lwt.return (Data v))
    (function
      | Eliom_common.Eliom_Session_expired -> return Data_session_expired
      | Not_found -> return No_data
      | e -> fail e)

let set_persistent_data ~table value =
  let f__ ~cookie_scope ~secure_o ?sp () =
    Eliommod_persess.find_or_create_persistent_cookie ~cookie_scope ~secure_o
      ?sp ()
  in
  get_p_table_key_ ~table f__ >>= fun (table, key) ->
  Ocsipersist.add table key (Int64.zero, value)

let remove_persistent_data ~table () =
  try%lwt
    let scope, secure, _ = table in
    let%lwt table, key =
      get_p_table_key_ ~table Eliommod_persess.find_persistent_cookie_only
    in
    let%lwt () = Ocsipersist.remove table key in
    close_persistent_state_if_empty ~scope ~secure ()
  with Not_found | Eliom_common.Eliom_Session_expired -> return_unit

(*****************************************************************************)
(** {2 session data in memory} *)

type 'a volatile_table =
  Eliom_common.user_scope * bool * 'a Eliom_common.SessionCookies.t

let create_volatile_table_during_session_ =
  Eliommod_datasess.create_volatile_table_during_session

let create_volatile_table ~scope ?secure () =
  match Eliom_common.get_sp_option () with
  | None -> (
    match Eliom_common.global_register_allowed () with
    | Some get_current_sitedata ->
        let sitedata = get_current_sitedata () in
        let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
        Eliommod_datasess.create_volatile_table ~scope ~secure
    | None ->
        raise
          (Eliom_common.Eliom_site_information_not_available
             "create_volatile_table"))
  | Some sp ->
      let sitedata = Eliom_request_info.get_sitedata_sp ~sp in
      let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
      create_volatile_table_during_session_ ~scope ~secure sitedata

let get_table_key_ ~table:(scope, secure, table)
    (find_cookie :
      cookie_scope:Eliom_common.cookie_scope
      -> secure_o:bool option
      -> ?sp:Eliom_common.server_params
      -> unit
      -> Eliom_common.one_data_cookie_info)
  =
  (* The key in the table is the cookie for client processes and sessions,
     and the group name for groups *)
  let get_cookie () =
    let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
    let c = find_cookie ~cookie_scope ~secure_o:(Some secure) () in
    Eliom_common.(Hashed_cookies.to_string c.dc_hvalue)
  in
  ( table
  , match scope with
    | `Session_group state_name -> (
      match
        get_volatile_data_session_group ~scope:(`Session state_name) ~secure ()
      with
      | Some a -> a
      | None ->
          (* No session group has been set. We use the session instead. *)
          get_cookie ())
    | _ -> get_cookie () )

let get_volatile_data ~table () =
  try
    let table, key =
      get_table_key_ ~table Eliommod_datasess.find_data_cookie_only
    in
    Data (Eliom_common.SessionCookies.find table key)
  with
  | Not_found -> No_data
  | Eliom_common.Eliom_Session_expired -> Data_session_expired

let set_volatile_data ~table value =
  let f__ ~cookie_scope ~secure_o ?sp () =
    Eliommod_datasess.find_or_create_data_cookie ~cookie_scope ~secure_o ?sp ()
  in
  let table, key = get_table_key_ ~table f__ in
  Eliom_common.SessionCookies.replace table key value

let remove_volatile_data ~table () =
  try
    let scope, secure, _ = table in
    let table, key =
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
      Eliommod_persess.close_persistent_state ~scope ~secure_o:(Some true) ()
      >>= fun () ->
      Eliommod_persess.close_persistent_state ~scope ~secure_o:(Some false) ()
  | _ -> Eliommod_persess.close_persistent_state ~scope ~secure_o:secure ()

let discard_services ~scope ?secure () =
  match secure with
  | None ->
      Eliommod_sersess.close_service_state ~scope ~secure_o:(Some true) ();
      Eliommod_sersess.close_service_state ~scope ~secure_o:(Some false) ()
  | _ -> Eliommod_sersess.close_service_state ~scope ~secure_o:secure ()

let discard_volatile_data ~scope ?secure () =
  match secure with
  | None ->
      Eliommod_datasess.close_data_state ~scope ~secure_o:(Some true) ();
      Eliommod_datasess.close_data_state ~scope ~secure_o:(Some false) ()
  | _ -> Eliommod_datasess.close_data_state ~scope ~secure_o:secure ()

let discard_request_data () =
  let table = Eliom_request_info.get_request_cache () in
  Polytables.clear ~table; Lwt.return_unit

let discard_data ?persistent ~scope ?secure () =
  match scope with
  | #Eliom_common.request_scope -> discard_request_data ()
  | #Eliom_common.user_scope as scope -> (
      (match persistent with
      | None | Some false -> discard_volatile_data ~scope ?secure ()
      | _ -> ());
      match persistent with
      | None | Some true -> discard_persistent_data ~scope ?secure ()
      | _ -> Lwt.return_unit)

let discard ~scope ?secure () =
  match scope with
  | #Eliom_common.request_scope -> discard_request_data ()
  | #Eliom_common.user_scope as scope ->
      discard_services ~scope:(scope :> [< Eliom_common.user_scope]) ?secure ();
      discard_data ~scope:(scope :> [< Eliom_common.user_scope]) ?secure ()
(* will close volatile and persistent sessions for one scope *)

let discard_all_scopes ?secure () =
  let discard_name scope_hierarchy =
    let%lwt () = discard ?secure ~scope:(`Session_group scope_hierarchy) () in
    let%lwt () = discard ?secure ~scope:(`Session scope_hierarchy) () in
    discard ?secure ~scope:(`Client_process scope_hierarchy) ()
  in
  let%lwt () = discard_request_data () in
  Lwt_list.iter_p discard_name (Eliom_common.list_scope_hierarchies ())

let discard_all_volatile_data ~scope ?secure () =
  let sitedata = Eliom_request_info.find_sitedata "discard_all_volatile_data" in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_sessadmin.close_all_data_states ~scope ~secure sitedata
(*VVV missing: scope group *)

let discard_all_persistent_data ~scope ?secure () =
  let sitedata =
    Eliom_request_info.find_sitedata "discard_all_persistent_data"
  in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_sessadmin.close_all_persistent_states ~scope ~secure sitedata
(*VVV missing: scope group *)

let discard_all_data ?persistent ~scope ?secure () =
  let%lwt () =
    match persistent with
    | None | Some false -> discard_all_volatile_data ~scope ?secure ()
    | _ -> Lwt.return_unit
  in
  match persistent with
  | None | Some true -> discard_all_persistent_data ~scope ?secure ()
  | _ -> Lwt.return_unit

let discard_all_services ~scope ?secure () =
  let sitedata =
    Eliom_request_info.find_sitedata "close_all_service_sessions"
  in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  Eliommod_sessadmin.close_all_service_states ~scope ~secure sitedata
(*VVV missing: scope group *)

let discard_all ~scope ?secure () =
  let%lwt () = discard_all_services ~scope ?secure () in
  discard_all_data ~scope ?secure ()

let discard_everything () =
  let discard_name scope_hierarchy =
    let%lwt () = discard_all ~scope:(`Session_group scope_hierarchy) () in
    let%lwt () = discard_all ~scope:(`Session scope_hierarchy) () in
    discard_all ~scope:(`Client_process scope_hierarchy) ()
  in
  let%lwt () = discard_request_data () in
  Lwt_list.iter_p discard_name (Eliom_common.list_scope_hierarchies ())

(*****************************************************************************)
(* Administration *)

module Ext = struct
  (** Type used to describe session timeouts *)

  type timeout = Eliom_common.timeout =
    | TGlobal  (** see global setting *)
    | TNone  (** explicitly set no timeout *)
    | TSome of float  (** timeout duration in seconds *)

  type (+'a (* scope *), +'b (* `Data, `Service or `Pers *)) state =
    Eliom_common.user_scope * [`Data | `Service | `Pers] * string

  type service_cookie_info =
    string (* cookie value *)
    * Eliom_common.tables Eliom_common.Service_cookie.t

  type data_cookie_info = string (* cookie value *) * Eliom_common.Data_cookie.t

  type persistent_cookie_info =
    string (* cookie value *) * Eliommod_cookies.cookie

  let untype_state state = state

  (*VVV Do we need this? + check

  (* The following function returns the group to which belongs
     a session or client process state: *)
  let group_of ~state:(_cookie, (_, _, _, _, sgr, _sgrnode)) =
    match Eliommod_sessiongroups.Serv.find_node_in_group_of_groups !sgr with
      | Some a -> a
      | None -> (* the group of a tab session,
                   that is, the browser session associated. *)
        Eliommod_sessiongroups.make_full_named_group_name_
          ~cookie_level:`Client_process sitedata cookie
        (*VVV à vérifier *)
*)

  let volatile_data_group_state ?(scope = Eliom_common.default_group_scope)
      group_name
    =
    (scope :> Eliom_common.user_scope), `Data, group_name

  let persistent_data_group_state ?(scope = Eliom_common.default_group_scope)
      group_name
    =
    (scope :> Eliom_common.user_scope), `Pers, group_name

  let service_group_state ?(scope = Eliom_common.default_group_scope) group_name
    =
    (scope :> Eliom_common.user_scope), `Service, group_name

  let current_volatile_data_state ?secure
      ?(scope = (Eliom_common.default_session_scope :> Eliom_common.user_scope))
      ()
    =
    let scope = (scope :> Eliom_common.user_scope) in
    match scope with
    | `Session_group h -> (
      match get_volatile_data_session_group ~scope:(`Session h) ?secure () with
      | Some g -> volatile_data_group_state ~scope:(`Session_group h) g
      | None -> raise Not_found)
    | #Eliom_common.cookie_scope as cookie_scope ->
        let cookie =
          Eliommod_datasess.find_or_create_data_cookie ~secure_o:secure
            ~cookie_scope ()
        in
        (scope, `Data, Eliom_common.(Hashed_cookies.to_string cookie.dc_hvalue)
          : ('a, 'b) state)

  let current_persistent_data_state ?secure
      ?(scope = (Eliom_common.default_session_scope :> Eliom_common.user_scope))
      ()
    =
    let scope = (scope :> Eliom_common.user_scope) in
    match scope with
    | `Session_group h -> (
        match%lwt
          get_persistent_data_session_group ~scope:(`Session h) ?secure ()
        with
        | Some g ->
            persistent_data_group_state ~scope:(`Session_group h) g
            |> Lwt.return
        | None -> Lwt.fail Not_found)
    | #Eliom_common.cookie_scope as cookie_scope ->
        Eliommod_persess.find_or_create_persistent_cookie ~secure_o:secure
          ~cookie_scope ()
        >>= fun cookie ->
        Lwt.return
          ( scope
          , `Pers
          , Eliom_common.(Hashed_cookies.to_string cookie.pc_hvalue) )

  let current_service_state ?secure
      ?(scope = (Eliom_common.default_session_scope :> Eliom_common.user_scope))
      ()
    =
    let scope = (scope :> Eliom_common.user_scope) in
    match scope with
    | `Session_group h -> (
      match get_service_session_group ~scope:(`Session h) ?secure () with
      | Some g -> service_group_state ~scope:(`Session_group h) g
      | None -> raise Not_found)
    | #Eliom_common.cookie_scope as cookie_scope ->
        let cookie =
          Eliommod_sersess.find_or_create_service_cookie ~secure_o:secure
            ~cookie_scope ()
        in
        ( scope
        , `Service
        , Eliom_common.(Hashed_cookies.to_string cookie.sc_hvalue) )

  let get_service_cookie_info
      ?(sitedata =
        Eliom_request_info.find_sitedata "Eliom_state.get_service_cookie_info")
      ((_, _, cookie) : ([< Eliom_common.cookie_level], [`Service]) state)
    =
    ( cookie
    , Eliom_common.SessionCookies.find sitedata.Eliom_common.session_services
        cookie )

  let get_volatile_data_cookie_info
      ?(sitedata =
        Eliom_request_info.find_sitedata
          "Eliom_state.get_volatile_data_cookie_info")
      ((_, _, cookie) : ([< Eliom_common.cookie_level], [`Data]) state)
    =
    ( cookie
    , Eliom_common.SessionCookies.find sitedata.Eliom_common.session_data cookie
    )

  let get_persistent_cookie_info
      ((_, _, cookie) : ([< Eliom_common.cookie_level], [`Pers]) state)
    =
    Eliommod_cookies.Persistent_cookies.Cookies.find cookie >>= fun v ->
    Lwt.return (cookie, v)

  let discard_state
      ?(sitedata = Eliom_request_info.find_sitedata "Eliom_state.discard_state")
      ~state ()
    =
    let make_sessgrp n =
      sitedata.Eliom_common.site_dir_string, `Session, Left n
    in
    match state with
    | `Session_group _, `Data, group_name ->
        (match
           Eliommod_sessiongroups.Data.find_node_in_group_of_groups
             (make_sessgrp group_name)
         with
        | Some node -> Eliommod_sessiongroups.Data.remove node
        | None -> ());
        Lwt.return_unit
    | `Session_group _, `Service, group_name ->
        (match
           Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
             (make_sessgrp group_name)
         with
        | Some (_, node) -> Eliommod_sessiongroups.Serv.remove node
        | None -> ());
        Lwt.return_unit
    | `Session_group _, `Pers, group_name ->
        let sgr_o =
          Eliom_common.make_persistent_full_group_name ~cookie_level:`Session
            sitedata.Eliom_common.site_dir_string (Some group_name)
        in
        Eliommod_sessiongroups.Pers.remove_group ~cookie_level:`Session sitedata
          sgr_o
    | _, `Service, (_cookie : string) ->
        let () =
          match get_service_cookie_info ~sitedata state with
          | exception Not_found -> ()
          | _, {Eliom_common.Service_cookie.session_group_node; _} ->
              Eliommod_sessiongroups.Serv.remove session_group_node
        in
        Lwt.return_unit
    | _, `Data, _cookie ->
        let () =
          match get_volatile_data_cookie_info ~sitedata state with
          | exception Not_found -> ()
          | _, {Eliom_common.Data_cookie.session_group_node; _} ->
              Eliommod_sessiongroups.Data.remove session_group_node
        in
        Lwt.return_unit
    | _, `Pers, _cookie -> (
        match%lwt get_persistent_cookie_info state with
        | exception Not_found -> Lwt.return_unit
        | cookie, {Eliommod_cookies.full_state_name; session_group; _} ->
            let scope = full_state_name.Eliom_common.user_scope in
            let cookie_level = Eliom_common.cookie_level_of_user_scope scope in
            Eliommod_sessiongroups.Pers.close_persistent_session2 ~cookie_level
              sitedata session_group cookie)
  (*VVV!!! est-ce que session_group est fullsessgrp ? *)

  let fold_sub_states_aux_aux
      ?(sitedata =
        Eliom_request_info.find_sitedata "Eliom_state (state iterator)")
      ~state:
        ((s, k, id) :
          ([< `Session_group | `Session], [< `Pers | `Data | `Service]) state) f
    =
    (* id is the session cookie value or the group name *)
    let reduce_scope = function
      | `Session_group n -> `Session n
      | `Session n -> `Client_process n
      | `Client_process _ -> failwith "fold_sub_states"
    in
    let reduce_level = function
      | `Session_group _ -> `Session
      | `Session _ -> `Client_process
      | `Client_process _ -> failwith "fold_sub_states"
    in
    let sub_states_level = reduce_level s in
    let sub_states_scope = reduce_scope s in
    let f a v = f a (sub_states_scope, k, v) in
    sitedata, sub_states_level, id, f

  let fold_sub_states_aux fold return (sitedata, sub_states_level, id, f) e
    = function
    | _, `Data, _ -> (
      try
        let dl =
          Eliommod_sessiongroups.Data.find
            (sitedata.Eliom_common.site_dir_string, sub_states_level, Left id)
        in
        fold f e dl
      with Not_found -> return e)
    | _, `Service, _ -> (
      try
        let dl =
          Eliommod_sessiongroups.Serv.find
            (sitedata.Eliom_common.site_dir_string, sub_states_level, Left id)
        in
        fold f e dl
      with Not_found -> return e)
    | _ -> failwith "fold_sub_states_aux"

  let fold_volatile_sub_states ?sitedata
      ~(state : Eliom_common.user_scope * [> `Data | `Service] * string) f e
    =
    let state' = (state :> ('aa, 'bb) state) in
    let a = fold_sub_states_aux_aux ?sitedata ~state:state' f in
    fold_sub_states_aux Ocsigen_cache.Dlist.fold Ocsigen_lib.id a e state

  let fold_sub_states ?sitedata ~state f e =
    let ((sitedata, sub_states_level, id, f) as a) =
      fold_sub_states_aux_aux ?sitedata ~state f
    in
    match state with
    | _, `Pers, _ ->
        Eliommod_sessiongroups.Pers.find
          (Eliom_common.make_persistent_full_group_name
             ~cookie_level:sub_states_level
             sitedata.Eliom_common.site_dir_string (Some id))
        >>= fun l -> Lwt_list.fold_left_s f e l
    | _ -> fold_sub_states_aux Ocsigen_cache.Dlist.lwt_fold Lwt.return a e state

  let iter_volatile_sub_states ?sitedata ~state f =
    fold_volatile_sub_states ?sitedata ~state (fun () -> f) ()

  let iter_sub_states ?sitedata ~state f =
    fold_sub_states ?sitedata ~state (fun () -> f) ()

  exception Wrong_scope

  module Low_level = struct
    (* We have a dynamic scope checking here.
       Would probably be possible to use phantom types again to check this
       statically. I don't want to make the types more complex for now.
       -- Vincent
    *)

    let check_scopes table_scope state_scope =
      if table_scope <> state_scope then raise Wrong_scope

    let lwt_check_scopes a b =
      try check_scopes a b; Lwt.return_unit with e -> Lwt.fail e

    (*VVV Does not work with volatile group data *)
    let get_volatile_data
        ~state:((state_scope, _, cookie) : ('s, [`Data]) state)
        ~table:((table_scope, _secure, t) : 'a volatile_table)
      =
      check_scopes table_scope state_scope;
      Eliom_common.SessionCookies.find t cookie

    let get_persistent_data
        ~state:((state_scope, _, cookie) : ('s, [`Pers]) state)
        ~table:((table_scope, _, t) : 'a persistent_table)
      =
      lwt_check_scopes table_scope state_scope >>= fun () ->
      Ocsipersist.find t cookie >>= fun (_, a) -> Lwt.return a

    let set_volatile_data
        ~state:((state_scope, _, cookie) : ('s, [`Data]) state)
        ~table:((table_scope, _secure, t) : 'a volatile_table) value
      =
      check_scopes table_scope state_scope;
      Eliom_common.SessionCookies.replace t cookie value

    let set_persistent_data
        ~state:((state_scope, _, cookie) : ('s, [`Pers]) state)
        ~table:((table_scope, _, t) : 'a persistent_table) value
      =
      lwt_check_scopes table_scope state_scope >>= fun () ->
      Ocsipersist.add t cookie (Int64.zero, value)

    let remove_volatile_data
        ~state:((state_scope, _, cookie) : ('s, [`Data]) state)
        ~table:((table_scope, _, t) : 'a volatile_table)
      =
      check_scopes table_scope state_scope;
      Eliom_common.SessionCookies.remove t cookie

    let remove_persistent_data
        ~state:((state_scope, _, cookie) : ('s, [`Pers]) state)
        ~table:((table_scope, _, t) : 'a persistent_table)
      =
      lwt_check_scopes table_scope state_scope >>= fun () ->
      Ocsipersist.remove t cookie
  end

  let get_service_cookie_scope ~cookie:(_, cookie) =
    cookie.Eliom_common.Service_cookie.full_state_name.Eliom_common.user_scope

  let get_volatile_data_cookie_scope ~cookie:(_, data_cookie) =
    data_cookie.Eliom_common.Data_cookie.full_state_name.Eliom_common.user_scope

  let get_persistent_data_cookie_scope ~cookie:(_, cookie) =
    cookie.Eliommod_cookies.full_state_name.Eliom_common.user_scope

  let set_service_cookie_timeout ~cookie:(_, cookie) t =
    cookie.Eliom_common.Service_cookie.timeout :=
      match t with None -> TNone | Some t -> TSome t

  let set_volatile_data_cookie_timeout ~cookie:(_, data_cookie) t =
    data_cookie.Eliom_common.Data_cookie.timeout :=
      match t with None -> TNone | Some t -> TSome t

  let set_persistent_data_cookie_timeout ~cookie:(c, cookie) t =
    let ti = match t with None -> TNone | Some t -> TSome t in
    Eliommod_cookies.Persistent_cookies.add c
      {cookie with Eliommod_cookies.timeout = ti}

  let get_service_cookie_timeout ~cookie:(_, cookie) =
    !(cookie.Eliom_common.Service_cookie.timeout)

  let get_volatile_data_cookie_timeout ~cookie:(_, data_cookie) =
    !(data_cookie.Eliom_common.Data_cookie.timeout)

  let get_persistent_data_cookie_timeout ~cookie:(_, cookie) =
    cookie.Eliommod_cookies.timeout

  let unset_service_cookie_timeout ~cookie:(_, cookie) =
    cookie.Eliom_common.Service_cookie.timeout := TGlobal

  let unset_volatile_data_cookie_timeout ~cookie:(_cookie, data_cookie) =
    data_cookie.Eliom_common.Data_cookie.timeout := TGlobal

  let unset_persistent_data_cookie_timeout ~cookie:(c, cookie) =
    Eliommod_cookies.Persistent_cookies.Cookies.add c
      {cookie with Eliommod_cookies.timeout = TGlobal}
    >>= fun () ->
    let {Eliommod_cookies.expiry; _} = cookie in
    Eliommod_cookies.Persistent_cookies.Expiry_dates.remove_cookie expiry c

  let get_session_group_list () =
    let sitedata = Eliom_request_info.find_sitedata "get_session_group_list" in
    let dl = sitedata.Eliom_common.group_of_groups in
    Ocsigen_cache.Dlist.fold
      (fun l -> function _, `Session, Left s -> s :: l | _ -> l)
      [] dl

  (** Iterator on service cookies *)
  let iter_service_cookies = Eliommod_sessexpl.iter_service_cookies

  (** Iterator on data cookies *)
  let iter_volatile_data_cookies = Eliommod_sessexpl.iter_data_cookies

  (** Iterator on persistent cookies *)
  let iter_persistent_data_cookies = Eliommod_sessexpl.iter_persistent_cookies

  (** Iterator on service cookies *)
  let fold_service_cookies = Eliommod_sessexpl.fold_service_cookies

  (** Iterator on data cookies *)
  let fold_volatile_data_cookies = Eliommod_sessexpl.fold_data_cookies

  (** Iterator on persistent cookies *)
  let fold_persistent_data_cookies = Eliommod_sessexpl.fold_persistent_cookies
end

(*****************************************************************************)
(* Exploration *)

let number_of_service_cookies = Eliommod_sessexpl.number_of_service_cookies
let number_of_volatile_data_cookies = Eliommod_sessexpl.number_of_data_cookies
let number_of_tables = Eliommod_sessexpl.number_of_tables
let number_of_table_elements = Eliommod_sessexpl.number_of_table_elements

let number_of_persistent_data_cookies =
  Eliommod_sessexpl.number_of_persistent_cookies

let number_of_persistent_tables =
  Eliom_common.Persistent_tables.number_of_tables

let number_of_persistent_table_elements =
  Eliom_common.Persistent_tables.number_of_table_elements

(*****************************************************************************)
let get_service_cookie ~cookie_scope ?secure () =
  try
    let c =
      Eliommod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure
        ()
    in
    Some c.Eliom_common.sc_hvalue
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_volatile_data_cookie ~cookie_scope ?secure () =
  try
    let c =
      Eliommod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ()
    in
    Some c.Eliom_common.dc_hvalue
  with Not_found | Eliom_common.Eliom_Session_expired -> None

let get_persistent_data_cookie ~cookie_scope ?secure () =
  try%lwt
    let%lwt c =
      Eliommod_persess.find_persistent_cookie_only ~cookie_scope
        ~secure_o:secure ()
    in
    return_some c.Eliom_common.pc_hvalue
  with Not_found | Eliom_common.Eliom_Session_expired -> return_none

(*****************************************************************************)
(** {2 User cookies} *)

let change_pathopt_ sp = function
  | None ->
      (Eliom_request_info.get_sitedata_sp ~sp).Eliom_common.site_dir
      (* Not possible to set a cookie for another site (?) *)
  | Some p -> (Eliom_request_info.get_sitedata_sp ~sp).Eliom_common.site_dir @ p

let set_cookie ?(cookie_level = `Session) ?path ?exp ?secure ~name ~value () =
  let sp = Eliom_common.get_sp () in
  let path = change_pathopt_ sp path in
  let sitedata = Eliom_request_info.find_sitedata "set_cookie" in
  let secure = Eliom_common.get_secure ~secure_o:secure ~sitedata () in
  match cookie_level with
  | `Session ->
      sp.Eliom_common.sp_user_cookies <-
        Ocsigen_cookie_map.add ~path name
          (OSet (exp, value, secure))
          sp.Eliom_common.sp_user_cookies
  | `Client_process ->
      sp.Eliom_common.sp_user_tab_cookies <-
        Ocsigen_cookie_map.add ~path name
          (OSet (exp, value, secure))
          sp.Eliom_common.sp_user_tab_cookies

let unset_cookie ?(cookie_level = `Session) ?path ~name () =
  let sp = Eliom_common.get_sp () in
  let path = change_pathopt_ sp path in
  match cookie_level with
  | `Session ->
      sp.Eliom_common.sp_user_cookies <-
        Ocsigen_cookie_map.add ~path name OUnset sp.Eliom_common.sp_user_cookies
  | `Client_process ->
      sp.Eliom_common.sp_user_tab_cookies <-
        Ocsigen_cookie_map.add ~path name OUnset
          sp.Eliom_common.sp_user_tab_cookies
