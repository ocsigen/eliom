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

open Lwt.Syntax
open Lib

(* Expired session? *)
type state_status = Alive_state | Empty_state | Expired_state

let service_state_status ~scope ?secure () =
  let cookie_scope = Common.cookie_scope_of_user_scope scope in
  try
    ignore
      (Mod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure ());
    Alive_state
  with
  | Not_found -> Empty_state
  | Common.Eliom_Session_expired -> Expired_state

let volatile_data_state_status ~scope ?secure () =
  let cookie_scope = Common.cookie_scope_of_user_scope scope in
  try
    ignore
      (Mod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ());
    Alive_state
  with
  | Not_found -> Empty_state
  | Common.Eliom_Session_expired -> Expired_state

let persistent_data_state_status ~scope ?secure () =
  let cookie_scope = Common.cookie_scope_of_user_scope scope in
  Lwt.catch
    (fun () ->
       let* _ =
         Mod_persess.find_persistent_cookie_only ~cookie_scope ~secure_o:secure
           ()
       in
       Lwt.return Alive_state)
    (function
      | Not_found -> Lwt.return Empty_state
      | Common.Eliom_Session_expired -> Lwt.return Expired_state
      | e -> Lwt.fail e)

(************)
let set_default_global_service_state_timeout
      ~cookie_level
      ?(override_configfile = false)
      timeout
  =
  let sitedata = Request_info.find_sitedata "set_global_service_timeout" in
  Mod_timeouts.set_default_global `Service cookie_level ~override_configfile
    ~from_configfile:false sitedata timeout

let set_global_service_state_timeout
      ~cookie_scope
      ?secure
      ?(recompute_expdates = false)
      ?(override_configfile = false)
      timeout
  =
  let sitedata = Request_info.find_sitedata "set_global_service_timeout" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_timeouts.set_global ~kind:`Service ~cookie_scope ~secure
    ~recompute_expdates ~override_configfile sitedata timeout

let set_default_global_volatile_data_state_timeout
      ~cookie_level
      ?(override_configfile = false)
      timeout
  =
  let sitedata = Request_info.find_sitedata "set_global_data_timeout" in
  Mod_timeouts.set_default_global `Data cookie_level ~override_configfile
    ~from_configfile:false sitedata timeout

let set_global_volatile_data_state_timeout
      ~cookie_scope
      ?secure
      ?(recompute_expdates = false)
      ?(override_configfile = false)
      timeout
  =
  let sitedata = Request_info.find_sitedata "set_global_data_timeout" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_timeouts.set_global ~kind:`Data ~cookie_scope ~secure ~recompute_expdates
    ~override_configfile sitedata timeout

let set_global_volatile_state_timeout
      ~cookie_scope
      ?secure
      ?(recompute_expdates = false)
      ?(override_configfile = false)
      timeout
  =
  let sitedata = Request_info.find_sitedata "set_global_volatile_timeouts" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_timeouts.set_global ~kind:`Service ~cookie_scope ~secure
    ~recompute_expdates ~override_configfile sitedata timeout;
  Mod_timeouts.set_global ~kind:`Data ~cookie_scope ~secure ~recompute_expdates
    ~override_configfile sitedata timeout

let set_default_global_persistent_data_state_timeout
      ~cookie_level
      ?(override_configfile = false)
      timeout
  =
  let sitedata = Request_info.find_sitedata "set_global_persistent_timeout" in
  Mod_timeouts.set_default_global `Persistent cookie_level ~override_configfile
    ~from_configfile:false sitedata timeout

let set_global_persistent_data_state_timeout
      ~cookie_scope
      ?secure
      ?(recompute_expdates = false)
      ?(override_configfile = false)
      timeout
  =
  let sitedata = Request_info.find_sitedata "set_global_persistent_timeout" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_timeouts.set_global ~kind:`Persistent ~cookie_scope ~secure
    ~recompute_expdates ~override_configfile sitedata timeout

let get_global_service_state_timeout ?secure ~cookie_scope () =
  let sitedata = Request_info.find_sitedata "get_global_timeout" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_timeouts.get_global ~kind:`Service ~cookie_scope ~secure sitedata

let get_global_volatile_data_state_timeout ?secure ~cookie_scope () =
  let sitedata = Request_info.find_sitedata "get_global_timeout" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_timeouts.get_global ~kind:`Data ~cookie_scope ~secure sitedata

let get_global_persistent_data_state_timeout ?secure ~cookie_scope () =
  let sitedata = Request_info.find_sitedata "get_global_persistent_timeout" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_timeouts.get_global ~kind:`Persistent ~cookie_scope ~secure sitedata

(* Now for current session *)
let set_service_state_timeout ~cookie_scope ?secure t =
  let c =
    Mod_sersess.find_or_create_service_cookie ~cookie_scope ~secure_o:secure ()
  in
  let tor = c.Common.sc_timeout in
  tor := Common.timeout_of_option t

let set_volatile_data_state_timeout ~cookie_scope ?secure t =
  let c =
    Mod_datasess.find_or_create_data_cookie ~cookie_scope ~secure_o:secure ()
  in
  let tor = c.Common.dc_timeout in
  tor := Common.timeout_of_option t

let unset_service_state_timeout ~cookie_scope ?secure () =
  try
    let c =
      Mod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure ()
    in
    let tor = c.Common.sc_timeout in
    tor := Common.TGlobal
  with Not_found | Common.Eliom_Session_expired -> ()

let unset_volatile_data_state_timeout ~cookie_scope ?secure () =
  try
    let c =
      Mod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ()
    in
    let tor = c.Common.dc_timeout in
    tor := Common.TGlobal
  with Not_found | Common.Eliom_Session_expired -> ()

let get_service_state_timeout ~cookie_scope ?secure () =
  let sp = Common.get_sp () in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  try
    let c =
      Mod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:(Some secure)
        ~sp ()
    in
    let tor = c.Common.sc_timeout in
    match !tor with
    | Common.TGlobal ->
        Mod_timeouts.get_global ~kind:`Service ~cookie_scope ~secure sitedata
    | Common.TNone -> None
    | Common.TSome t -> Some t
  with Not_found | Common.Eliom_Session_expired ->
    Mod_timeouts.get_global ~kind:`Service ~cookie_scope ~secure sitedata

let get_volatile_data_state_timeout ~cookie_scope ?secure () =
  let sp = Common.get_sp () in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  try
    let c =
      Mod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:(Some secure)
        ~sp ()
    in
    let tor = c.Common.dc_timeout in
    match !tor with
    | Common.TGlobal ->
        Mod_timeouts.get_global ~kind:`Data ~cookie_scope ~secure sitedata
    | Common.TNone -> None
    | Common.TSome t -> Some t
  with Not_found | Common.Eliom_Session_expired ->
    Mod_timeouts.get_global ~kind:`Data ~cookie_scope ~secure sitedata

let set_persistent_data_state_timeout ~cookie_scope ?secure t =
  let* c =
    Mod_persess.find_or_create_persistent_cookie ~cookie_scope ~secure_o:secure
      ()
  in
  let tor = c.Common.pc_timeout in
  Lwt.return (tor := Common.timeout_of_option t)

let unset_persistent_data_state_timeout ~cookie_scope ?secure () =
  Lwt.catch
    (fun () ->
       let* c =
         Mod_persess.find_persistent_cookie_only ~cookie_scope ~secure_o:secure
           ()
       in
       let tor = c.Common.pc_timeout in
       tor := Common.TGlobal;
       Lwt.return_unit)
    (function
      | Not_found | Common.Eliom_Session_expired -> Lwt.return_unit
      | exc -> Lwt.fail exc)

let get_persistent_data_state_timeout ~cookie_scope ?secure () =
  let sp = Common.get_sp () in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Lwt.catch
    (fun () ->
       let* c =
         Mod_persess.find_persistent_cookie_only ~cookie_scope
           ~secure_o:(Some secure) ~sp ()
       in
       let tor = c.Common.pc_timeout in
       Lwt.return
         (match !tor with
         | Common.TGlobal ->
             Mod_timeouts.get_global ~kind:`Persistent ~cookie_scope ~secure
               sitedata
         | Common.TNone -> None
         | Common.TSome t -> Some t))
    (function
      | Not_found | Common.Eliom_Session_expired ->
          Lwt.return
            (Mod_timeouts.get_global ~kind:`Persistent ~cookie_scope ~secure
               sitedata)
      | exc -> Lwt.fail exc)

(* Preventing memory leaks: we must close empty sessions *)

let rec close_service_state_if_empty ~scope ?secure () =
  (* Close the session if it has not services inside
     and no group and no sub sessions *)
  (* See also in Mod_gc and in Mod_sessiongroups. *)
  try
    let sp = Common.get_sp () in
    let sitedata = Request_info.get_sitedata_sp ~sp in
    let cookie_scope = Common.cookie_scope_of_user_scope scope in
    let c =
      Mod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure ~sp ()
    in
    match scope with
    | `Session _ ->
        if
          Mod_sessiongroups.Serv.group_size
            { Common.sg_site_dir = Common.get_site_dir_string sitedata
            ; sg_level = `Client_process
            ; sg_group =
                Common.Group_name Common.(Hashed_cookies.to_string c.sc_hvalue)
            }
          = 0
          (* no tab sessions *)
          && Common.service_tables_are_empty !(c.Common.sc_table)
        then Mod_sessiongroups.Serv.remove c.Common.sc_session_group_node
    | `Client_process _ ->
        if Common.service_tables_are_empty !(c.Common.sc_table)
        then Mod_sessiongroups.Serv.remove c.Common.sc_session_group_node
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
  (* See also in Mod_gc and in Mod_sessiongroups. *)
  try
    let sp = Common.get_sp () in
    let sitedata = Request_info.get_sitedata_sp ~sp in
    let cookie_scope = Common.cookie_scope_of_user_scope scope in
    let c =
      Mod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ~sp ()
    in
    match scope with
    | `Session _ -> (
      match !(c.Common.dc_session_group) with
      | {Common.sg_group = Common.No_group; _}
      (* no group *)
        when Mod_sessiongroups.Data.group_size
               { Common.sg_site_dir = Common.get_site_dir_string sitedata
               ; sg_level = `Client_process
               ; sg_group =
                   Common.Group_name
                     Common.(Hashed_cookies.to_string c.dc_hvalue) }
             = 0
             (* no tab sessions *)
             && sitedata.Common.not_bound_in_data_tables
                  Common.(Hashed_cookies.to_string c.dc_hvalue) ->
          Mod_sessiongroups.Data.remove c.Common.dc_session_group_node
      | _ -> ())
    | `Client_process _ -> ()
    (* Nothing to close: we always have tab session data when we have a tab
       session (at least the change_page_event). *)
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

let set_service_session_group
      ?set_max
      ?(scope = Common.default_session_scope)
      ?secure
      session_group
  =
  Option.iter (Common.check_limit "State.set_service_session_group") set_max;
  let c =
    Mod_sersess.find_or_create_service_cookie ~set_session_group:session_group
      ~cookie_scope:(scope :> Common.cookie_scope)
      ~secure_o:secure ()
  in
  match set_max with
  | None -> ()
  | Some m -> Mod_sessiongroups.Serv.set_max c.Common.sc_session_group_node m

let unset_service_session_group
      ?(scope = Common.default_session_scope)
      ?secure
      ()
  =
  try
    let sp = Common.get_sp () in
    let sitedata = Request_info.get_sitedata_sp ~sp in
    let c =
      Mod_sersess.find_service_cookie_only
        ~cookie_scope:(scope :> Common.cookie_scope)
        ~secure_o:secure ~sp ()
    in
    let n =
      Mod_sessiongroups.make_full_group_name ~cookie_level:`Session ~sitedata
        None
    in
    let node =
      Mod_sessiongroups.Serv.move sitedata c.Common.sc_session_group_node n
    in
    c.Common.sc_session_group_node <- node;
    c.Common.sc_session_group := n;
    (* Now we want to close the session if it has not data inside
       and no tab sessions *)
    close_service_state_if_empty ~scope:(scope :> Common.user_scope) ?secure ()
  with Not_found | Common.Eliom_Session_expired -> ()

let get_service_session_group ?(scope = Common.default_session_scope) ?secure ()
  =
  try
    let c =
      Mod_sersess.find_service_cookie_only
        ~cookie_scope:(scope :> Common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Common.sc_session_group) with
    | {Common.sg_group = Common.No_group; _} -> None
    | {Common.sg_group = Common.Group_name v; _} -> Some v
  with Not_found | Common.Eliom_Session_expired -> None

let get_service_session_group_size
      ?(scope = Common.default_session_scope)
      ?secure
      ()
  =
  try
    let c =
      Mod_sersess.find_service_cookie_only
        ~cookie_scope:(scope :> Common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Common.sc_session_group) with
    | {Common.sg_group = Common.No_group; _} -> None
    | {Common.sg_group = Common.Group_name _; _} ->
        Some (Mod_sessiongroups.Serv.group_size !(c.Common.sc_session_group))
  with Not_found | Common.Eliom_Session_expired -> None

let set_volatile_data_session_group
      ?set_max
      ?(scope = Common.default_session_scope)
      ?secure
      session_group
  =
  Option.iter
    (Common.check_limit "State.set_volatile_data_session_group")
    set_max;
  let c =
    Mod_datasess.find_or_create_data_cookie ~set_session_group:session_group
      ~cookie_scope:(scope :> Common.cookie_scope)
      ~secure_o:secure ()
  in
  match set_max with
  | None -> ()
  | Some m -> Mod_sessiongroups.Data.set_max c.Common.dc_session_group_node m

let unset_volatile_data_session_group
      ?(scope = Common.default_session_scope)
      ?secure
      ()
  =
  try
    let sp = Common.get_sp () in
    let sitedata = Request_info.get_sitedata_sp ~sp in
    let c =
      Mod_datasess.find_data_cookie_only
        ~cookie_scope:(scope :> Common.cookie_scope)
        ~secure_o:secure ~sp ()
    in
    let n =
      Mod_sessiongroups.make_full_group_name ~cookie_level:`Session ~sitedata
        None
    in
    let node =
      Mod_sessiongroups.Data.move sitedata c.Common.dc_session_group_node n
    in
    c.Common.dc_session_group_node <- node;
    c.Common.dc_session_group := n;
    (* Now we want to close the session if it has not data inside
       and no tab sessions *)
    close_volatile_state_if_empty ~scope:(scope :> Common.user_scope) ?secure ()
  with Not_found | Common.Eliom_Session_expired -> ()

let get_volatile_data_session_group
      ?(scope = Common.default_session_scope)
      ?secure
      ()
  =
  try
    let c =
      Mod_datasess.find_data_cookie_only
        ~cookie_scope:(scope :> Common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Common.dc_session_group) with
    | {Common.sg_group = Common.No_group; _} -> None
    | {Common.sg_group = Common.Group_name v; _} -> Some v
  with Not_found | Common.Eliom_Session_expired -> None

let get_volatile_data_session_group_size
      ?(scope = Common.default_session_scope)
      ?secure
      ()
  =
  try
    let c =
      Mod_datasess.find_data_cookie_only
        ~cookie_scope:(scope :> Common.cookie_scope)
        ~secure_o:secure ()
    in
    match !(c.Common.dc_session_group) with
    | {Common.sg_group = Common.No_group; _} -> None
    | {Common.sg_group = Common.Group_name _; _} ->
        Some (Mod_sessiongroups.Data.group_size !(c.Common.dc_session_group))
  with Not_found | Common.Eliom_Session_expired -> None

let set_persistent_data_session_group
      ?set_max
      ?(scope = Common.default_session_scope)
      ?secure
      n
  =
  Option.iter
    (Option.iter (Common.check_limit "State.set_persistent_data_session_group"))
    set_max;
  let sp = Common.get_sp () in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let* c =
    Mod_persess.find_or_create_persistent_cookie
      ~cookie_scope:(scope :> Common.cookie_scope)
      ~secure_o:secure ~sp ()
  in
  let n =
    Mod_sessiongroups.make_persistent_full_group_name ~cookie_level:`Session
      (Common.get_site_dir_string sitedata)
      (Some n)
  in
  let grp = c.Common.pc_session_group in
  let* l =
    Mod_sessiongroups.Pers.move sitedata ?set_max
      sitedata.Common.max_persistent_data_sessions_per_group.Common.cf_value
      Common.(Hashed_cookies.to_string c.pc_hvalue)
      !grp n
  in
  let* () =
    Lwt_list.iter_p
      (Mod_persess.close_persistent_state_of_cookie
         ~scope:(scope :> Common.user_scope)
         sitedata None)
      l
  in
  grp := n;
  Lwt.return_unit

let unset_persistent_data_session_group
      ?(scope = Common.default_session_scope)
      ?secure
      ()
  =
  let sp = Common.get_sp () in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  Lwt.catch
    (fun () ->
       let* c =
         Mod_persess.find_persistent_cookie_only
           ~cookie_scope:(scope :> Common.cookie_scope)
           ~secure_o:secure ~sp ()
       in
       let grp = c.Common.pc_session_group in
       let* () =
         Mod_sessiongroups.Pers.remove sitedata
           Common.(Hashed_cookies.to_string c.pc_hvalue)
           !grp
       in
       grp := None;
       close_persistent_state_if_empty
         ~scope:(scope :> Common.user_scope)
         ?secure ())
    (function
      | Not_found | Common.Eliom_Session_expired -> Lwt.return_unit
      | exc -> Lwt.fail exc)

let get_persistent_data_session_group
      ?(scope = Common.default_session_scope)
      ?secure
      ()
  =
  Lwt.catch
    (fun () ->
       let* c =
         Mod_persess.find_persistent_cookie_only
           ~cookie_scope:(scope :> Common.cookie_scope)
           ~secure_o:secure ()
       in
       Lwt.return
         (match !(c.Common.pc_session_group) with
         | None -> None
         | Some v -> (
           match Mod_sessiongroups.getperssessgrp v with
           | {Common.sg_group = Common.Group_name s; _} -> Some s
           | _ -> None)))
    (function
      | Not_found | Common.Eliom_Session_expired -> Lwt.return_none
      | exc -> Lwt.fail exc)

(* max *)
(* [limit_sitedata name n] is the current site, if [n] is a valid limit for
   the function [name] *)
let limit_sitedata name n =
  Common.check_limit ("State." ^ name) n;
  Request_info.find_sitedata name

let set_default_max_service_sessions_per_group ?(override_configfile = false) n =
  let sitedata =
    limit_sitedata "set_default_max_service_sessions_per_group" n
  in
  sitedata.Common.max_service_sessions_per_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_service_sessions_per_group n

let set_default_max_volatile_data_sessions_per_group
      ?(override_configfile = false)
      n
  =
  let sitedata =
    limit_sitedata "set_default_max_volatile_data_sessions_per_group" n
  in
  sitedata.Common.max_volatile_data_sessions_per_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_volatile_data_sessions_per_group n

let set_default_max_persistent_data_sessions_per_group
      ?(override_configfile = false)
      n
  =
  Option.iter
    (Common.check_limit
       "State.set_default_max_persistent_data_sessions_per_group")
    n;
  let sitedata =
    Request_info.find_sitedata
      "set_default_max_persistent_data_sessions_per_group"
  in
  sitedata.Common.max_persistent_data_sessions_per_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_persistent_data_sessions_per_group n

(* The sessions without a group of the site are in one group, created with
   the first of them: a new limit applies to it at once. Before the site is
   linked to the application, there is no session yet. *)
let sessions_without_group name n =
  let sitedata = limit_sitedata name n in
  ( sitedata
  , Option.map
      (fun sg_site_dir ->
         {Common.sg_site_dir; sg_level = `Session; sg_group = Common.No_group})
      sitedata.Common.site_dir_string )

let set_default_max_service_sessions_without_group
      ?(override_configfile = false)
      n
  =
  let sitedata, group =
    sessions_without_group "set_default_max_service_sessions_without_group" n
  in
  sitedata.Common.max_service_sessions_without_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_service_sessions_without_group n;
  Option.iter
    (fun g ->
       Mod_sessiongroups.Serv.set_group_max g
         sitedata.Common.max_service_sessions_without_group.Common.cf_value)
    group

let set_default_max_volatile_data_sessions_without_group
      ?(override_configfile = false)
      n
  =
  let sitedata, group =
    sessions_without_group
      "set_default_max_volatile_data_sessions_without_group" n
  in
  sitedata.Common.max_volatile_data_sessions_without_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_volatile_data_sessions_without_group n;
  Option.iter
    (fun g ->
       Mod_sessiongroups.Data.set_group_max g
         sitedata.Common.max_volatile_data_sessions_without_group
           .Common.cf_value)
    group

let set_default_max_volatile_sessions_per_group ?override_configfile n =
  set_default_max_service_sessions_per_group ?override_configfile n;
  set_default_max_volatile_data_sessions_per_group ?override_configfile n

let set_default_max_volatile_sessions_without_group ?override_configfile n =
  set_default_max_service_sessions_without_group ?override_configfile n;
  set_default_max_volatile_data_sessions_without_group ?override_configfile n

let set_default_max_service_tab_sessions_per_group
      ?(override_configfile = false)
      n
  =
  let sitedata =
    limit_sitedata "set_default_max_service_tab_sessions_per_group" n
  in
  sitedata.Common.max_service_tab_sessions_per_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_service_tab_sessions_per_group n

let set_default_max_volatile_data_tab_sessions_per_group
      ?(override_configfile = false)
      n
  =
  let sitedata =
    limit_sitedata "set_default_max_volatile_data_tab_sessions_per_group" n
  in
  sitedata.Common.max_volatile_data_tab_sessions_per_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_volatile_data_tab_sessions_per_group n

let set_default_max_persistent_data_tab_sessions_per_group
      ?(override_configfile = false)
      n
  =
  Option.iter
    (Common.check_limit
       "State.set_default_max_persistent_data_tab_sessions_per_group")
    n;
  let sitedata =
    Request_info.find_sitedata
      "set_default_max_persistent_data_tab_sessions_per_group"
  in
  sitedata.Common.max_persistent_data_tab_sessions_per_group <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_persistent_data_tab_sessions_per_group n

let set_default_max_volatile_tab_sessions_per_group ?override_configfile n =
  set_default_max_service_tab_sessions_per_group ?override_configfile n;
  set_default_max_volatile_data_tab_sessions_per_group ?override_configfile n

let set_default_max_anonymous_services_per_site ?(override_configfile = false) n
  =
  let sitedata =
    limit_sitedata "set_default_max_anonymous_services_per_site" n
  in
  sitedata.Common.max_anonymous_services_per_site <-
    Common.set_configured ~override:override_configfile
      sitedata.Common.max_anonymous_services_per_site n;
  Common.set_max_anonymous_services sitedata.Common.anonymous_services
    sitedata.Common.max_anonymous_services_per_site.Common.cf_value

(* The parent of the session groups is the group of groups of the site *)
let set_max_groups_of_site name m =
  let sitedata = Request_info.find_sitedata name in
  ignore
    (Ocsigen_base.Cache.Dlist.set_maxsize sitedata.Common.group_of_groups m)

(* [set_max_in_parent name ~scope session_group set_max] calls [set_max ()],
   which sets the maximum size of the parent of the current state of scope
   [scope], whose session group is [session_group]. A session without a
   group has no parent of its own: the sessions without a group of the site
   are not limited this way. *)
let set_max_in_parent name ~scope session_group set_max =
  match scope, session_group with
  | `Session _, {Common.sg_group = Common.No_group; _} ->
      Logs.warn ~src:eliom_logs_src (fun fmt ->
        fmt
          "State.%s: the session is not in a session group, the limit is not changed"
          name)
  | _ -> set_max ()

let set_max_service_states_in_parent ~scope ?secure m =
  let name = "set_max_service_states_in_parent" in
  Common.check_limit ("State." ^ name) m;
  match scope with
  | `Session_group _ -> set_max_groups_of_site name m
  | `Session _ | `Client_process _ ->
      let c =
        Mod_sersess.find_or_create_service_cookie ~secure_o:secure
          ~cookie_scope:(Common.cookie_scope_of_user_scope scope)
          ()
      in
      set_max_in_parent name ~scope !(c.Common.sc_session_group) (fun () ->
        Mod_sessiongroups.Serv.set_max c.Common.sc_session_group_node m)

let set_max_volatile_data_states_in_parent ~scope ?secure m =
  let name = "set_max_volatile_data_states_in_parent" in
  Common.check_limit ("State." ^ name) m;
  match scope with
  | `Session_group _ -> set_max_groups_of_site name m
  | `Session _ | `Client_process _ ->
      let c =
        Mod_datasess.find_or_create_data_cookie
          ~cookie_scope:(Common.cookie_scope_of_user_scope scope)
          ~secure_o:secure ()
      in
      set_max_in_parent name ~scope !(c.Common.dc_session_group) (fun () ->
        Mod_sessiongroups.Data.set_max c.Common.dc_session_group_node m)

let set_max_volatile_states_in_parent ~scope ?secure m =
  set_max_service_states_in_parent ~scope ?secure m;
  set_max_volatile_data_states_in_parent ~scope ?secure m

(*VVV No version for persistent sessions? Why? *)

(* expiration dates *)
let set_service_cookie_exp_date ~cookie_scope ?secure t =
  let c =
    Mod_sersess.find_or_create_service_cookie ~cookie_scope ~secure_o:secure ()
  in
  let exp = c.Common.sc_cookie_exp in
  match t with
  | None -> exp := Common.CEBrowser
  | Some t -> exp := Common.CESome t

let set_volatile_data_cookie_exp_date ~cookie_scope ?secure t =
  let c =
    Mod_datasess.find_or_create_data_cookie ~cookie_scope ~secure_o:secure ()
  in
  let exp = c.Common.dc_cookie_exp in
  match t with
  | None -> exp := Common.CEBrowser
  | Some t -> exp := Common.CESome t

let set_persistent_data_cookie_exp_date ~cookie_scope ?secure t =
  let* c =
    Mod_persess.find_or_create_persistent_cookie ~cookie_scope ~secure_o:secure
      ()
  in
  let exp = c.Common.pc_cookie_exp in
  Lwt.return
    (match t with
    | None -> exp := Common.CEBrowser
    | Some t -> exp := Common.CESome t)

(* *)
let get_global_table () =
  let sitedata = Request_info.find_sitedata "get_global_table" in
  sitedata.Common.global_services

(** If the session does not exist, we create it
   (new cookie, new session service table) *)
let session_service_table ~scope c =
  match scope, !(c.Common.sc_session_group) with
  | `Session_group _, {Common.sg_group = Common.Group_name _; _} -> (
    match
      Mod_sessiongroups.Serv.find_node_in_group_of_groups
        !(c.Common.sc_session_group)
    with
    | None -> raise Not_found
    | Some (t, _) -> t)
  | `Session_group _, {Common.sg_group = Common.No_group; _} ->
      (* No session group has been set: the session is its own group, as for
         the data of the group scope. *)
      c.Common.sc_table
  | _ -> c.Common.sc_table

let get_session_service_table ~sp ~scope ?secure () =
  let cookie_scope = Common.cookie_scope_of_user_scope scope in
  let c =
    Mod_sersess.find_or_create_service_cookie ~cookie_scope ~secure_o:secure ~sp
      ()
  in
  session_service_table ~scope c

(** If the session does not exist, we raise Not_found *)
let get_session_service_table_if_exists ~sp ~scope ?secure () =
  let cookie_scope = Common.cookie_scope_of_user_scope scope in
  try
    let c =
      Mod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure ~sp ()
    in
    session_service_table ~scope c
  with Common.Eliom_Session_expired -> raise Not_found

(*****************************************************************************)
(** {2 persistent sessions} *)

type 'a persistent_table =
  (module Common.Ocsipersist.TABLE with type key = string and type value = 'a)
    Common.state_table

let create_persistent_table ~scope ?secure ~json name :
  'a persistent_table Lwt.t
  =
  let sitedata = Request_info.find_sitedata "create_persistent_table" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  let t = Common.Persistent_tables.create_json ~name json in
  Lwt.return {Common.table_scope = scope; table_secure = secure; table = t}

let get_p_table_key_
      ~table:{Common.table_scope = scope; table_secure = secure; table}
      (find_cookie :
        cookie_scope:Common.cookie_scope
        -> secure_o:bool option
        -> ?sp:Common.server_params
        -> unit
        -> Common.one_persistent_cookie_info Lwt.t)
  =
  let get_cookie () =
    let cookie_scope = Common.cookie_scope_of_user_scope scope in
    let* c = find_cookie ~cookie_scope ~secure_o:(Some secure) () in
    Lwt.return Common.(Hashed_cookies.to_string c.pc_hvalue)
  in
  let* key =
    match scope with
    | `Session_group state_name -> (
        let* group =
          get_persistent_data_session_group ~scope:(`Session state_name) ~secure
            ()
        in
        match group with
        | Some a -> Lwt.return a
        | None ->
            (* No session group. We use the session cookie as key. *)
            get_cookie ())
    | _ -> get_cookie ()
  in
  Lwt.return (table, key)

let get_persistent_data (type a) ~(table : a persistent_table) () =
  Lwt.catch
    (fun () ->
       let* table, key =
         get_p_table_key_ ~table Mod_persess.find_persistent_cookie_only
       in
       let module T =
         (val table
           : Common.Ocsipersist.TABLE with type key = string and type value = a)
       in
       let* v = T.find key in
       Lwt.return (Data v))
    (function
      | Common.Eliom_Session_expired -> Lwt.return Data_session_expired
      | Not_found -> Lwt.return No_data
      | e -> Lwt.fail e)

let set_persistent_data (type a) ~(table : a persistent_table) (value : a) =
  let find_or_create_cookie ~cookie_scope ~secure_o ?sp () =
    Mod_persess.find_or_create_persistent_cookie ~cookie_scope ~secure_o ?sp ()
  in
  let* table, key = get_p_table_key_ ~table find_or_create_cookie in
  let module T = (val table) in
  T.add key value

let remove_persistent_data (type a) ~(table : a persistent_table) () =
  Lwt.catch
    (fun () ->
       let {Common.table_scope = scope; table_secure = secure; _} = table in
       let* table, key =
         get_p_table_key_ ~table Mod_persess.find_persistent_cookie_only
       in
       let module T =
         (val table
           : Common.Ocsipersist.TABLE with type key = string and type value = a)
       in
       let* () = T.remove key in
       close_persistent_state_if_empty ~scope ~secure ())
    (function
      | Not_found | Common.Eliom_Session_expired -> Lwt.return_unit
      | exc -> Lwt.fail exc)

(*****************************************************************************)
(** {2 session data in memory} *)

type 'a volatile_table = 'a Common.SessionCookies.t Common.state_table

let create_volatile_table ~scope ?secure () =
  match Common.get_sp_option () with
  | None -> (
    match Common.global_register_allowed () with
    | Some get_current_sitedata ->
        let sitedata = get_current_sitedata () in
        let secure = Common.get_secure ~secure_o:secure ~sitedata in
        Mod_datasess.create_volatile_table ~scope ~secure
    | None ->
        raise (Common.Site_information_not_available "create_volatile_table"))
  | Some sp ->
      let sitedata = Request_info.get_sitedata_sp ~sp in
      let secure = Common.get_secure ~secure_o:secure ~sitedata in
      Mod_datasess.create_volatile_table_during_session ~scope ~secure sitedata

let get_table_key_
      ~table:{Common.table_scope = scope; table_secure = secure; table}
      (find_cookie :
        cookie_scope:Common.cookie_scope
        -> secure_o:bool option
        -> ?sp:Common.server_params
        -> unit
        -> Common.one_data_cookie_info)
  =
  (* The key in the table is the cookie for client processes and sessions,
     and the group name for groups *)
  let get_cookie () =
    let cookie_scope = Common.cookie_scope_of_user_scope scope in
    let c = find_cookie ~cookie_scope ~secure_o:(Some secure) () in
    Common.(Hashed_cookies.to_string c.dc_hvalue)
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
    let table, key = get_table_key_ ~table Mod_datasess.find_data_cookie_only in
    Data (Common.SessionCookies.find table key)
  with
  | Not_found -> No_data
  | Common.Eliom_Session_expired -> Data_session_expired

let set_volatile_data ~table value =
  let find_or_create_cookie ~cookie_scope ~secure_o ?sp () =
    Mod_datasess.find_or_create_data_cookie ~cookie_scope ~secure_o ?sp ()
  in
  let table, key = get_table_key_ ~table find_or_create_cookie in
  Common.SessionCookies.replace table key value

let remove_volatile_data ~table () =
  try
    let {Common.table_scope = scope; table_secure = secure; _} = table in
    let table, key = get_table_key_ ~table Mod_datasess.find_data_cookie_only in
    Common.SessionCookies.remove table key;
    (* Now we want to close the session if it has not data inside
       and no group and no sub sessions *)
    close_volatile_state_if_empty ~scope ~secure ()
  with Not_found | Common.Eliom_Session_expired -> ()

(*****************************************************************************)

(** Close a state *)
let discard_persistent_data ~scope ?secure () =
  match secure with
  | None ->
      let* () =
        Mod_persess.close_persistent_state ~scope ~secure_o:(Some true) ()
      in
      Mod_persess.close_persistent_state ~scope ~secure_o:(Some false) ()
  | _ -> Mod_persess.close_persistent_state ~scope ~secure_o:secure ()

let discard_services ~scope ?secure () =
  match secure with
  | None ->
      Mod_sersess.close_service_state ~scope ~secure_o:(Some true) ();
      Mod_sersess.close_service_state ~scope ~secure_o:(Some false) ()
  | _ -> Mod_sersess.close_service_state ~scope ~secure_o:secure ()

let discard_volatile_data ~scope ?secure () =
  match secure with
  | None ->
      Mod_datasess.close_data_state ~scope ~secure_o:(Some true) ();
      Mod_datasess.close_data_state ~scope ~secure_o:(Some false) ()
  | _ -> Mod_datasess.close_data_state ~scope ~secure_o:secure ()

let discard_request_data () =
  let table = Request_info.get_request_cache () in
  Polytables.clear ~table; Lwt.return_unit

let discard_data ?persistent ~scope ?secure () =
  match scope with
  | #Common.request_scope -> discard_request_data ()
  | #Common.user_scope as scope -> (
      (match persistent with
      | None | Some false -> discard_volatile_data ~scope ?secure ()
      | _ -> ());
      match persistent with
      | None | Some true -> discard_persistent_data ~scope ?secure ()
      | _ -> Lwt.return_unit)

let discard ~scope ?secure () =
  match scope with
  | #Common.request_scope -> discard_request_data ()
  | #Common.user_scope as scope ->
      discard_services ~scope:(scope :> [< Common.user_scope]) ?secure ();
      discard_data ~scope:(scope :> [< Common.user_scope]) ?secure ()
(* will close volatile and persistent sessions for one scope *)

let discard_all_scopes ?secure () =
  let discard_name scope_hierarchy =
    let* () = discard ?secure ~scope:(`Session_group scope_hierarchy) () in
    let* () = discard ?secure ~scope:(`Session scope_hierarchy) () in
    discard ?secure ~scope:(`Client_process scope_hierarchy) ()
  in
  let* () = discard_request_data () in
  Lwt_list.iter_p discard_name (Common.list_scope_hierarchies ())

let discard_all_volatile_data ~scope ?secure () =
  let sitedata = Request_info.find_sitedata "discard_all_volatile_data" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_sessadmin.close_all_data_states ~scope ~secure sitedata
(*VVV missing: scope group *)

let discard_all_persistent_data ~scope ?secure () =
  let sitedata = Request_info.find_sitedata "discard_all_persistent_data" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_sessadmin.close_all_persistent_states ~scope ~secure sitedata
(*VVV missing: scope group *)

let discard_all_data ?persistent ~scope ?secure () =
  let* () =
    match persistent with
    | None | Some false -> discard_all_volatile_data ~scope ?secure ()
    | _ -> Lwt.return_unit
  in
  match persistent with
  | None | Some true -> discard_all_persistent_data ~scope ?secure ()
  | _ -> Lwt.return_unit

let discard_all_services ~scope ?secure () =
  let sitedata = Request_info.find_sitedata "close_all_service_sessions" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  Mod_sessadmin.close_all_service_states ~scope ~secure sitedata
(*VVV missing: scope group *)

let discard_all ~scope ?secure () =
  let* () = discard_all_services ~scope ?secure () in
  discard_all_data ~scope ?secure ()

let discard_everything () =
  let discard_name scope_hierarchy =
    let* () = discard_all ~scope:(`Session_group scope_hierarchy) () in
    let* () = discard_all ~scope:(`Session scope_hierarchy) () in
    discard_all ~scope:(`Client_process scope_hierarchy) ()
  in
  let* () = discard_request_data () in
  Lwt_list.iter_p discard_name (Common.list_scope_hierarchies ())

(*****************************************************************************)
(* Administration *)

module Ext = struct
  (** Type used to describe session timeouts *)

  type timeout = Common.timeout =
    | TGlobal  (** see global setting *)
    | TNone  (** explicitly set no timeout *)
    | TSome of float  (** timeout duration in seconds *)

  type (+'a (* scope *), +'b (* `Data, `Service or `Pers *)) state =
    { state_scope : Common.user_scope
    ; state_kind : [`Data | `Service | `Pers]
    ; state_id : string  (** The cookie value or the group name *) }

  type service_cookie_info =
    string (* cookie value *) * Common.tables Common.Service_cookie.t

  type data_cookie_info = string (* cookie value *) * Common.Data_cookie.t
  type persistent_cookie_info = string (* cookie value *) * Mod_cookies.cookie

  (* The same state with other phantom types *)
  let untype_state state = {state with state_id = state.state_id}

  let volatile_data_group_state ?(scope = Common.default_group_scope) group_name
    =
    { state_scope = (scope :> Common.user_scope)
    ; state_kind = `Data
    ; state_id = group_name }

  let persistent_data_group_state
        ?(scope = Common.default_group_scope)
        group_name
    =
    { state_scope = (scope :> Common.user_scope)
    ; state_kind = `Pers
    ; state_id = group_name }

  let service_group_state ?(scope = Common.default_group_scope) group_name =
    { state_scope = (scope :> Common.user_scope)
    ; state_kind = `Service
    ; state_id = group_name }

  let current_volatile_data_state
        ?secure
        ?(scope = (Common.default_session_scope :> Common.user_scope))
        ()
    =
    let scope = (scope :> Common.user_scope) in
    match scope with
    | `Session_group h -> (
      match get_volatile_data_session_group ~scope:(`Session h) ?secure () with
      | Some g -> volatile_data_group_state ~scope:(`Session_group h) g
      | None -> raise Not_found)
    | #Common.cookie_scope as cookie_scope ->
        let cookie =
          Mod_datasess.find_or_create_data_cookie ~secure_o:secure ~cookie_scope
            ()
        in
        ({ state_scope = scope
         ; state_kind = `Data
         ; state_id = Common.(Hashed_cookies.to_string cookie.dc_hvalue) }
         : ('a, 'b) state)

  let current_persistent_data_state
        ?secure
        ?(scope = (Common.default_session_scope :> Common.user_scope))
        ()
    =
    let scope = (scope :> Common.user_scope) in
    match scope with
    | `Session_group h -> (
        let* group =
          get_persistent_data_session_group ~scope:(`Session h) ?secure ()
        in
        match group with
        | Some g ->
            Lwt.return (persistent_data_group_state ~scope:(`Session_group h) g)
        | None -> Lwt.fail Not_found)
    | #Common.cookie_scope as cookie_scope ->
        let* cookie =
          Mod_persess.find_or_create_persistent_cookie ~secure_o:secure
            ~cookie_scope ()
        in
        Lwt.return
          { state_scope = scope
          ; state_kind = `Pers
          ; state_id = Common.(Hashed_cookies.to_string cookie.pc_hvalue) }

  let current_service_state
        ?secure
        ?(scope = (Common.default_session_scope :> Common.user_scope))
        ()
    =
    let scope = (scope :> Common.user_scope) in
    match scope with
    | `Session_group h -> (
      match get_service_session_group ~scope:(`Session h) ?secure () with
      | Some g -> service_group_state ~scope:(`Session_group h) g
      | None -> raise Not_found)
    | #Common.cookie_scope as cookie_scope ->
        let cookie =
          Mod_sersess.find_or_create_service_cookie ~secure_o:secure
            ~cookie_scope ()
        in
        { state_scope = scope
        ; state_kind = `Service
        ; state_id = Common.(Hashed_cookies.to_string cookie.sc_hvalue) }

  let get_service_cookie_info
        ?(sitedata = Request_info.find_sitedata "State.get_service_cookie_info")
        ({state_id = cookie; _} : ([< Common.cookie_level], [< `Service]) state)
    =
    cookie, Common.SessionCookies.find sitedata.Common.session_services cookie

  let get_volatile_data_cookie_info
        ?(sitedata =
          Request_info.find_sitedata "State.get_volatile_data_cookie_info")
        ({state_id = cookie; _} : ([< Common.cookie_level], [< `Data]) state)
    =
    cookie, Common.SessionCookies.find sitedata.Common.session_data cookie

  let get_persistent_cookie_info
        ({state_id = cookie; _} : ([< Common.cookie_level], [< `Pers]) state)
    =
    let* v = Mod_cookies.Persistent_cookies.Cookies.find cookie in
    Lwt.return (cookie, v)

  let discard_state
        ?(sitedata = Request_info.find_sitedata "State.discard_state")
        ~state
        ()
    =
    let make_sessgrp n =
      { Common.sg_site_dir = Common.get_site_dir_string sitedata
      ; sg_level = `Session
      ; sg_group = Common.Group_name n }
    in
    match state with
    | {state_scope = `Session_group _; state_kind = `Data; state_id = group_name}
      ->
        (match
           Mod_sessiongroups.Data.find_node_in_group_of_groups
             (make_sessgrp group_name)
         with
        | Some node -> Mod_sessiongroups.Data.remove node
        | None -> ());
        Lwt.return_unit
    | { state_scope = `Session_group _
      ; state_kind = `Service
      ; state_id = group_name } ->
        (match
           Mod_sessiongroups.Serv.find_node_in_group_of_groups
             (make_sessgrp group_name)
         with
        | Some (_, node) -> Mod_sessiongroups.Serv.remove node
        | None -> ());
        Lwt.return_unit
    | {state_scope = `Session_group _; state_kind = `Pers; state_id = group_name}
      ->
        let sgr_o =
          Common.make_persistent_full_group_name ~cookie_level:`Session
            (Common.get_site_dir_string sitedata)
            (Some group_name)
        in
        Mod_sessiongroups.Pers.remove_group ~cookie_level:`Session sitedata
          sgr_o
    | {state_kind = `Service; _} ->
        let () =
          match
            get_service_cookie_info ~sitedata {state with state_kind = `Service}
          with
          | exception Not_found -> ()
          | _, {Common.Service_cookie.session_group_node; _} ->
              Mod_sessiongroups.Serv.remove session_group_node
        in
        Lwt.return_unit
    | {state_kind = `Data; _} ->
        let () =
          match
            get_volatile_data_cookie_info ~sitedata
              {state with state_kind = `Data}
          with
          | exception Not_found -> ()
          | _, {Common.Data_cookie.session_group_node; _} ->
              Mod_sessiongroups.Data.remove session_group_node
        in
        Lwt.return_unit
    | {state_kind = `Pers; _} ->
        Lwt.try_bind
          (fun () -> get_persistent_cookie_info {state with state_kind = `Pers})
          (function
            | cookie, {Mod_cookies.full_state_name; session_group; _} ->
                let scope = full_state_name.Common.user_scope in
                let cookie_level = Common.cookie_level_of_user_scope scope in
                Mod_sessiongroups.Pers.close_persistent_session ~cookie_level
                  sitedata session_group cookie)
          (function Not_found -> Lwt.return_unit | exc -> Lwt.fail exc)
  (*VVV!!! Is session_group a full session group name (fullsessgrp)? *)

  let prepare_sub_states_fold
        ?(sitedata = Request_info.find_sitedata "State (state iterator)")
        ~state:
          ({state_scope = s; state_kind = k; state_id = id} :
            ([< `Session_group | `Session], 'k) state)
        f
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
    let f a v =
      f a
        ({state_scope = sub_states_scope; state_kind = k; state_id = v}
         : (_, 'k) state)
    in
    sitedata, sub_states_level, id, f

  let fold_sub_states_aux fold return (sitedata, sub_states_level, id, f) e
    = function
    | {state_kind = `Data; _} -> (
      try
        let dl =
          Mod_sessiongroups.Data.find
            { Common.sg_site_dir = Common.get_site_dir_string sitedata
            ; sg_level = sub_states_level
            ; sg_group = Common.Group_name id }
        in
        fold f e dl
      with Not_found -> return e)
    | {state_kind = `Service; _} -> (
      try
        let dl =
          Mod_sessiongroups.Serv.find
            { Common.sg_site_dir = Common.get_site_dir_string sitedata
            ; sg_level = sub_states_level
            ; sg_group = Common.Group_name id }
        in
        fold f e dl
      with Not_found -> return e)
    | _ -> failwith "fold_sub_states_aux"

  let fold_volatile_sub_states ?sitedata ~state f e =
    let a = prepare_sub_states_fold ?sitedata ~state f in
    fold_sub_states_aux Ocsigen_base.Cache.Dlist.fold Fun.id a e state

  (** Fold over the snapshot of a Dlist. *)
  let dlist_lwt_fold f acc dlist =
    Ocsigen_base.Cache.Dlist.fold (fun acc x -> x :: acc) [] dlist
    |> List.rev |> Lwt_list.fold_left_s f acc

  let fold_sub_states ?sitedata ~state f e =
    let ((sitedata, sub_states_level, id, f) as a) =
      prepare_sub_states_fold ?sitedata ~state f
    in
    match state with
    | {state_kind = `Pers; _} ->
        let* l =
          Mod_sessiongroups.Pers.find
            (Common.make_persistent_full_group_name
               ~cookie_level:sub_states_level
               (Common.get_site_dir_string sitedata)
               (Some id))
        in
        Lwt_list.fold_left_s f e l
    | _ -> fold_sub_states_aux dlist_lwt_fold Lwt.return a e state

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

    let lwt_check_scopes a b = Lwt.wrap2 check_scopes a b

    (*VVV Does not work with volatile group data *)
    let get_volatile_data
          ~state:({state_scope; state_id = cookie; _} : ('s, [< `Data]) state)
          ~table:({Common.table_scope; table = t; _} : 'a volatile_table)
      =
      check_scopes table_scope state_scope;
      Common.SessionCookies.find t cookie

    let get_persistent_data
          (type a)
          ~state:({state_scope; state_id = cookie; _} : ('s, [< `Pers]) state)
          ~table:({Common.table_scope; table = t; _} : a persistent_table)
      =
      let* () = lwt_check_scopes table_scope state_scope in
      let module T =
        (val t
          : Common.Ocsipersist.TABLE with type key = string and type value = a)
      in
      T.find cookie

    let set_volatile_data
          ~state:({state_scope; state_id = cookie; _} : ('s, [< `Data]) state)
          ~table:({Common.table_scope; table = t; _} : 'a volatile_table)
          value
      =
      check_scopes table_scope state_scope;
      Common.SessionCookies.replace t cookie value

    let set_persistent_data
          (type a)
          ~state:({state_scope; state_id = cookie; _} : ('s, [< `Pers]) state)
          ~table:({Common.table_scope; table = t; _} : a persistent_table)
          (value : a)
      =
      let* () = lwt_check_scopes table_scope state_scope in
      let module T =
        (val t
          : Common.Ocsipersist.TABLE with type key = string and type value = a)
      in
      T.add cookie value

    let remove_volatile_data
          ~state:({state_scope; state_id = cookie; _} : ('s, [< `Data]) state)
          ~table:({Common.table_scope; table = t; _} : 'a volatile_table)
      =
      check_scopes table_scope state_scope;
      Common.SessionCookies.remove t cookie

    let remove_persistent_data
          (type a)
          ~state:({state_scope; state_id = cookie; _} : ('s, [< `Pers]) state)
          ~table:({Common.table_scope; table = t; _} : a persistent_table)
      =
      let* () = lwt_check_scopes table_scope state_scope in
      let module T =
        (val t
          : Common.Ocsipersist.TABLE with type key = string and type value = a)
      in
      T.remove cookie
  end

  let get_service_cookie_scope ~cookie:(_, cookie) =
    cookie.Common.Service_cookie.full_state_name.Common.user_scope

  let get_volatile_data_cookie_scope ~cookie:(_, data_cookie) =
    data_cookie.Common.Data_cookie.full_state_name.Common.user_scope

  let get_persistent_data_cookie_scope ~cookie:(_, cookie) =
    cookie.Mod_cookies.full_state_name.Common.user_scope

  let set_service_cookie_timeout ~cookie:(_, cookie) t =
    cookie.Common.Service_cookie.timeout := Common.timeout_of_option t

  let set_volatile_data_cookie_timeout ~cookie:(_, data_cookie) t =
    data_cookie.Common.Data_cookie.timeout := Common.timeout_of_option t

  let set_persistent_data_cookie_timeout ~cookie:(c, _) t =
    Mod_cookies.Persistent_cookies.set_timeout c (Common.timeout_of_option t)

  let get_service_cookie_timeout ~cookie:(_, cookie) =
    !(cookie.Common.Service_cookie.timeout)

  let get_volatile_data_cookie_timeout ~cookie:(_, data_cookie) =
    !(data_cookie.Common.Data_cookie.timeout)

  let get_persistent_data_cookie_timeout ~cookie:(_, cookie) =
    cookie.Mod_cookies.timeout

  let unset_service_cookie_timeout ~cookie:(_, cookie) =
    cookie.Common.Service_cookie.timeout := TGlobal

  let unset_volatile_data_cookie_timeout ~cookie:(_cookie, data_cookie) =
    data_cookie.Common.Data_cookie.timeout := TGlobal

  let unset_persistent_data_cookie_timeout ~cookie:(c, _) =
    Mod_cookies.Persistent_cookies.set_timeout c TGlobal

  let get_session_group_list () =
    let sitedata = Request_info.find_sitedata "get_session_group_list" in
    let dl = sitedata.Common.group_of_groups in
    (* A group with service and data sessions has an entry for each kind *)
    List.sort_uniq String.compare
      (Ocsigen_base.Cache.Dlist.fold
         (fun l {Common.gg_name; _} -> gg_name :: l)
         [] dl)

  (** Iterator on service cookies *)
  let iter_service_cookies = Mod_sessexpl.iter_service_cookies

  (** Iterator on data cookies *)
  let iter_volatile_data_cookies = Mod_sessexpl.iter_data_cookies

  (** Iterator on persistent cookies *)
  let iter_persistent_data_cookies = Mod_sessexpl.iter_persistent_cookies

  (** Iterator on service cookies *)
  let fold_service_cookies = Mod_sessexpl.fold_service_cookies

  (** Iterator on data cookies *)
  let fold_volatile_data_cookies = Mod_sessexpl.fold_data_cookies

  (** Iterator on persistent cookies *)
  let fold_persistent_data_cookies = Mod_sessexpl.fold_persistent_cookies
end

(*****************************************************************************)
(* Exploration *)

let number_of_service_cookies = Mod_sessexpl.number_of_service_cookies
let number_of_volatile_data_cookies = Mod_sessexpl.number_of_data_cookies
let number_of_tables = Mod_sessexpl.number_of_tables
let number_of_table_elements = Mod_sessexpl.number_of_table_elements

let number_of_persistent_data_cookies =
  Mod_sessexpl.number_of_persistent_cookies

let number_of_persistent_tables = Common.Persistent_tables.number_of_tables

let number_of_persistent_table_elements =
  Common.Persistent_tables.number_of_table_elements

(*****************************************************************************)
let get_service_cookie ~cookie_scope ?secure () =
  try
    let c =
      Mod_sersess.find_service_cookie_only ~cookie_scope ~secure_o:secure ()
    in
    Some c.Common.sc_hvalue
  with Not_found | Common.Eliom_Session_expired -> None

let get_volatile_data_cookie ~cookie_scope ?secure () =
  try
    let c =
      Mod_datasess.find_data_cookie_only ~cookie_scope ~secure_o:secure ()
    in
    Some c.Common.dc_hvalue
  with Not_found | Common.Eliom_Session_expired -> None

let get_persistent_data_cookie ~cookie_scope ?secure () =
  Lwt.catch
    (fun () ->
       let* c =
         Mod_persess.find_persistent_cookie_only ~cookie_scope ~secure_o:secure
           ()
       in
       Lwt.return_some c.Common.pc_hvalue)
    (function
      | Not_found | Common.Eliom_Session_expired -> Lwt.return_none
      | exc -> Lwt.fail exc)

(*****************************************************************************)
(** {2 User cookies} *)

let change_pathopt_ sp = function
  | None ->
      Common.get_site_dir (Request_info.get_sitedata_sp ~sp)
      (* Not possible to set a cookie for another site (?) *)
  | Some p -> Common.get_site_dir (Request_info.get_sitedata_sp ~sp) @ p

let set_cookie ?(cookie_level = `Session) ?path ?exp ?secure ~name ~value () =
  let sp = Common.get_sp () in
  let path = change_pathopt_ sp path in
  let sitedata = Request_info.find_sitedata "set_cookie" in
  let secure = Common.get_secure ~secure_o:secure ~sitedata in
  match cookie_level with
  | `Session ->
      sp.Common.sp_user_cookies <-
        Ocsigen_cookie_map.add ~path name
          (OSet (exp, value, secure))
          sp.Common.sp_user_cookies
  | `Client_process ->
      sp.Common.sp_user_tab_cookies <-
        Ocsigen_cookie_map.add ~path name
          (OSet (exp, value, secure))
          sp.Common.sp_user_tab_cookies

let unset_cookie ?(cookie_level = `Session) ?path ~name () =
  let sp = Common.get_sp () in
  let path = change_pathopt_ sp path in
  match cookie_level with
  | `Session ->
      sp.Common.sp_user_cookies <-
        Ocsigen_cookie_map.add ~path name OUnset sp.Common.sp_user_cookies
  | `Client_process ->
      sp.Common.sp_user_tab_cookies <-
        Ocsigen_cookie_map.add ~path name OUnset sp.Common.sp_user_tab_cookies
