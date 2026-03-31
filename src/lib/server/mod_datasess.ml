(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_datasess.ml
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
(*****************************************************************************)
(*****************************************************************************)
(** Internal functions used by Eliom:                                        *)

(** Volatile data tables                                                     *)

(*****************************************************************************)
(*****************************************************************************)

open Lib

let compute_cookie_info sitedata secure_o secure_ci cookie_info =
  let secure = Common.get_secure ~secure_o ~sitedata () in
  if secure
  then
    let _, c, _ = secure_ci in
    c, true
  else cookie_info, false

(* to be called during a request *)
let close_data_state ~scope ~secure_o ?sp () =
  let sp = Common.sp_of_option sp in
  try
    let cookie_level = Common.cookie_level_of_user_scope scope in
    let (_, cookie_info, _), secure_ci =
      Common.get_cookie_info sp cookie_level
    in
    let sitedata = Request_info.get_sitedata_sp ~sp in
    let cookie_info, secure =
      compute_cookie_info sitedata secure_o secure_ci cookie_info
    in
    let full_st_name = Common.make_full_state_name ~sp ~secure ~scope in
    let _, ior =
      Lazy.force (Common.Full_state_name_table.find full_st_name !cookie_info)
    in
    match !ior with
    | Common.SC c ->
        (* There is only one way to close a session:
           remove it from the session group table.
           It will remove all the data table entries
           and also the entry in the session table *)
        (match scope with
        | `Session_group _ -> (
          (* If we want to close all the group of browser sessions,
                   the node is found in the group table: *)
          match
            Mod_sessiongroups.Data.find_node_in_group_of_groups
              !(c.Common.dc_session_group)
          with
          | None ->
              Logs.err ~src:eliom_logs_src (fun fmt ->
                fmt "No group of groups. Please report this problem.")
          | Some g -> Mod_sessiongroups.Data.remove g)
        | `Session _ | `Client_process _ ->
            (* If we want to close a (tab/browser) session, the node is found
                 in the cookie info: *)
            Mod_sessiongroups.Data.remove c.Common.dc_session_group_node);
        ior := Common.SCNo_data
    | _ -> ()
  with Not_found -> ()

let fullsessgrp ~cookie_level ~sp set_session_group =
  Mod_sessiongroups.make_full_group_name ~cookie_level
    sp.Common.sp_request.Ocsigen_extensions.request_info
    (Common.get_site_dir_string sp.Common.sp_sitedata)
    (Common.get_mask4 sp.Common.sp_sitedata)
    (Common.get_mask6 sp.Common.sp_sitedata)
    set_session_group

let rec find_or_create_data_cookie
          ?set_session_group
          ~(cookie_scope : Common.cookie_scope)
          ~secure_o
          ?sp
          ()
  =
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let cookie_level = Common.cookie_level_of_user_scope cookie_scope in
  let sp = Common.sp_of_option sp in
  let new_data_cookie sitedata full_st_name table =
    let set_session_group =
      match cookie_scope with
      | `Client_process n ->
          (* We create a group whose name is the
                   browser session cookie
                   and put the tab session into it. *)
          let v =
            find_or_create_data_cookie ~cookie_scope:(`Session n) ~secure_o ~sp
              ()
          in
          Some Common.(Hashed_cookies.to_string v.dc_hvalue)
      | _ -> set_session_group
    in
    let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
    let c = Mod_cookies.make_new_session_id () in
    let hc = Common.Hashed_cookies.hash c in
    let hc_string = Common.Hashed_cookies.to_string hc in
    let usertimeout =
      ref Common.TGlobal
      (* See global table *)
    in
    let serverexp =
      ref None
      (* Some 0. *)
      (* None = never. We'll change it later. *)
    in
    let fullsessgrpref = ref fullsessgrp in
    let node = Mod_sessiongroups.Data.add sitedata hc_string fullsessgrp in
    Common.SessionCookies.replace
      (* actually it will add the cookie *)
      table hc_string
      { Common.Data_cookie.full_state_name = full_st_name
      ; expiry = serverexp
      ; timeout = usertimeout
      ; session_group = fullsessgrpref
      ; session_group_node = node };
    { Common.dc_hvalue = hc
    ; Common.dc_set_value = Some c
    ; Common.dc_timeout = usertimeout
    ; Common.dc_exp = serverexp
    ; Common.dc_cookie_exp = ref (Common.default_client_cookie_exp ())
    ; Common.dc_session_group = fullsessgrpref
    ; Common.dc_session_group_node = node }
  in
  let (_, cookie_info, _), secure_ci = Common.get_cookie_info sp cookie_level in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Common.make_full_state_name ~sp ~secure ~scope:cookie_scope
  in
  try
    let _old, ior =
      Lazy.force (Common.Full_state_name_table.find full_st_name !cookie_info)
    in
    match !ior with
    | Common.SCData_session_expired
      (* We do not trust the value sent by the client,
           for security reasons *)
    | Common.SCNo_data ->
        let v =
          new_data_cookie sitedata full_st_name sitedata.Common.session_data
        in
        ior := Common.SC v;
        v
    | Common.SC c ->
        (match set_session_group with
        | None -> ()
        | Some _session_group ->
            let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
            let node =
              Mod_sessiongroups.Data.move sitedata
                c.Common.dc_session_group_node fullsessgrp
            in
            c.Common.dc_session_group_node <- node;
            c.Common.dc_session_group := fullsessgrp);
        c
  with Not_found ->
    let v =
      new_data_cookie sitedata full_st_name sitedata.Common.session_data
    in
    cookie_info :=
      Common.Full_state_name_table.add full_st_name
        (Lazy.from_val (None, ref (Common.SC v)))
        !cookie_info;
    v

let find_or_create_data_cookie =
  (find_or_create_data_cookie
    : ?set_session_group:string
      -> cookie_scope:Common.cookie_scope
      -> secure_o:bool option
      -> ?sp:Common.server_params
      -> unit
      -> Common.one_data_cookie_info
    :> ?set_session_group:string
       -> cookie_scope:[< Common.cookie_scope]
       -> secure_o:bool option
       -> ?sp:Common.server_params
       -> unit
       -> Common.one_data_cookie_info)

let find_data_cookie_only ~cookie_scope ~secure_o ?sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let sp = Common.sp_of_option sp in
  let cookie_level = Common.cookie_level_of_user_scope cookie_scope in
  let (_, cookie_info, _), secure_ci = Common.get_cookie_info sp cookie_level in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Common.make_full_state_name ~sp ~secure ~scope:cookie_scope
  in
  let _, ior =
    Lazy.force (Common.Full_state_name_table.find full_st_name !cookie_info)
  in
  match !ior with
  | Common.SCNo_data -> raise Not_found
  | Common.SCData_session_expired -> raise Common.Eliom_Session_expired
  | Common.SC v -> v

(*****************************************************************************)
(** session data *)

let counttableelements = ref []
(* Here only for exploration functions *)

let create_volatile_table, create_volatile_table_during_session =
  let aux ~scope ~secure sitedata =
    let t = Common.SessionCookies.create 100 in
    let old_remove_session_data = sitedata.Common.remove_session_data in
    sitedata.Common.remove_session_data <-
      (fun cookie ->
        (* cookie is actually either a cookie or a a group name *)
        (* In session group tables, keys may be either group names,
            or a cookie values when no group name has been set. *)
        old_remove_session_data cookie;
        Common.SessionCookies.remove t cookie);
    let old_not_bound_in_data_tables =
      sitedata.Common.not_bound_in_data_tables
    in
    sitedata.Common.not_bound_in_data_tables <-
      (fun cookie ->
        old_not_bound_in_data_tables cookie
        && not (Common.SessionCookies.mem t cookie));
    counttableelements :=
      (fun () -> Common.SessionCookies.length t) :: !counttableelements;
    scope, secure, t
  in
  ( (fun ~scope ~secure ->
      let sitedata = Common.get_current_sitedata () in
      aux ~scope ~secure sitedata)
  , fun ~scope ~secure sitedata -> aux ~scope ~secure sitedata )
