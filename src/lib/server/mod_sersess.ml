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
(*****************************************************************************)
(*****************************************************************************)
(** Internal functions used by Eliom:                                        *)

(** Service sessions                                                         *)

(*****************************************************************************)
(*****************************************************************************)

open Lib

let compute_cookie_info sitedata secure_o secure_ci cookie_info =
  let secure = Common.get_secure ~secure_o ~sitedata () in
  if secure
  then
    let c, _, _ = secure_ci in
    c, true
  else cookie_info, false

(*****************************************************************************)
let close_service_state ~scope ~secure_o ?sp () =
  let sp = Common.sp_of_option sp in
  try
    let cookie_level = Common.cookie_level_of_user_scope scope in
    let (cookie_info, _, _), secure_ci =
      Common.get_cookie_info sp cookie_level
    in
    let sitedata = Request_info.get_sitedata_sp ~sp in
    let cookie_info, secure =
      compute_cookie_info sitedata secure_o secure_ci cookie_info
    in
    let full_st_name = Common.make_full_state_name ~sp ~secure ~scope in
    let _, ior = Common.Full_state_name_table.find full_st_name !cookie_info in
    match !ior with
    | Common.SC c ->
        (* there is only one way to close a session:
             remove it from the session group table.
             It will remove the entry in the session table *)
        (match scope with
        | `Session_group _ -> (
          (* If we want to close all the group of browser sessions,
                   the node is found in the group table: *)
          match
            Mod_sessiongroups.Serv.find_node_in_group_of_groups
              !(c.Common.sc_session_group)
          with
          | None ->
              Logs.err ~src:eliom_logs_src (fun fmt ->
                fmt "No group of groups. Please report this problem.")
          | Some (_service_table, g) -> Mod_sessiongroups.Serv.remove g)
        | `Session _ | `Client_process _ ->
            Mod_sessiongroups.Serv.remove c.Common.sc_session_group_node);
        ior := Common.SCNo_data
    | _ -> ()
  with Not_found -> ()

let fullsessgrp ~cookie_level ~sp set_session_group =
  let sitedata = Request_info.get_sitedata_sp ~sp in
  Mod_sessiongroups.make_full_group_name ~cookie_level
    (Request_info.get_request_sp sp).Ocsigen_extensions.request_info
    (Common.get_site_dir_string sitedata)
    (Common.get_mask4 sitedata)
    (Common.get_mask6 sitedata)
    set_session_group

let rec find_or_create_service_cookie_
          ?set_session_group
          ~(cookie_scope : Common.cookie_scope)
          ~secure_o
          ~sp
          ()
  =
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let cookie_level = Common.cookie_level_of_user_scope cookie_scope in
  let new_service_cookie sitedata full_state_name table =
    let set_session_group =
      match cookie_scope with
      | `Client_process n ->
          (* We create a group whose name is the
                   browser session cookie
                   and put the tab session into it. *)
          let v =
            find_or_create_service_cookie_ ~cookie_scope:(`Session n) ~secure_o
              ~sp ()
          in
          Some Common.(Hashed_cookies.to_string v.sc_hvalue)
      | _ -> set_session_group
    in
    let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
    let c = Mod_cookies.make_new_session_id () in
    let hc = Common.Hashed_cookies.hash c in
    let hc_string = Common.Hashed_cookies.to_string hc in
    let str = ref (Common.new_service_session_tables sitedata) in
    let timeout =
      ref Common.TGlobal
      (* See global table *)
    in
    let expiry =
      ref None
      (*Some 0.*)
      (* None = never. We'll change it later. *)
    in
    let session_group = ref fullsessgrp in
    let session_group_node =
      Mod_sessiongroups.Serv.add sitedata hc_string fullsessgrp
    in
    Common.SessionCookies.replace
      (* actually it will add the cookie *)
      table hc_string
      { Common.Service_cookie.full_state_name
      ; session_table = !str
      ; expiry
      ; timeout
      ; session_group
      ; session_group_node };
    { Common.sc_hvalue = hc
    ; Common.sc_set_value = Some c
    ; Common.sc_table = str
    ; Common.sc_timeout = timeout
    ; Common.sc_exp = expiry
    ; Common.sc_cookie_exp = ref (Common.default_client_cookie_exp ())
    ; Common.sc_session_group = session_group
    ; Common.sc_session_group_node = session_group_node }
  in
  let (cookie_info, _, _), secure_ci = Common.get_cookie_info sp cookie_level in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Common.make_full_state_name ~sp ~secure ~scope:cookie_scope
  in
  try
    let _old, ior =
      Common.Full_state_name_table.find full_st_name !cookie_info
    in
    match !ior with
    | Common.SCData_session_expired
      (* We do not trust the value sent by the client,
           for security reasons *)
    | Common.SCNo_data ->
        let v =
          new_service_cookie sitedata full_st_name
            sitedata.Common.session_services
        in
        ior := Common.SC v;
        v
    | Common.SC c ->
        (match set_session_group with
        | None -> ()
        | Some _session_group ->
            let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
            let node =
              Mod_sessiongroups.Serv.move sitedata
                c.Common.sc_session_group_node fullsessgrp
            in
            c.Common.sc_session_group_node <- node;
            c.Common.sc_session_group := fullsessgrp);
        c
  with Not_found ->
    let v =
      new_service_cookie sitedata full_st_name sitedata.Common.session_services
    in
    cookie_info :=
      Common.Full_state_name_table.add full_st_name
        (None, ref (Common.SC v))
        !cookie_info;
    v

let find_or_create_service_cookie_ =
  (find_or_create_service_cookie_
    : ?set_session_group:string
      -> cookie_scope:Common.cookie_scope
      -> secure_o:bool option
      -> sp:Common.server_params
      -> unit
      -> Common.tables Common.one_service_cookie_info
    :> ?set_session_group:string
       -> cookie_scope:[< Common.cookie_scope]
       -> secure_o:bool option
       -> sp:Common.server_params
       -> unit
       -> Common.tables Common.one_service_cookie_info)

let find_or_create_service_cookie
      ?set_session_group
      ~cookie_scope
      ~secure_o
      ?sp
      ()
  =
  let sp = Common.sp_of_option sp in
  find_or_create_service_cookie_ ?set_session_group ~cookie_scope ~secure_o ~sp
    ()

let find_service_cookie_only ~cookie_scope ~secure_o ?sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let sp = Common.sp_of_option sp in
  let (cookie_info, _, _), secure_ci =
    Common.get_cookie_info sp (Common.cookie_level_of_user_scope cookie_scope)
  in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Common.make_full_state_name ~sp ~secure ~scope:cookie_scope
  in
  let _, ior = Common.Full_state_name_table.find full_st_name !cookie_info in
  match !ior with
  | Common.SCNo_data -> raise Not_found
  | Common.SCData_session_expired -> raise Common.Eliom_Session_expired
  | Common.SC v -> v
