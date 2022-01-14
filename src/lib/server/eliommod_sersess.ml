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

open Eliom_lib

let compute_cookie_info sitedata secure_o secure_ci cookie_info =
  let secure = Eliom_common.get_secure ~secure_o ~sitedata () in
  if secure
  then let (c, _, _) = secure_ci in c, true
  else cookie_info, false

(*****************************************************************************)
let close_service_state ~scope ~secure_o ?sp () =
  let sp = Eliom_common.sp_of_option sp in
  try
    let cookie_level = Eliom_common.cookie_level_of_user_scope scope in
    let ((cookie_info, _, _), secure_ci) =
      Eliom_common.get_cookie_info sp cookie_level
    in
    let sitedata = Eliom_request_info.get_sitedata_sp sp in
    let cookie_info, secure =
      compute_cookie_info sitedata secure_o secure_ci cookie_info
    in
    let full_st_name = Eliom_common.make_full_state_name ~sp ~secure ~scope in
    let (_, ior) =
      Eliom_common.Full_state_name_table.find full_st_name !cookie_info
    in

    match !ior with
      | Eliom_common.SC c ->
          (* there is only one way to close a session:
             remove it from the session group table.
             It will remove the entry in the session table *)
        begin
          match scope with
            | `Session_group _ ->
              begin
                (* If we want to close all the group of browser sessions,
                   the node is found in the group table: *)
                match
                  Eliommod_sessiongroups.Serv.find_node_in_group_of_groups
                    !(c.Eliom_common.sc_session_group)
                with
                  | None -> Lwt_log.ign_error ~section:Lwt_log.eliom "No group of groups. Please report this problem."
                  | Some (_service_table, g) ->
                    Eliommod_sessiongroups.Serv.remove g
              end
            | `Session _
            | `Client_process _ ->
              Eliommod_sessiongroups.Serv.remove
                c.Eliom_common.sc_session_group_node
        end;
        ior := Eliom_common.SCNo_data
      | _ -> ()

  with Not_found -> ()



let fullsessgrp ~cookie_level ~sp set_session_group =
  let sitedata = Eliom_request_info.get_sitedata_sp sp in
  Eliommod_sessiongroups.make_full_group_name
    ~cookie_level
    (Eliom_request_info.get_request_sp sp).Ocsigen_extensions.request_info
    sitedata.Eliom_common.site_dir_string
    (Eliom_common.get_mask4 sitedata)
    (Eliom_common.get_mask6 sitedata)
    set_session_group

let rec find_or_create_service_cookie_ ?set_session_group
    ~(cookie_scope: Eliom_common.cookie_scope) ~secure_o ~sp () =
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)

  let cookie_level = Eliom_common.cookie_level_of_user_scope cookie_scope in

  let new_service_cookie sitedata full_state_name table =
    let set_session_group =
      match cookie_scope with
        | `Client_process n ->
          begin (* We create a group whose name is the
                   browser session cookie
                   and put the tab session into it. *)
            let v = find_or_create_service_cookie_
              ~cookie_scope:(`Session n)
              ~secure_o
              ~sp
              ()
            in
            Some Eliom_common.(Hashed_cookies.to_string v.sc_hvalue)
          end
        |  _ -> set_session_group
    in
    let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
    let c = Eliommod_cookies.make_new_session_id () in
    let hc = Eliom_common.Hashed_cookies.hash c in
    let hc_string = Eliom_common.Hashed_cookies.to_string hc in
    let str = ref (Eliom_common.new_service_session_tables sitedata) in
    let timeout = ref Eliom_common.TGlobal (* See global table *) in
    let expiry = ref None (*Some 0.*) (* None = never. We'll change it later. *) in
    let session_group = ref fullsessgrp in
    let session_group_node =
      Eliommod_sessiongroups.Serv.add sitedata hc_string fullsessgrp
    in
    Eliom_common.SessionCookies.replace
      (* actually it will add the cookie *)
      table
      hc_string
      {Eliom_common.Service_cookie.full_state_name; session_table = !str;
       expiry; timeout = timeout; session_group; session_group_node};
    {Eliom_common.sc_hvalue= hc;
     Eliom_common.sc_set_value= Some c;
     Eliom_common.sc_table= str;
     Eliom_common.sc_timeout= timeout;
     Eliom_common.sc_exp= expiry;
     Eliom_common.sc_cookie_exp=
       ref (Eliom_common.default_client_cookie_exp ());
     Eliom_common.sc_session_group= session_group;
     Eliom_common.sc_session_group_node= session_group_node}
  in

  let ((cookie_info, _, _), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_level
  in
  let sitedata = Eliom_request_info.get_sitedata_sp sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Eliom_common.make_full_state_name ~sp ~secure ~scope:cookie_scope in

  try

    let (_old, ior) =
      Eliom_common.Full_state_name_table.find full_st_name !cookie_info
    in
    match !ior with
    | Eliom_common.SCData_session_expired
        (* We do not trust the value sent by the client,
           for security reasons *)
    | Eliom_common.SCNo_data ->
      let v =
        new_service_cookie
          sitedata full_st_name
          sitedata.Eliom_common.session_services
      in
      ior := Eliom_common.SC v;
      v
    | Eliom_common.SC c ->
      (match set_session_group with
        | None -> ()
        | Some _session_group ->
          let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
          let node = Eliommod_sessiongroups.Serv.move
            sitedata
            c.Eliom_common.sc_session_group_node fullsessgrp
          in
          c.Eliom_common.sc_session_group_node <- node;
          c.Eliom_common.sc_session_group := fullsessgrp
      );
      c
  with Not_found ->
    let v =
      new_service_cookie
        sitedata full_st_name
        sitedata.Eliom_common.session_services
    in
    cookie_info :=
      Eliom_common.Full_state_name_table.add
        full_st_name
        (None, ref (Eliom_common.SC v))
        !cookie_info;
    v


let find_or_create_service_cookie_ =
  (find_or_create_service_cookie_ :
         ?set_session_group:string ->
         cookie_scope:Eliom_common.cookie_scope ->
         secure_o:bool option ->
         sp:Eliom_common.server_params ->
         unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
:>
         ?set_session_group:string ->
    cookie_scope:[< Eliom_common.cookie_scope] ->
         secure_o:bool option ->
         sp:Eliom_common.server_params ->
         unit -> Eliom_common.tables Eliom_common.one_service_cookie_info
  )

let find_or_create_service_cookie ?set_session_group
    ~cookie_scope ~secure_o ?sp () =
  let sp = Eliom_common.sp_of_option sp in
  find_or_create_service_cookie_ ?set_session_group
    ~cookie_scope ~secure_o ~sp ()


let find_service_cookie_only ~cookie_scope ~secure_o ?sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let sp = Eliom_common.sp_of_option sp in
  let ((cookie_info, _, _), secure_ci) =
    Eliom_common.get_cookie_info sp
      (Eliom_common.cookie_level_of_user_scope cookie_scope)
  in
  let sitedata = Eliom_request_info.get_sitedata_sp sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Eliom_common.make_full_state_name ~sp ~secure ~scope:cookie_scope in
  let (_, ior) =
    Eliom_common.Full_state_name_table.find full_st_name !cookie_info
  in
  match !ior with
  | Eliom_common.SCNo_data -> raise Not_found
  | Eliom_common.SCData_session_expired ->
      raise Eliom_common.Eliom_Session_expired
  | Eliom_common.SC v -> v
