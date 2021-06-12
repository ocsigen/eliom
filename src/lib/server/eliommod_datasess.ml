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

open Eliom_lib

let compute_cookie_info sitedata secure_o secure_ci cookie_info =
  let secure = Eliom_common.get_secure ~secure_o ~sitedata () in
  if secure
  then let (_, c, _) = secure_ci in c, true
  else cookie_info, false


(* to be called during a request *)
let close_data_state ~scope ~secure_o ?sp () =
  let sp = Eliom_common.sp_of_option sp in
  try
    let cookie_level = Eliom_common.cookie_level_of_user_scope scope in
    let ((_, cookie_info, _), secure_ci) =
      Eliom_common.get_cookie_info sp cookie_level
    in
    let sitedata = Eliom_request_info.get_sitedata_sp sp in
    let cookie_info, secure =
      compute_cookie_info sitedata secure_o secure_ci cookie_info
    in
    let full_st_name = Eliom_common.make_full_state_name ~sp ~secure ~scope in
    let (_, ior) =
      Lazy.force
        (Eliom_common.Full_state_name_table.find full_st_name !cookie_info)
    in

    match !ior with
      | Eliom_common.SC c ->
        (* There is only one way to close a session:
           remove it from the session group table.
           It will remove all the data table entries
           and also the entry in the session table *)
        begin
          match scope with
            | `Session_group _ ->
              begin
                (* If we want to close all the group of browser sessions,
                   the node is found in the group table: *)
                match
                  Eliommod_sessiongroups.Data.find_node_in_group_of_groups
                    !(c.Eliom_common.dc_session_group)
                with
                  | None -> Lwt_log.ign_error ~section:Lwt_log.eliom "No group of groups. Please report this problem."
                  | Some g -> Eliommod_sessiongroups.Data.remove g
              end
            | `Session _ | `Client_process _ ->
              (* If we want to close a (tab/browser) session, the node is found
                 in the cookie info: *)
              Eliommod_sessiongroups.Data.remove
                c.Eliom_common.dc_session_group_node
        end;
        ior := Eliom_common.SCNo_data
      | _ -> ()

  with Not_found -> ()


let fullsessgrp ~cookie_level ~sp set_session_group =
  Eliommod_sessiongroups.make_full_group_name
    ~cookie_level
    sp.Eliom_common.sp_request.Ocsigen_extensions.request_info
    sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
    (Eliom_common.get_mask4 sp.Eliom_common.sp_sitedata)
    (Eliom_common.get_mask6 sp.Eliom_common.sp_sitedata)
    set_session_group

let rec find_or_create_data_cookie ?set_session_group
    ~(cookie_scope:  Eliom_common.cookie_scope ) ~secure_o ?sp () =
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)

  let cookie_level = Eliom_common.cookie_level_of_user_scope cookie_scope in

  let sp = Eliom_common.sp_of_option sp in

  let new_data_cookie sitedata full_st_name table =

    let set_session_group =
      match cookie_scope with
        | `Client_process n ->
          begin (* We create a group whose name is the
                   browser session cookie
                   and put the tab session into it. *)
            let v = find_or_create_data_cookie
              ~cookie_scope:(`Session n)
              ~secure_o
              ~sp
              ()
            in
            Some v.Eliom_common.dc_hvalue
          end
        | _ -> set_session_group
    in
    let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in

    let rec aux () =
      let c = Eliommod_cookies.make_new_session_id () in
      (* Just to be sure it is not already used.
         Actually not needed for the cookies we use *)
      if Eliom_common.SessionCookies.mem table c
      then aux ()
      else c
    in
    let c = aux () in
    let usertimeout = ref Eliom_common.TGlobal (* See global table *) in
    let serverexp = ref None (* Some 0. *) (* None = never. We'll change it later. *) in
    let fullsessgrpref = ref fullsessgrp in
    let node = Eliommod_sessiongroups.Data.add sitedata c fullsessgrp in
    Eliom_common.SessionCookies.replace
      (* actually it will add the cookie *)
      table
      c
      (full_st_name,
       serverexp (* exp on server *),
       usertimeout,
       fullsessgrpref,
       node);
    {Eliom_common.dc_hvalue= Eliom_common.hash_cookie c;
     Eliom_common.dc_set_value= Some c;
     Eliom_common.dc_timeout= usertimeout;
     Eliom_common.dc_exp= serverexp;
     Eliom_common.dc_cookie_exp=
       ref (Eliom_common.default_client_cookie_exp ());
     Eliom_common.dc_session_group= fullsessgrpref;
     Eliom_common.dc_session_group_node= node;
    }

  in

  let ((_, cookie_info, _), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_level
  in
  let sitedata = Eliom_request_info.get_sitedata_sp sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Eliom_common.make_full_state_name ~sp ~secure ~scope:cookie_scope in
  try
    let (old, ior) =
      Lazy.force
        (Eliom_common.Full_state_name_table.find full_st_name !cookie_info)
    in
    match !ior with
    | Eliom_common.SCData_session_expired
        (* We do not trust the value sent by the client,
           for security reasons *)
    | Eliom_common.SCNo_data ->
      let v =
        new_data_cookie
          sitedata full_st_name
          sitedata.Eliom_common.session_data
      in
      ior := Eliom_common.SC v;
      v
    | Eliom_common.SC c ->
        (match set_session_group with
          | None -> ()
          | Some session_group ->
            let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
            let node = Eliommod_sessiongroups.Data.move
              sitedata
              c.Eliom_common.dc_session_group_node
              fullsessgrp
            in
            c.Eliom_common.dc_session_group_node <- node;
            c.Eliom_common.dc_session_group := fullsessgrp
        );
        c
  with Not_found ->
    let v =
      new_data_cookie
        sitedata full_st_name
        sitedata.Eliom_common.session_data
    in
    cookie_info :=
      Eliom_common.Full_state_name_table.add
        full_st_name
        (Lazy.from_val (false, ref (Eliom_common.SC v)))
        !cookie_info;
    v

let find_or_create_data_cookie =
  (find_or_create_data_cookie :
     ?set_session_group:string ->
   cookie_scope:Eliom_common.cookie_scope ->
   secure_o:bool option ->
   ?sp:Eliom_common.server_params ->
   unit -> Eliom_common.one_data_cookie_info
   :>
     ?set_session_group:string ->
   cookie_scope:[< Eliom_common.cookie_scope ] ->
   secure_o:bool option ->
   ?sp:Eliom_common.server_params ->
   unit -> Eliom_common.one_data_cookie_info)

let find_data_cookie_only ~cookie_scope ~secure_o ?sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let sp = Eliom_common.sp_of_option sp in
  let cookie_level = Eliom_common.cookie_level_of_user_scope cookie_scope in
  let ((_, cookie_info, _), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_level
  in
  let sitedata = Eliom_request_info.get_sitedata_sp sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Eliom_common.make_full_state_name ~sp ~secure ~scope:cookie_scope in
  let (_, ior) =
    Lazy.force
      (Eliom_common.Full_state_name_table.find full_st_name !cookie_info)
  in
  match !ior with
  | Eliom_common.SCNo_data -> raise Not_found
  | Eliom_common.SCData_session_expired ->
      raise Eliom_common.Eliom_Session_expired
  | Eliom_common.SC v -> v




(*****************************************************************************)
(** session data *)

let counttableelements = ref []
(* Here only for exploration functions *)

let create_volatile_table, create_volatile_table_during_session =
  let aux ~scope ~secure sitedata =
    let t = Eliom_common.SessionCookies.create 100 in
    let old_remove_session_data =
      sitedata.Eliom_common.remove_session_data
    in
    sitedata.Eliom_common.remove_session_data <-
      (fun cookie ->
         (* cookie is actually either a cookie or a a group name *)
         (* In session group tables, keys may be either group names,
            or a cookie values when no group name has been set. *)
        old_remove_session_data cookie;
        Eliom_common.SessionCookies.remove t cookie
      );
    let old_not_bound_in_data_tables =
      sitedata.Eliom_common.not_bound_in_data_tables
    in
    sitedata.Eliom_common.not_bound_in_data_tables <-
      (fun cookie ->
        old_not_bound_in_data_tables cookie &&
        not (Eliom_common.SessionCookies.mem t cookie)
      );
    counttableelements :=
      (fun () -> Eliom_common.SessionCookies.length t)::
      !counttableelements;
    (scope, secure, t)
  in
  ((fun ~scope ~secure ->
     let sitedata = Eliom_common.get_current_sitedata () in
     aux ~scope ~secure sitedata),
   (fun ~scope ~secure sitedata ->
      aux ~scope ~secure sitedata))
