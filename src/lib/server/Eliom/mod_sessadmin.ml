(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessadmin.ml
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

(*****************************************************************************)
(*****************************************************************************)
(** Internal functions used by Eliom:                                        *)

(** Administration of sessions                                               *)

(*****************************************************************************)
(*****************************************************************************)

let section = Logs.Src.create "eliom:admin"

let close_all_service_states_of_name full_st_name sitedata =
  Common.SessionCookies.fold
    (fun _
      {Common.Service_cookie.full_state_name; timeout; session_group_node; _}
      thr ->
       let* () = thr in
       if full_st_name = full_state_name && !timeout = Common.TGlobal
       then Mod_sessiongroups.Serv.remove session_group_node;
       Lwt.pause ())
    sitedata.Common.session_services Lwt.return_unit

(** Close all service states for one session name.
    If the optional parameter [?state_name] (session name) is not present,
    only the state with default name is closed.
 *)
let close_all_service_states ~scope ~secure sitedata =
  let full_st_name =
    Common.make_full_state_name_of_sitedata ~sitedata ~secure ~scope
  in
  close_all_service_states_of_name full_st_name sitedata
(*VVV Missing:
   - close all sessions, whatever be the state_name
   - secure
   - close all groups (but closing sessions will close the groups (?))
*)

let close_all_data_states_of_name full_st_name sitedata =
  Common.SessionCookies.fold
    (fun _
      {Common.Data_cookie.full_state_name; timeout; session_group_node; _}
      thr ->
       let* () = thr in
       if full_st_name = full_state_name && !timeout = Common.TGlobal
       then Mod_sessiongroups.Data.remove session_group_node;
       Lwt.pause ())
    sitedata.Common.session_data Lwt.return_unit

(** Close all in memory data sessions for one session name.
    If the optional parameter [?state_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_data_states ~scope ~secure sitedata =
  let full_st_name =
    Common.make_full_state_name_of_sitedata ~sitedata ~secure ~scope
  in
  close_all_data_states_of_name full_st_name sitedata
(*VVV Missing:
   - close all sessions, whatever be the state_name
   - secure
   - close all groups (but closing sessions will close the groups (?))
*)

let close_all_persistent_states_of_name full_st_name sitedata =
  Mod_cookies.Persistent_cookies.Cookies.iter
    (fun k {Mod_cookies.full_state_name; timeout = old_t; session_group; _} ->
       let scope = full_state_name.Common.user_scope in
       if full_st_name = full_state_name && old_t = Common.TGlobal
       then
         let* () =
           Mod_persess.close_persistent_state_of_cookie ~scope sitedata
             session_group k
         in
         Lwt.pause ()
       else Lwt.return_unit)

(** Close all persistent sessions for one session name.
    If the optional parameter [?state_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_persistent_states ~scope ~secure sitedata =
  let full_st_name =
    Common.make_full_state_name_of_sitedata ~sitedata ~secure ~scope
  in
  close_all_persistent_states_of_name full_st_name sitedata
(*VVV Missing:
   - close all sessions, whatever be the state_name
   - secure
   - close all groups (but closing sessions will close the groups (?))
*)

(* The expiry date of a state after the global timeout changed from
   [old_glob_timeout] to [new_glob_timeout] *)
let recompute_expiry ~now ~old_glob_timeout ~new_glob_timeout exp =
  match exp, old_glob_timeout, new_glob_timeout with
  | _, _, None -> None
  | None, _, Some t | Some _, None, Some t -> Some (now +. t)
  | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)

(* Update the expiration date for all service sessions                      *)
let update_serv_exp full_st_name sitedata old_glob_timeout new_glob_timeout =
  Logs.app ~src:section (fun fmt ->
    fmt "Updating expiration date for all service sessions");
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_service_states_of_name full_st_name sitedata
  | _ ->
      let now = Unix.time () in
      Common.SessionCookies.fold
        (fun _
          { Common.Service_cookie.full_state_name
          ; expiry
          ; timeout
          ; session_group_node
          ; _ }
          thr ->
           let* () = thr in
           (if full_st_name = full_state_name && !timeout = Common.TGlobal
            then
              let newexp =
                recompute_expiry ~now ~old_glob_timeout ~new_glob_timeout
                  !expiry
              in
              match newexp with
              | Some t when t <= now ->
                  Mod_sessiongroups.Serv.remove session_group_node
              | _ -> expiry := newexp);
           Lwt.pause ())
        sitedata.Common.session_services Lwt.return_unit

(* Update the expiration date for all in memory data sessions                *)
let update_data_exp full_st_name sitedata old_glob_timeout new_glob_timeout =
  Logs.app ~src:section (fun fmt ->
    fmt "Updating expiration date for all data sessions");
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_data_states_of_name full_st_name sitedata
  | _ ->
      let now = Unix.time () in
      Common.SessionCookies.fold
        (fun _
          { Common.Data_cookie.full_state_name
          ; expiry
          ; timeout
          ; session_group_node
          ; _ }
          thr ->
           let* () = thr in
           (if full_st_name = full_state_name && !timeout = Common.TGlobal
            then
              let newexp =
                recompute_expiry ~now ~old_glob_timeout ~new_glob_timeout
                  !expiry
              in
              match newexp with
              | Some t when t <= now ->
                  Mod_sessiongroups.Data.remove session_group_node
              | _ -> expiry := newexp);
           Lwt.pause ())
        sitedata.Common.session_data Lwt.return_unit

(* Update the expiration date for all sessions                               *)
let update_pers_exp full_st_name sitedata old_glob_timeout new_glob_timeout =
  Logs.app ~src:section (fun fmt ->
    fmt "Updating expiration date for all persistent sessions");
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_persistent_states_of_name full_st_name sitedata
  | _ ->
      let now = Unix.time () in
      Mod_cookies.Persistent_cookies.Cookies.iter
        (fun
            k
             { Mod_cookies.full_state_name
             ; expiry = old_exp
             ; timeout = old_t
             ; session_group }
           ->
           let scope = full_state_name.Common.user_scope in
           if full_st_name = full_state_name && old_t = Common.TGlobal
           then
             let newexp =
               recompute_expiry ~now ~old_glob_timeout ~new_glob_timeout old_exp
             in
             match newexp with
             | Some t when t <= now ->
                 Mod_persess.close_persistent_state_of_cookie ~scope sitedata
                   session_group k
             | _ ->
                 let* () =
                   Mod_cookies.Persistent_cookies.add k
                     { Mod_cookies.full_state_name
                     ; expiry = newexp
                     ; timeout = Common.TGlobal
                     ; session_group }
                 in
                 let* () =
                   Mod_cookies.Persistent_cookies.Expiry_dates.remove_cookie
                     old_exp k
                 in
                 Lwt.pause ()
           else Lwt.return_unit)
