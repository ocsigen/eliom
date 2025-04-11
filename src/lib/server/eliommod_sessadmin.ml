open Lwt.Syntax

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
(*****************************************************************************)
(*****************************************************************************)
(** Internal functions used by Eliom:                                        *)

(** Administration of sessions                                               *)

(*****************************************************************************)
(*****************************************************************************)

open Lwt

let section = Logs.Src.create "eliom:admin"

(*
   (** Iterator on volatile sessions *)
let iter_sessions f =

(** Iterator on persistent sessions *)
let iter_persistent_sessions f =
*)

let close_all_service_states2 full_st_name sitedata =
  Eliom_common.SessionCookies.fold
    (fun _
      { Eliom_common.Service_cookie.full_state_name
      ; timeout
      ; session_group_node
      ; _ }
      thr ->
       let* () = thr in
       if full_st_name = full_state_name && !timeout = Eliom_common.TGlobal
       then Eliommod_sessiongroups.Serv.remove session_group_node;
       Lwt.pause ())
    sitedata.Eliom_common.session_services return_unit

(** Close all service states for one session name.
    If the optional parameter [?state_name] (session name) is not present,
    only the state with default name is closed.
 *)
let close_all_service_states ~scope ~secure sitedata =
  let full_st_name =
    Eliom_common.make_full_state_name2
      (Eliom_common.get_site_dir_string sitedata)
      secure ~scope
  in
  close_all_service_states2 full_st_name sitedata
(*VVV Missing:
   - close all sessions, whatever be the state_name
   - secure
   - close all groups (but closing sessions will close the groups (?))
*)

let close_all_data_states2 full_st_name sitedata =
  Eliom_common.SessionCookies.fold
    (fun _
      {Eliom_common.Data_cookie.full_state_name; timeout; session_group_node; _}
      thr ->
       thr >>= fun () ->
       if full_st_name = full_state_name && !timeout = Eliom_common.TGlobal
       then Eliommod_sessiongroups.Data.remove session_group_node;
       Lwt.pause ())
    sitedata.Eliom_common.session_data return_unit

(** Close all in memory data sessions for one session name.
    If the optional parameter [?state_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_data_states ~scope ~secure sitedata =
  let full_st_name =
    Eliom_common.make_full_state_name2
      (Eliom_common.get_site_dir_string sitedata)
      secure ~scope
  in
  close_all_data_states2 full_st_name sitedata
(*VVV Missing:
   - close all sessions, whatever be the state_name
   - secure
   - close all groups (but closing sessions will close the groups (?))
*)

let close_all_persistent_states2 full_st_name sitedata =
  Eliommod_cookies.Persistent_cookies.Cookies.iter
    (fun
        k
         {Eliommod_cookies.full_state_name; timeout = old_t; session_group; _}
       ->
       let scope = full_state_name.Eliom_common.user_scope in
       if full_st_name = full_state_name && old_t = Eliom_common.TGlobal
       then
         Eliommod_persess.close_persistent_state2 ~scope sitedata session_group
           k
         >>= Lwt.pause
       else return_unit)

(** Close all persistent sessions for one session name.
    If the optional parameter [?state_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_persistent_states ~scope ~secure sitedata =
  let full_st_name =
    Eliom_common.make_full_state_name2
      (Eliom_common.get_site_dir_string sitedata)
      secure ~scope
  in
  close_all_persistent_states2 full_st_name sitedata
(*VVV Missing:
   - close all sessions, whatever be the state_name
   - secure
   - close all groups (but closing sessions will close the groups (?))
*)

(* Update the expiration date for all service sessions                      *)
let update_serv_exp full_st_name sitedata old_glob_timeout new_glob_timeout =
  Logs.app ~src:section (fun fmt ->
    fmt "Updating expiration date for all service sessions");
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_service_states2 full_st_name sitedata
  | _ ->
      let now = Unix.time () in
      Eliom_common.SessionCookies.fold
        (fun _
          { Eliom_common.Service_cookie.full_state_name
          ; expiry
          ; timeout
          ; session_group_node
          ; _ }
          thr ->
           let* () = thr in
           (if full_st_name = full_state_name && !timeout = Eliom_common.TGlobal
            then
              let newexp =
                match !expiry, old_glob_timeout, new_glob_timeout with
                | _, _, None -> None
                | None, _, Some t | Some _, None, Some t -> Some (now +. t)
                | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
              in
              match newexp with
              | Some t when t <= now ->
                  Eliommod_sessiongroups.Serv.remove session_group_node
              | _ -> expiry := newexp);
           Lwt.pause ())
        sitedata.Eliom_common.session_services return_unit

(* Update the expiration date for all in memory data sessions                *)
let update_data_exp full_st_name sitedata old_glob_timeout new_glob_timeout =
  Logs.app ~src:section (fun fmt ->
    fmt "Updating expiration date for all data sessions");
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_data_states2 full_st_name sitedata
  | _ ->
      let now = Unix.time () in
      Eliom_common.SessionCookies.fold
        (fun _
          { Eliom_common.Data_cookie.full_state_name
          ; expiry
          ; timeout
          ; session_group_node
          ; _ }
          thr ->
           thr >>= fun () ->
           (if full_st_name = full_state_name && !timeout = Eliom_common.TGlobal
            then
              let newexp =
                match !expiry, old_glob_timeout, new_glob_timeout with
                | _, _, None -> None
                | None, _, Some t | Some _, None, Some t -> Some (now +. t)
                | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
              in
              match newexp with
              | Some t when t <= now ->
                  Eliommod_sessiongroups.Data.remove session_group_node
              | _ -> expiry := newexp);
           Lwt.pause ())
        sitedata.Eliom_common.session_data return_unit

(* Update the expiration date for all sessions                               *)
let update_pers_exp full_st_name sitedata old_glob_timeout new_glob_timeout =
  Logs.app ~src:section (fun fmt ->
    fmt "Updating expiration date for all persistent sessions");
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_persistent_states2 full_st_name sitedata
  | _ ->
      let now = Unix.time () in
      Eliommod_cookies.Persistent_cookies.Cookies.iter
        (fun
            k
             { Eliommod_cookies.full_state_name
             ; expiry = old_exp
             ; timeout = old_t
             ; session_group }
           ->
           let scope = full_state_name.Eliom_common.user_scope in
           if full_st_name = full_state_name && old_t = Eliom_common.TGlobal
           then
             let newexp =
               match old_exp, old_glob_timeout, new_glob_timeout with
               | _, _, None -> None
               | None, _, Some t | Some _, None, Some t -> Some (now +. t)
               | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
             in
             match newexp with
             | Some t when t <= now ->
                 Eliommod_persess.close_persistent_state2 ~scope sitedata
                   session_group k
             | _ ->
                 let* () =
                   Eliommod_cookies.Persistent_cookies.add k
                     { Eliommod_cookies.full_state_name
                     ; expiry = newexp
                     ; timeout = Eliom_common.TGlobal
                     ; session_group }
                 in
                 Eliommod_cookies.Persistent_cookies.Expiry_dates.remove_cookie
                   old_exp k
                 >>= Lwt.pause
           else return_unit)
