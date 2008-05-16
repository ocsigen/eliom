(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_sessadmin.ml
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(*
(** Iterator on volatile sessions *)
let iter_sessions f =

(** Iterator on persistent sessions *)
let iter_persistent_sessions f =

*)

let close_all_service_sessions2 ?(close_group = false) fullsessname sitedata =
  Eliom_common.SessionCookies.fold
    (fun k (fullsessname2, table, expref, timeoutref, sessgrpref) thr ->
      thr >>= fun () ->
      if fullsessname = fullsessname2 && !timeoutref = Eliom_common.TGlobal
      then (if close_group then
        Eliommod_sersess.close_service_group sitedata !sessgrpref
      else
        Eliom_common.close_service_session2 sitedata !sessgrpref k);
      Lwt_unix.yield ()
    )
    sitedata.Eliom_common.session_services
    (return ())

(** Close all service sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_service_sessions ?close_group ?session_name sitedata =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  close_all_service_sessions2 ?close_group fullsessname sitedata

let close_all_data_sessions2 ?(close_group = false) fullsessname sitedata =
  Eliom_common.SessionCookies.fold
    (fun k (fullsessname2, expref, timeoutref, sessgrpref) thr ->
      thr >>= fun () ->
      if fullsessname = fullsessname2 && !timeoutref = Eliom_common.TGlobal
      then (if close_group then
        Eliommod_datasess.close_data_group sitedata !sessgrpref
      else
        Eliommod_datasess.close_data_session2 sitedata !sessgrpref k);
      Lwt_unix.yield ()
    )
    sitedata.Eliom_common.session_data
    (return ())

(** Close all in memory data sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_data_sessions ?close_group ?session_name sitedata =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  close_all_data_sessions2 ?close_group fullsessname sitedata


let close_all_persistent_sessions2 ?(close_group = false) fullsessname =
  Ocsipersist.iter_table
    (fun k (fullsessname2, old_exp, old_t, sessiongrp) ->
      if fullsessname = fullsessname2 && old_t = Eliom_common.TGlobal
      then (if close_group then
        Eliommod_persess.close_persistent_group sessiongrp
      else Eliommod_persess.close_persistent_session2 sessiongrp k) >>=
      Lwt_unix.yield
      else return ()
    )
    Eliommod_persess.persistent_cookies_table

(** Close all persistent sessions for one session name.
    If the optional parameter [?session_name] (session name) is not present,
    only the session with default name is closed.
 *)
let close_all_persistent_sessions ?close_group ?session_name sitedata =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  close_all_persistent_sessions2 ?close_group fullsessname



(* Update the expiration date for all service sessions                      *)
let update_serv_exp fullsessname sitedata old_glob_timeout new_glob_timeout =
  Ocsigen_messages.debug2
    "--Eliom: Updating expiration date for all service sessions";
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_service_sessions2 fullsessname sitedata
  | _ ->
    let now = Unix.time () in
    Eliom_common.SessionCookies.fold
      (fun k (fullsessname2, table, expref, timeoutref, sessgrpref) thr ->
        thr >>= fun () ->
        (if fullsessname = fullsessname2 && !timeoutref =
          Eliom_common.TGlobal
        then
          let newexp = match !expref, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now ->
              Eliom_common.close_service_session2 sitedata !sessgrpref k
          | _ -> expref := newexp
        );
        Lwt_unix.yield ()
      )
      sitedata.Eliom_common.session_services
      (return ())

(* Update the expiration date for all in memory data sessions                *)
let update_data_exp fullsessname sitedata old_glob_timeout new_glob_timeout =
  Ocsigen_messages.debug2
    "--Eliom: Updating expiration date for all data sessions";
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_data_sessions2 fullsessname sitedata
  | _ ->
    let now = Unix.time () in
    Eliom_common.SessionCookies.fold
      (fun k (fullsessname2, expref, timeoutref, sessgrpref) thr ->
        thr >>= fun () ->
        (if fullsessname = fullsessname2 && !timeoutref =
          Eliom_common.TGlobal
        then
          let newexp = match !expref, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now ->
              Eliommod_datasess.close_data_session2 sitedata !sessgrpref k
          | _ -> expref := newexp
        );
        Lwt_unix.yield ()
      )
      sitedata.Eliom_common.session_data
      (return ())


(* Update the expiration date for all sessions                               *)
let update_pers_exp fullsessname old_glob_timeout new_glob_timeout =
  Ocsigen_messages.debug2
    "--Eliom: Updating expiration date for all persistent sessions";
  match new_glob_timeout with
  | Some t when t <= 0. ->
      (* We close all sessions but those with user defined timeout *)
      close_all_persistent_sessions2 fullsessname
  | _ ->
    let now = Unix.time () in
    Ocsipersist.iter_table
      (fun k (fullsessname2, old_exp, old_t, sessgrp) ->
        if fullsessname = fullsessname2 && old_t =
          Eliom_common.TGlobal
        then
          let newexp = match old_exp, old_glob_timeout, new_glob_timeout with
          | _, _, None -> None
          | None, _, Some t
          | Some _, None, Some t -> Some (now +. t)
          | Some oldexp, Some oldt, Some t -> Some (oldexp -. oldt +. t)
          in
          match newexp with
          | Some t when t <= now ->
              Eliommod_persess.close_persistent_session2 sessgrp k
          | _ ->
              Ocsipersist.add
                Eliommod_persess.persistent_cookies_table
                k
                (fullsessname2, newexp,
                 Eliom_common.TGlobal, sessgrp) >>= Lwt_unix.yield
        else return ()
      )
      Eliommod_persess.persistent_cookies_table


