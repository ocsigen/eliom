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

(** Session timeouts                                                         *)

(*****************************************************************************)
(*****************************************************************************)

open Eliom_lib
open Lwt

type kind = [`Service | `Data | `Persistent]

(*****************************************************************************)
(* Table of timeouts for sessions *)

let default_timeouts
    : ( kind * Eliom_common.cookie_level * Eliom_common.scope_hierarchy option
    , float ) Hashtbl.t
  =
  let t = Hashtbl.create 9 in
  Hashtbl.add t (`Service, `Session, None) 3600.;
  Hashtbl.add t (`Data, `Session, None) 3600.;
  Hashtbl.add t (`Persistent, `Session, None) 86400.;
  Hashtbl.add t (`Service, `Client_process, None) 3600.;
  Hashtbl.add t (`Data, `Client_process, None) 3600.;
  Hashtbl.add t (`Persistent, `Client_process, None) 86400.;
  t

let set_default ?scope_hierarchy kind level = function
  | Some t ->
      Hashtbl.replace default_timeouts
        ((kind :> kind), (level :> Eliom_common.cookie_level), scope_hierarchy)
        t
  | None ->
      Hashtbl.remove default_timeouts
        ((kind :> kind), (level :> Eliom_common.cookie_level), scope_hierarchy)

let get_default kind user_scope =
  let level = Eliom_common.cookie_level_of_user_scope user_scope
  and scope_hierarchy = Eliom_common.scope_hierarchy_of_user_scope user_scope in
  try
    Some
      (Hashtbl.find default_timeouts
         ( (kind :> kind)
         , (level :> Eliom_common.cookie_level)
         , Some scope_hierarchy ))
  with Not_found -> (
    try
      Some
        (Hashtbl.find default_timeouts
           ((kind :> kind), (level :> Eliom_common.cookie_level), None))
    with Not_found -> None)

let set_timeout_ get set get_default update ?full_st_name ?cookie_level
    ~recompute_expdates override_configfile fromconfigfile sitedata t
  =
  (* cookie_level is useful and mandatory
         only if full_st_name is not present *)
  let def_bro, def_tab, tl = get sitedata in
  match full_st_name with
  | None -> (
    (* means default timeout for all hierarchies *)
    match def_bro, def_tab, cookie_level with
    | Some (_, true), _, Some `Session when not override_configfile ->
        ()
        (* if it has been set by config file
                  and we do not ask to override, we do nothing *)
    | _, Some (_, true), Some `Client_process when not override_configfile ->
        ()
        (* if it has been set by config file
                  and we do not ask to override, we do nothing *)
    | _, _, Some `Session -> set sitedata (Some (t, fromconfigfile), def_tab, tl)
    | _, _, Some `Client_process ->
        set sitedata (def_bro, Some (t, fromconfigfile), tl)
    | _, _, None -> failwith "set_timeout_")
  | Some ({Eliom_common.user_scope; _} as full_st_name) ->
      (* recompute_expdates works only if full_st_name is present *)
      let oldtopt =
        try
          let (oldt, wasfromconf), newtl = List.assoc_remove full_st_name tl in
          if override_configfile || not wasfromconf
          then
            set sitedata
              (def_bro, def_tab, (full_st_name, (t, fromconfigfile)) :: newtl);
          Some oldt
        with Not_found ->
          set sitedata
            (def_bro, def_tab, (full_st_name, (t, fromconfigfile)) :: tl);
          None
      in
      if recompute_expdates
      then
        let oldt =
          match oldtopt with
          | Some o -> o
          | None -> (
            match def_bro, def_tab, user_scope with
            | Some (t, _), _, `Session _ -> t
            | _, Some (t, _), `Client_process _ -> t
            | _, _, ct -> get_default ct)
        in
        ignore
          (catch
             (fun () -> update full_st_name sitedata oldt t)
             (function
               | exn ->
                   Lwt_log.warning ~exn ~section:Lwt_log.eliom
                     "Error while updating timeouts"))
(*VVV Check possible exceptions raised *)

(* global timeout = timeout for the whole site (may be changed dynamically) *)

let sitedata_timeout kind sitedata =
  match kind with
  | `Service -> sitedata.Eliom_common.servtimeout
  | `Data -> sitedata.Eliom_common.datatimeout
  | `Persistent -> sitedata.Eliom_common.perstimeout

let set_sitedata_timeout kind sitedata v =
  match kind with
  | `Service -> sitedata.Eliom_common.servtimeout <- v
  | `Data -> sitedata.Eliom_common.datatimeout <- v
  | `Persistent -> sitedata.Eliom_common.perstimeout <- v

let find_global kind full_st_name sitedata =
  let def_bro, def_tab, tl = sitedata_timeout kind sitedata in
  try fst (List.assoc full_st_name tl)
  with Not_found -> (
    match def_bro, def_tab, full_st_name.Eliom_common.user_scope with
    | Some (t, _), _, `Session _ -> t
    | _, Some (t, _), `Client_process _ -> t
    | _, _, ct -> get_default kind ct)

let set_global_ ?full_st_name ?cookie_level ~kind ~recompute_expdates a =
  set_timeout_ (sitedata_timeout kind)
    (set_sitedata_timeout kind)
    (get_default kind) Eliommod_sessadmin.update_serv_exp ?full_st_name
    ?cookie_level ~recompute_expdates a

let get_global ~kind ~cookie_scope ~secure sitedata =
  let full_st_name =
    Eliom_common.make_full_state_name2 sitedata.Eliom_common.site_dir_string
      secure ~scope:cookie_scope
  in
  find_global kind full_st_name sitedata

let set_global ~kind ~cookie_scope ~secure ~recompute_expdates
    override_configfile sitedata timeout
  =
  let full_st_name =
    Eliom_common.make_full_state_name2 sitedata.Eliom_common.site_dir_string
      secure ~scope:cookie_scope
  in
  set_global_ ~kind ~full_st_name ~recompute_expdates override_configfile false
    sitedata timeout

let set_default_global kind cookie_level override_configfile fromconfigfile
    sitedata timeout
  =
  set_global_ ~kind ~cookie_level ~recompute_expdates:false override_configfile
    fromconfigfile sitedata timeout
