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

let fst3 (a,b,c) = a

(*****************************************************************************)
(* Table of timeouts for sessions *)

(* default timeout = the one set in config file (or here) *)
let service_t = ref (Some 3600.) (* 1 hour by default *)
let data_t = ref (Some 3600.) (* 1 hour by default *)
let persistent_t = ref (Some 86400.) (* 1 day by default *)
let tab_service_t = ref (Some 3600.) (* 1 hour by default *)
let tab_data_t = ref (Some 3600.) (* 1 hour by default *)
let tab_persistent_t = ref (Some 86400.) (* 1 day by default *)
let group_service_t = ref (Some 3600.) (* 1 hour by default *)
let group_data_t = ref (Some 3600.) (* 1 hour by default *)
let group_persistent_t = ref (Some 86400.) (* 1 day by default *)

let set_default_service_timeout level timeout =
  match level with
    | `Session -> service_t := timeout
    | `Client_process -> tab_service_t := timeout
    | `Session_group -> group_service_t := timeout

let (set_default_data_timeout,
     set_default_persistent_timeout,
     get_default_service_timeout,
     get_default_data_timeout,
     get_default_persistent_timeout) =

  ((fun cookie_level timeout ->
     match cookie_level with
       | `Session -> data_t := timeout
       | `Client_process -> tab_data_t := timeout
   ),
   (fun cookie_level timeout ->
     match cookie_level with
       | `Session -> persistent_t := timeout
       | `Client_process -> tab_persistent_t := timeout
   ),
   (function
      | `Session -> !service_t
      | `Client_process -> !tab_service_t
   ),
   (function
      | `Session -> !data_t
      | `Client_process -> !tab_data_t
   ),
   (function
      | `Session -> !persistent_t
      | `Client_process -> !tab_persistent_t
   ))

let set_default_service_timeout a b = set_default_service_timeout a b

let set_default_volatile_timeout ct t =
  set_default_data_timeout ct t;
  set_default_service_timeout ct t

let add k v l = (k, v)::List.remove_assoc k l

let set_timeout_ get set get_default update =
  fun ?full_st_name ?cookie_level ~recompute_expdates
    override_configfile fromconfigfile sitedata t ->
      (* cookie_level is useful and mandatory
         only if full_st_name is not present *)
      let def_bro, def_tab, tl = get sitedata in
      match full_st_name with
        | None -> (* means default timeout *)
          (match def_bro, def_tab, cookie_level with
            | Some (_, true), _, Some `Session
              when not override_configfile -> ()
               (* if it has been set by config file
                  and we do not ask to override, we do nothing *)
            | _, Some (_, true), Some `Client_process
              when not override_configfile -> ()
               (* if it has been set by config file
                  and we do not ask to override, we do nothing *)
            | _, _, Some `Session ->
              set sitedata (Some (t, fromconfigfile), def_tab, tl)
            | _, _, Some `Client_process ->
              set sitedata (def_bro, Some (t, fromconfigfile), tl)
            | _, _, None -> failwith "set_timeout_"
          )
        | Some full_st_name ->
            (* recompute_expdates works only if full_st_name is present *)
            let oldtopt =
              try
                let (oldt, wasfromconf), newtl =
                  List.assoc_remove full_st_name tl
                in
                if override_configfile || not wasfromconf
                then
                  set
                    sitedata
                    (def_bro, def_tab, (full_st_name, (t, fromconfigfile))::newtl);
                Some oldt
              with Not_found ->
                set
                  sitedata
                  (def_bro, def_tab, (full_st_name, (t, fromconfigfile))::tl);
                None
            in
            if recompute_expdates
            then
              let oldt = match oldtopt with
                | Some o -> o
                | None ->
                    match def_bro, def_tab, (fst3 full_st_name) with
                      | Some (t, _), _, `Session _ -> t
                      | _, Some (t, _), `Client_process _ -> t
                      | _, _, ct -> get_default
                        (Eliom_common.cookie_level_of_user_scope ct)
              in
              ignore
                (catch
                   (fun () -> update full_st_name sitedata oldt t)
                   (function e ->
                      Ocsigen_messages.warning
                        ("Eliom: Error while updating timeouts: "^
                           Printexc.to_string e);
                      Lwt.return ())
                )
                (*VVV Check possible exceptions raised *)


(* global timeout = timeout for the whole site (may be changed dynamically) *)

let find_global_service_timeout full_st_name sitedata =
  let def_bro, def_tab, tl = sitedata.Eliom_common.servtimeout in
  try
    fst (List.assoc full_st_name tl)
  with Not_found ->
    match def_bro, def_tab, (fst3 full_st_name) with
      | Some (t, _), _, `Session _ -> t
      | _, Some (t, _), `Client_process _ -> t
      | _, _, ct -> get_default_service_timeout
        (Eliom_common.cookie_level_of_user_scope ct)

let find_global_data_timeout full_st_name sitedata =
  let def_bro, def_tab, tl = sitedata.Eliom_common.datatimeout in
  try
    fst (List.assoc full_st_name tl)
  with Not_found ->
    match def_bro, def_tab, (fst3 full_st_name) with
      | Some (t, _), _, `Session _ -> t
      | _, Some (t, _), `Client_process _ -> t
      | _, _, ct -> get_default_data_timeout
        (Eliom_common.cookie_level_of_user_scope ct)

let find_global_persistent_timeout full_st_name sitedata =
  let def_bro, def_tab, tl = sitedata.Eliom_common.perstimeout in
  try
    fst (List.assoc full_st_name tl)
  with Not_found ->
    match def_bro, def_tab, (fst3 full_st_name) with
      | Some (t, _), _, `Session _ -> t
      | _, Some (t, _), `Client_process _ -> t
      | _, _, ct -> get_default_persistent_timeout
        (Eliom_common.cookie_level_of_user_scope ct)

let set_global_service_timeout_
    ?full_st_name ?cookie_level ~recompute_expdates a =
  set_timeout_
    (fun sitedata -> sitedata.Eliom_common.servtimeout)
    (fun sitedata v -> sitedata.Eliom_common.servtimeout <- v)
    get_default_service_timeout
    Eliommod_sessadmin.update_serv_exp
    ?full_st_name ?cookie_level ~recompute_expdates a

let set_global_data_timeout_
    ?full_st_name ?cookie_level ~recompute_expdates a =
  set_timeout_
    (fun sitedata -> sitedata.Eliom_common.datatimeout)
    (fun sitedata v -> sitedata.Eliom_common.datatimeout <- v)
    get_default_data_timeout
    Eliommod_sessadmin.update_data_exp
    ?full_st_name ?cookie_level ~recompute_expdates a

let set_global_persistent_timeout_
    ?full_st_name ?cookie_level ~recompute_expdates a =
  set_timeout_
    (fun sitedata -> sitedata.Eliom_common.perstimeout)
    (fun sitedata v -> sitedata.Eliom_common.perstimeout <- v)
    get_default_persistent_timeout
    Eliommod_sessadmin.update_pers_exp
    ?full_st_name ?cookie_level ~recompute_expdates a


let get_global_service_timeout ~cookie_scope ~secure sitedata =
  let full_st_name =
    Eliom_common.make_full_state_name2
      sitedata.Eliom_common.site_dir_string secure ~scope:cookie_scope
  in
  find_global_service_timeout full_st_name sitedata

let get_global_data_timeout ~cookie_scope ~secure sitedata =
  let full_st_name =
    Eliom_common.make_full_state_name2
      sitedata.Eliom_common.site_dir_string secure ~scope:cookie_scope
  in
  find_global_data_timeout full_st_name sitedata

let get_global_persistent_timeout ~cookie_scope ~secure sitedata =
  let full_st_name =
    Eliom_common.make_full_state_name2
      sitedata.Eliom_common.site_dir_string secure ~scope:cookie_scope
  in
  find_global_persistent_timeout full_st_name sitedata

let set_global_service_timeout ~cookie_scope ~secure ~recompute_expdates
    override_configfile sitedata timeout =
  let full_st_name = Eliom_common.make_full_state_name2
    sitedata.Eliom_common.site_dir_string secure ~scope:cookie_scope
  in
  set_global_service_timeout_ ~full_st_name ~recompute_expdates
    override_configfile false sitedata timeout

let set_global_data_timeout ~cookie_scope ~secure ~recompute_expdates
    override_configfile sitedata timeout =
  let full_st_name =
    Eliom_common.make_full_state_name2
      sitedata.Eliom_common.site_dir_string secure ~scope:cookie_scope
  in
  set_global_data_timeout_ ~full_st_name ~recompute_expdates
    override_configfile false sitedata timeout

let set_global_persistent_timeout
    ~cookie_scope ~secure ~recompute_expdates
    override_configfile sitedata timeout =
  let full_st_name =
    Eliom_common.make_full_state_name2
      sitedata.Eliom_common.site_dir_string secure ~scope:cookie_scope
  in
  set_global_persistent_timeout_
    ~full_st_name ~recompute_expdates
    override_configfile false sitedata timeout


let set_default_global_service_timeout cookie_level
    override_configfile fromconfigfile sitedata timeout =
  set_global_service_timeout_ ~cookie_level ~recompute_expdates:false
    override_configfile fromconfigfile sitedata timeout

let set_default_global_data_timeout cookie_level
    override_configfile fromconfigfile sitedata timeout =
  set_global_data_timeout_ ~cookie_level ~recompute_expdates:false
    override_configfile fromconfigfile sitedata timeout

let set_default_global_persistent_timeout cookie_level
    override_configfile fromconfigfile sitedata timeout =
  set_global_persistent_timeout_ ~cookie_level ~recompute_expdates:false
    override_configfile fromconfigfile sitedata timeout
