(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod.ml
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
(** Session timeouts                                                         *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt

(*****************************************************************************)
(* Table of timeouts for sessions *)

(* default timeout = the one set in config file (or here) *)
let (set_default_service_timeout,
     set_default_data_timeout,
     set_default_persistent_timeout,
     get_default_service_timeout,
     get_default_data_timeout,
     get_default_persistent_timeout) =

  let service_t = ref (Some 3600.) in (* 1 hour by default *)
  let data_t = ref (Some 3600.) in (* 1 hour by default *)
  let persistent_t = ref (Some 86400.) in (* 1 day by default *)
  ((fun timeout -> service_t := timeout),
   (fun timeout -> data_t := timeout),
   (fun timeout -> persistent_t := timeout),
   (fun () -> !service_t),
   (fun () -> !data_t),
   (fun () -> !persistent_t))

let set_default_volatile_timeout t =
  set_default_data_timeout t;
  set_default_service_timeout t

let add k v l = (k, v)::List.remove_assoc k l

let set_timeout get set get_default update =
  fun ?fullsessname ~recompute_expdates 
    override_configfile fromconfigfile sitedata t ->
      let def, tl = get sitedata in
      match fullsessname with
        | None ->
            (match def with
               | Some (_, true) when not override_configfile -> ()
                   (* if it has been set by config file 
                      and we do not ask to override, we do nothing *)
               | _ -> set sitedata (Some (t, fromconfigfile), tl))
        | Some fullsessname ->
            (* recompute_expdates works only if fullsessname is present *)
            let oldtopt =
              try
                let (oldt, wasfromconf), newtl =
                  Ocsigen_lib.list_assoc_remove fullsessname tl
                in
                if override_configfile || not wasfromconf
                then
                  set 
                    sitedata
                    (def, (fullsessname, (t, fromconfigfile))::newtl);
                Some oldt
              with Not_found -> 
                set
                  sitedata
                  (def, (fullsessname, (t, fromconfigfile))::tl);
                None
            in
            if recompute_expdates
            then
              let oldt = match oldtopt with
                | Some o -> o
                | None ->
                    match def with
                      | Some (t, _) -> t
                      | None -> get_default ()
              in
              ignore 
                (catch
                   (fun () -> update fullsessname sitedata oldt t)
                   (function e ->
                      Ocsigen_messages.warning
                        ("Eliom: Error while updating timeouts: "^
                           Ocsigen_lib.string_of_exn e);
                      Lwt.return ())
                )
                (*VVV Check possible exceptions raised *)


(* global timeout = timeout for the whole site (may be changed dynamically) *)
let (find_global_service_timeout,
     find_global_data_timeout,
     find_global_persistent_timeout,
     set_global_service_timeout2,
     set_global_data_timeout2,
     set_global_persistent_timeout2) =

  (
   (* find_global_service_timeout *)
   (fun fullsessname sitedata ->
      let def, tl = sitedata.Eliom_common.servtimeout in
      try
        fst (List.assoc fullsessname tl)
      with Not_found -> 
        match def with
          | Some (t, _) -> t
          | None -> get_default_service_timeout ()),

   (* find_global_data_timeout *)
   (fun fullsessname sitedata ->
      let def, tl = sitedata.Eliom_common.datatimeout in
      try
        fst (List.assoc fullsessname tl)
      with Not_found ->
        match def with
          | Some (t, _) -> t
          | None -> get_default_data_timeout ()),

   (* find_global_persistent_timeout *)
   (fun fullsessname sitedata ->
      let def, tl = sitedata.Eliom_common.perstimeout in
      try
        fst (List.assoc fullsessname tl)
      with Not_found ->
        match def with
          | Some (t, _) -> t
          | None -> get_default_persistent_timeout ()),

   (* set_global_service_timeout2 *)
    (set_timeout
      (fun sitedata -> sitedata.Eliom_common.servtimeout)
      (fun sitedata v -> sitedata.Eliom_common.servtimeout <- v)
      get_default_service_timeout
      Eliommod_sessadmin.update_serv_exp),

   (* set_global_data_timeout2 *)
    (set_timeout
      (fun sitedata -> sitedata.Eliom_common.datatimeout)
      (fun sitedata v -> sitedata.Eliom_common.datatimeout <- v)
      get_default_data_timeout
      Eliommod_sessadmin.update_data_exp),

   (* set_global_persistent_timeout *)
    (set_timeout
      (fun sitedata -> sitedata.Eliom_common.perstimeout)
      (fun sitedata v -> sitedata.Eliom_common.perstimeout <- v)
      get_default_persistent_timeout
      Eliommod_sessadmin.update_pers_exp)

  )

let get_global_service_timeout ~session_name sitedata =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  find_global_service_timeout fullsessname sitedata

let get_global_data_timeout ~session_name sitedata =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  find_global_data_timeout fullsessname sitedata

let get_global_persistent_timeout ~session_name sitedata =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  find_global_persistent_timeout fullsessname sitedata

let set_global_service_timeout ~session_name ~recompute_expdates 
    override_configfile sitedata timeout =
  let fullsessname = Eliom_common.make_fullsessname2
    sitedata.Eliom_common.site_dir_string session_name
  in
  set_global_service_timeout2 ~fullsessname ~recompute_expdates
    override_configfile false sitedata timeout

let set_global_data_timeout ~session_name ~recompute_expdates
    override_configfile sitedata timeout =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  set_global_data_timeout2 ~fullsessname ~recompute_expdates
    override_configfile false sitedata timeout

let set_global_persistent_timeout
    ~session_name ~recompute_expdates override_configfile sitedata timeout =
  let fullsessname =
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name
  in
  set_global_persistent_timeout2
    ~fullsessname ~recompute_expdates
    override_configfile false sitedata timeout


let set_default_global_service_timeout
    override_configfile fromconfigfile sitedata timeout =
  set_global_service_timeout2 ~recompute_expdates:false
    override_configfile fromconfigfile sitedata timeout

let set_default_global_data_timeout
    override_configfile fromconfigfile sitedata timeout =
  set_global_data_timeout2 ~recompute_expdates:false
    override_configfile fromconfigfile sitedata timeout

let set_default_global_persistent_timeout	
    override_configfile fromconfigfile sitedata timeout =
  set_global_persistent_timeout2 ~recompute_expdates:false
    override_configfile fromconfigfile sitedata timeout


