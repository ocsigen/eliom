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
     try
       List.assoc fullsessname sitedata.Eliom_common.servtimeout
     with Not_found -> get_default_service_timeout ()),

   (* find_global_data_timeout *)
   (fun fullsessname sitedata -> 
     try
       List.assoc fullsessname sitedata.Eliom_common.datatimeout
     with Not_found -> get_default_data_timeout ()),

   (* find_global_persistent_timeout *)
   (fun fullsessname sitedata -> 
     try
       List.assoc fullsessname sitedata.Eliom_common.perstimeout
     with Not_found -> get_default_persistent_timeout ()),

   (* set_global_service_timeout2 *)
   (fun fullsessname ~recompute_expdates sitedata t -> 
     if recompute_expdates
     then
       let oldt = 
         try
           List.assoc fullsessname sitedata.Eliom_common.servtimeout
         with Not_found -> get_default_service_timeout ()
       in
       sitedata.Eliom_common.servtimeout <- 
         add fullsessname t sitedata.Eliom_common.servtimeout;
       ignore (catch
                 (fun () -> 
                   Eliommod_sessadmin.update_serv_exp
                     fullsessname sitedata oldt t)
                 (function e -> 
                   Messages.warning 
                     ("Eliom: Error while updating global service timeouts: "^
                      Ocsigen_lib.string_of_exn e);
                   Lwt.return ())
              )
         (*VVV Check possible exceptions raised *)
     else
       sitedata.Eliom_common.servtimeout <- 
         add fullsessname t sitedata.Eliom_common.servtimeout
   ),

   (* set_global_data_timeout2 *)
   (fun fullsessname ~recompute_expdates sitedata t -> 
     if recompute_expdates
     then
       let oldt = 
         try
           List.assoc fullsessname sitedata.Eliom_common.datatimeout
         with Not_found -> get_default_data_timeout ()
       in
       sitedata.Eliom_common.datatimeout <- 
         add fullsessname t sitedata.Eliom_common.datatimeout;
       ignore (catch
                 (fun () -> 
                   Eliommod_sessadmin.update_data_exp
                     fullsessname sitedata oldt t)
                 (function e -> 
                   Messages.warning 
                     ("Eliom: Error while updating global data timeouts: "^
                      Ocsigen_lib.string_of_exn e);
                   Lwt.return ())
              )
         (*VVV Check possible exceptions raised *)
     else
       sitedata.Eliom_common.datatimeout <- 
         add fullsessname t sitedata.Eliom_common.datatimeout
   ),

   (* set_global_persistent_timeout *)
   (fun fullsessname ~recompute_expdates sitedata t -> 
     if recompute_expdates
     then
       let oldt = 
         try
           List.assoc fullsessname sitedata.Eliom_common.perstimeout
         with Not_found -> get_default_persistent_timeout ()
       in
       sitedata.Eliom_common.perstimeout <- 
         add fullsessname t sitedata.Eliom_common.perstimeout;
       ignore (catch
                 (fun () -> 
                   Eliommod_sessadmin.update_pers_exp fullsessname oldt t)
                 (function e -> 
                   Messages.warning 
                     ("Eliom: Error while updating global persistent timeouts: "^
                      Ocsigen_lib.string_of_exn e);
                   Lwt.return ())
              )
         (*VVV Check possible exceptions raised *)
     else
       sitedata.Eliom_common.perstimeout <- 
         add fullsessname t sitedata.Eliom_common.perstimeout
   )
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

let set_global_service_timeout ~session_name ~recompute_expdates sitedata
    timeout = 
  let fullsessname = Eliom_common.make_fullsessname2 
      sitedata.Eliom_common.site_dir_string session_name 
  in
  set_global_service_timeout2
    fullsessname ~recompute_expdates sitedata timeout

let set_global_data_timeout ~session_name ~recompute_expdates sitedata
    timeout = 
  let fullsessname = 
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name 
  in
  set_global_data_timeout2 fullsessname ~recompute_expdates sitedata timeout

let get_global_persistent_timeout ~session_name sitedata =
  let fullsessname = 
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name 
  in
  find_global_persistent_timeout fullsessname sitedata

let set_global_persistent_timeout
    ~session_name ~recompute_expdates sitedata timeout = 
  let fullsessname = 
    Eliom_common.make_fullsessname2
      sitedata.Eliom_common.site_dir_string session_name 
  in
  set_global_persistent_timeout2
    fullsessname ~recompute_expdates sitedata timeout


