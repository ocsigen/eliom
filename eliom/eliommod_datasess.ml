(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_datasess.ml
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
(** Volatile data tables                                                     *)
(*****************************************************************************)
(*****************************************************************************)



(* to be called from outside requests *)
let close_data_session2 sitedata fullsessgrp cookie = 
  try
    Eliom_common.SessionCookies.remove 
      sitedata.Eliom_common.session_data cookie;
    Eliommod_sessiongroups.Data.remove cookie fullsessgrp;
    sitedata.Eliom_common.remove_session_data cookie;
  with Not_found -> ()

let close_data_group sitedata fullsessgrp =
  let cooklist = Eliommod_sessiongroups.Data.find fullsessgrp in
  List.iter (close_data_session2 sitedata None) cooklist;
  Eliommod_sessiongroups.Data.remove_group fullsessgrp

(* to be called during a request *)
let close_data_session ?(close_group = false) ?session_name ~sp () = 
  try
    let fullsessname = Eliom_common.make_fullsessname ~sp session_name in
    let (_, cookie_info, _) = sp.Eliom_common.sp_cookie_info in
    let (_, ior) = 
      Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info) 
    in
    match !ior with
    | Eliom_common.SC c ->
        if close_group then
          close_data_group sp.Eliom_common.sp_sitedata
            !(c.Eliom_common.dc_session_group)
        else
          close_data_session2 sp.Eliom_common.sp_sitedata
            !(c.Eliom_common.dc_session_group) 
            c.Eliom_common.dc_value;
        ior := Eliom_common.SCNo_data
    | _ -> ()
  with Not_found -> ()



let rec new_data_cookie sitedata fullsessgrp fullsessname table = 
  let c = Eliommod_cookies.make_new_cookie_value () in
  try
    ignore (Eliom_common.SessionCookies.find table c); 
    (* Actually not needed for the cookies we use *)
    new_data_cookie sitedata fullsessgrp fullsessname table
  with Not_found ->
    let usertimeout = ref Eliom_common.TGlobal (* See global table *) in
    let serverexp = ref (Some 0.) (* None = never. We'll change it later. *) in
    let fullsessgrpref = ref fullsessgrp in
    Eliom_common.SessionCookies.replace
      (* actually it will add the cookie *)
      table 
      c
      (fullsessname,
       serverexp (* exp on server *),
       usertimeout,
       fullsessgrpref);
    List.iter
      (close_data_session2 sitedata None)
      (Eliommod_sessiongroups.Data.add 
         sitedata.Eliom_common.max_volatile_data_sessions_per_group
         c fullsessgrp);
    (* add returns the list of session to close if
       maxsessionspergroup exceded *)
    {Eliom_common.dc_value= c;
     Eliom_common.dc_timeout= usertimeout;
     Eliom_common.dc_exp= serverexp;
     Eliom_common.dc_cookie_exp= 
     ref Eliom_common.CENothing (* exp on client - nothing to set *);
     Eliom_common.dc_session_group= fullsessgrpref
   }






let find_or_create_data_cookie ?session_group ?session_name ~sp () = 
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let fullsessname = Eliom_common.make_fullsessname ~sp session_name in
  let fullsessgrp = 
    Eliommod_sessiongroups.make_full_group_name
      sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
      session_group 
  in
  let (_, cookie_info, _) = sp.Eliom_common.sp_cookie_info in
  try
    let (old, ior) = 
      Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info) 
    in
    match !ior with
    | Eliom_common.SCData_session_expired 
    | Eliom_common.SCNo_data -> 
        let v = 
          new_data_cookie
            sp.Eliom_common.sp_sitedata fullsessgrp fullsessname 
            sp.Eliom_common.sp_sitedata.Eliom_common.session_data
        in
        ior := Eliom_common.SC v;
        v
    | Eliom_common.SC v -> v;
  with Not_found -> 
    let v = 
      new_data_cookie
        sp.Eliom_common.sp_sitedata fullsessgrp fullsessname 
        sp.Eliom_common.sp_sitedata.Eliom_common.session_data 
    in
    cookie_info := 
      Http_frame.Cookievalues.add
        fullsessname
        (Lazy.lazy_from_val (None, ref (Eliom_common.SC v)))
        !cookie_info;
    v
        
let find_data_cookie_only ?session_name ~sp () = 
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = Eliom_common.make_fullsessname ~sp session_name in
  let (_, cookie_info, _) = sp.Eliom_common.sp_cookie_info in
  let (_, ior) = 
    Lazy.force (Http_frame.Cookievalues.find fullsessname !cookie_info) 
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
  let aux sitedata =
    let t = Eliom_common.SessionCookies.create 1000 in
    let old_remove_session_data = 
      sitedata.Eliom_common.remove_session_data 
    in
    sitedata.Eliom_common.remove_session_data <-
      (fun cookie ->
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
    t
  in
  ((fun () ->
    let sitedata = Eliom_common.get_current_sitedata () in
    aux sitedata),
   (fun sp -> aux sp.Eliom_common.sp_sitedata))


