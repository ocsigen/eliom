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
(** Service sessions                                                         *)
(*****************************************************************************)
(*****************************************************************************)


let compute_cookie_info secure secure_ci cookie_info =
  let secure = match secure with
    | None -> true
    | Some s -> s
  in
  if secure 
  then match secure_ci with
    | None (* not ssl *) -> cookie_info
    | Some (c, _, _) -> c
  else cookie_info



let close_service_group sitedata fullsessgrp =
  let cooklist = Eliommod_sessiongroups.Serv.find fullsessgrp in
  List.iter (Eliom_common.close_service_session2 sitedata None) cooklist;
  Eliommod_sessiongroups.Serv.remove_group fullsessgrp

let close_service_session ?(close_group = false) ?session_name ~secure ~sp () =
  try
    let fullsessname = Eliom_common.make_fullsessname ~sp session_name in
    let ((cookie_info, _, _), secure_ci) = sp.Eliom_common.sp_cookie_info in
    let cookie_info = compute_cookie_info secure secure_ci cookie_info in
    let (_, ior) = 
      Ocsigen_http_frame.Cookievalues.find fullsessname !cookie_info 
    in
    match !ior with
    | Eliom_common.SC c ->
        if close_group then
          close_service_group sp.Eliom_common.sp_sitedata
            !(c.Eliom_common.sc_session_group)
        else
          Eliom_common.close_service_session2
            sp.Eliom_common.sp_sitedata
            !(c.Eliom_common.sc_session_group)
            c.Eliom_common.sc_value;
        ior := Eliom_common.SCNo_data
    | _ -> ()
  with Not_found -> ()


let rec new_service_cookie sitedata fullsessgrp fullsessname table =
  let c = Eliommod_cookies.make_new_cookie_value () in
  try
    ignore (Eliom_common.SessionCookies.find table c);
                                   (* Actually not needed
                                      for the cookies we use *)
    new_service_cookie sitedata fullsessgrp fullsessname table
  with Not_found ->
    let str = ref (Eliom_common.new_service_session_tables ()) in
    let usertimeout = ref Eliom_common.TGlobal (* See global table *) in
    let serverexp = ref (Some 0.) (* None = never. We'll change it later. *) in
    let fullsessgrpref = ref fullsessgrp in
    Eliom_common.SessionCookies.replace
      (* actually it will add the cookie *)
      table
      c
      (fullsessname,
       !str,
       serverexp (* exp on server *),
       usertimeout,
       fullsessgrpref);
    List.iter
      (Eliom_common.close_service_session2 sitedata None)
      (Eliommod_sessiongroups.Serv.add
         sitedata.Eliom_common.max_service_sessions_per_group
         c fullsessgrp);
    (* add returns the list of session to close if
       maxsessionspergroup exceded *)
    {Eliom_common.sc_value= c;
     Eliom_common.sc_table= str;
     Eliom_common.sc_timeout= usertimeout;
     Eliom_common.sc_exp= serverexp;
     Eliom_common.sc_cookie_exp= ref Eliom_common.CENothing
       (* exp on client - nothing to set *);
     Eliom_common.sc_session_group= fullsessgrpref
   }


let find_or_create_service_cookie ?session_group ?session_name ~secure ~sp () =
  (* If the cookie does not exist, create it.
     Returns the cookie info for the cookie *)
  let fullsessname = Eliom_common.make_fullsessname ~sp session_name in
  let fullsessgrp =
    Eliommod_sessiongroups.make_full_group_name
      sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
      session_group
  in
  let ((cookie_info, _, _), secure_ci) = sp.Eliom_common.sp_cookie_info in
  let cookie_info = compute_cookie_info secure secure_ci cookie_info in
  try
    let (old, ior) = Ocsigen_http_frame.Cookievalues.find fullsessname !cookie_info in
    match !ior with
    | Eliom_common.SCData_session_expired
        (* We do not trust the value sent by the client,
           for security reasons *)
    | Eliom_common.SCNo_data ->
        let v =
          new_service_cookie
            sp.Eliom_common.sp_sitedata
            fullsessgrp fullsessname
            sp.Eliom_common.sp_sitedata.Eliom_common.session_services
        in
        ior := Eliom_common.SC v;
        v
    | Eliom_common.SC v -> v
  with Not_found ->
    let v =
      new_service_cookie
        sp.Eliom_common.sp_sitedata fullsessgrp fullsessname
        sp.Eliom_common.sp_sitedata.Eliom_common.session_services
    in
    cookie_info :=
      Ocsigen_http_frame.Cookievalues.add
        fullsessname
        (None, ref (Eliom_common.SC v))
        !cookie_info;
    v


let find_service_cookie_only ?session_name ~secure ~sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = Eliom_common.make_fullsessname ~sp session_name in
  let ((cookie_info, _, _), secure_ci) = sp.Eliom_common.sp_cookie_info in
  let cookie_info = compute_cookie_info secure secure_ci cookie_info in
  let (_, ior) = Ocsigen_http_frame.Cookievalues.find fullsessname !cookie_info in
  match !ior with
  | Eliom_common.SCNo_data -> raise Not_found
  | Eliom_common.SCData_session_expired ->
      raise Eliom_common.Eliom_Session_expired
  | Eliom_common.SC v -> v



