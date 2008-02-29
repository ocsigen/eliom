(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessions.ml
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

open Lwt
open Eliommod
open Extensions
open Lazy


(*****************************************************************************)
(* Making accessible some types from Eliommod: *)

type server_params = Eliommod.server_params


let get_config () = 
  match global_register_allowed () with
  | Some _ -> !Eliommod.config
  | None -> raise (Eliom_function_forbidden_outside_site_loading "get_config")


let find_sitedata fun_name = function
  | Some sp -> sp.sp_sitedata
  | None ->
      match global_register_allowed () with
      | Some get_current_sitedata -> get_current_sitedata ()
      | _ -> raise (Eliom_function_forbidden_outside_site_loading fun_name)


let get_user_agent ~sp = sp.sp_ri.ri_user_agent
let get_full_url ~sp = sp.sp_ri.ri_url_string
let get_ip ~sp = sp.sp_ri.ri_ip
let get_inet_addr ~sp = sp.sp_ri.ri_inet_addr
let get_get_params ~sp = force sp.sp_ri.ri_get_params
let get_all_get_params ~sp = sp.sp_si.si_all_get_params
let get_get_params_string ~sp = sp.sp_ri.ri_get_params_string
let get_post_params ~sp = force sp.sp_ri.ri_post_params
let get_all_post_params ~sp = sp.sp_si.si_all_post_params
let get_current_full_path ~sp = sp.sp_ri.ri_full_path
let get_current_full_path_string ~sp = sp.sp_ri.ri_full_path_string
let get_current_sub_path ~sp = sp.sp_ri.ri_sub_path
let get_current_sub_path_string ~sp = sp.sp_ri.ri_sub_path_string
let get_hostname ~sp = sp.sp_ri.ri_host
let get_port ~sp = sp.sp_ri.ri_port
let get_other_get_params ~sp = sp.sp_si.si_other_get_params
let get_suffix ~sp = sp.sp_suffix
let get_session_name ~sp = sp.sp_fullsessname
let get_exn ~sp = sp.sp_ri.ri_extension_info
let get_config_file_charset ~sp = sp.sp_si.si_config_file_charset
let get_cookies ~sp = force sp.sp_ri.ri_cookies
let get_data_cookies ~sp = sp.sp_si.si_data_session_cookies
let get_persistent_cookies ~sp = sp.sp_si.si_persistent_session_cookies
let get_previous_extension_error_code ~sp = 
  sp.sp_si.si_previous_extension_error

let get_service_session_cookie ?session_name ~sp () = 
  try
    let c = find_service_cookie_only ?session_name ~sp () in
    Some c.sc_value
  with Not_found -> None

let get_volatile_data_session_cookie ?session_name ~sp () = 
  try
    let c = find_data_cookie_only ?session_name ~sp () in
    Some c.dc_value
  with Not_found -> None

let get_persistent_data_session_cookie ?session_name ~sp () =
  catch
    (fun () ->
      find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
      return (Some c.pc_value)
    )
    (function Not_found -> return None | e -> fail e)

let get_default_service_session_timeout = Eliommod.get_default_service_timeout
let set_default_service_session_timeout = Eliommod.set_default_service_timeout

let get_default_volatile_data_session_timeout = 
  Eliommod.get_default_data_timeout

let set_default_volatile_data_session_timeout = 
  Eliommod.set_default_data_timeout

let set_default_volatile_session_timeout = 
  Eliommod.set_default_volatile_timeout
    
let set_global_service_session_timeout 
    ?session_name ?sp ?(recompute_expdates = false) timeout = 
  let sitedata = find_sitedata "set_global_service_timeout" sp in
  Eliommod.set_global_service_timeout
    ~session_name ~recompute_expdates sitedata timeout
  
let set_global_volatile_data_session_timeout 
    ?session_name ?sp ?(recompute_expdates = false) timeout = 
  let sitedata = find_sitedata "set_global_data_timeout" sp in
  Eliommod.set_global_data_timeout 
    ~session_name ~recompute_expdates sitedata timeout
  
let set_global_volatile_session_timeout
    ?session_name ?sp ?(recompute_expdates = false) timeout = 
  let sitedata = find_sitedata "set_global_volatile_timeouts" sp in
  Eliommod.set_global_service_timeout
    ~session_name ~recompute_expdates sitedata timeout;
  Eliommod.set_global_data_timeout 
    ~session_name ~recompute_expdates sitedata timeout

let get_global_service_session_timeout ?session_name ?sp () = 
  let sitedata = find_sitedata "get_global_timeout" sp in
  Eliommod.get_global_service_timeout ?session_name sitedata

let get_global_volatile_data_session_timeout ?session_name ?sp () = 
  let sitedata = find_sitedata "get_global_timeout" sp in
  Eliommod.get_global_data_timeout ?session_name sitedata

let get_default_persistent_data_session_timeout = 
  Eliommod.get_default_persistent_timeout
let set_default_persistent_data_session_timeout = 
  Eliommod.set_default_persistent_timeout

let set_global_persistent_data_session_timeout 
    ?session_name ?sp ?(recompute_expdates = false) timeout = 
  let sitedata = find_sitedata "set_global_persistent_timeout" sp in
  Eliommod.set_global_persistent_timeout
    ~session_name ~recompute_expdates sitedata timeout

let get_global_persistent_data_session_timeout ?session_name ?sp () =
  let sitedata = find_sitedata "get_global_persistent_timeout" sp in
  Eliommod.get_global_persistent_timeout ?session_name sitedata


let set_service_session_timeout ?session_name ~sp t = 
  let c = find_or_create_service_cookie ?session_name ~sp () in
  let tor = c.sc_timeout in
  match t with
  | None -> tor := TNone
  | Some t -> tor := TSome t

let set_volatile_data_session_timeout ?session_name ~sp t = 
  let c = find_or_create_data_cookie ?session_name ~sp () in
  let tor = c.dc_timeout in
  match t with
  | None -> tor := TNone
  | Some t -> tor := TSome t


let unset_service_session_timeout ?session_name ~sp () = 
  try
    let c = find_service_cookie_only ?session_name ~sp () in
    let tor = c.sc_timeout in
    tor := TGlobal
  with Not_found -> ()

let unset_volatile_data_session_timeout ?session_name ~sp () = 
  try
    let c = find_data_cookie_only ?session_name ~sp () in
    let tor = c.dc_timeout in
    tor := TGlobal
  with Not_found -> ()


let get_service_session_timeout ?session_name ~sp () = 
  try
    let c = find_service_cookie_only ?session_name ~sp () in
    let tor = c.sc_timeout in
    match !tor with
    | TGlobal -> 
        Eliommod.get_global_service_timeout ?session_name sp.sp_sitedata
    | TNone -> None
    | TSome t -> Some t
  with Not_found -> 
    Eliommod.get_global_service_timeout ?session_name sp.sp_sitedata

let get_volatile_data_session_timeout ?session_name ~sp () = 
  try
    let c = find_data_cookie_only ?session_name ~sp () in
    let tor = c.dc_timeout in
    match !tor with
    | TGlobal -> Eliommod.get_global_data_timeout ?session_name sp.sp_sitedata
    | TNone -> None
    | TSome t -> Some t
  with Not_found -> 
    Eliommod.get_global_data_timeout ?session_name sp.sp_sitedata


let set_volatile_session_timeout ?session_name ~sp t = 
  set_service_session_timeout ?session_name ~sp t;
  set_volatile_data_session_timeout ?session_name ~sp t

let unset_volatile_session_timeout ?session_name ~sp () = 
  unset_service_session_timeout ?session_name ~sp ();
  unset_volatile_data_session_timeout ?session_name ~sp ()






let set_persistent_data_session_timeout ?session_name ~sp t =
  find_or_create_persistent_cookie ?session_name ~sp () >>= fun c ->
  let tor = c.pc_timeout in
  return
      (match t with
      | None -> tor := TNone
      | Some t -> tor := TSome t)

let unset_persistent_data_session_timeout ?session_name ~sp () = 
  catch
    (fun () ->
      find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
      let tor = c.pc_timeout in
      tor := TGlobal;
      return ()
    )
    (function Not_found -> return () | e -> fail e)

let get_persistent_data_session_timeout ?session_name ~sp () = 
  catch
    (fun () ->
      find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
      let tor = c.pc_timeout in
      return
        (match !tor with
        | TGlobal -> Eliommod.get_global_persistent_timeout
              ~session_name sp.sp_sitedata
        | TNone -> None
        | TSome t -> Some t)
    )
    (function
      | Not_found -> 
          return 
            (Eliommod.get_global_persistent_timeout 
               ~session_name sp.sp_sitedata)
      | e -> fail e)


(* session groups *)

type 'a session_data =
  | No_data
  | Data_session_expired
  | Data of 'a


let set_service_session_group ?set_max ?session_name ~sp n = 
  let c = find_or_create_service_cookie ?session_name ~sp () in
  let n = 
    Eliommod_sessiongroups.make_full_group_name
      sp.sp_sitedata.site_dir_string (Some n) 
  in
  let grp = c.sc_session_group in
  List.iter 
    (close_service_session2 sp.sp_sitedata None)
    (Eliommod_sessiongroups.Serv.move ?set_max
       sp.sp_sitedata.max_service_sessions_per_group c.sc_value !grp n);
  grp := n

let unset_service_session_group ?session_name ~sp () = 
  try
    let c = find_service_cookie_only ?session_name ~sp () in
    let grp = c.sc_session_group in
    Eliommod_sessiongroups.Serv.remove c.sc_value !grp;
    grp := None
  with
    | Not_found
    | Eliom_Session_expired -> ()

let get_service_session_group ?session_name ~sp () = 
  try
    let c = find_service_cookie_only ?session_name ~sp () in
    match !(c.sc_session_group) with
      | None -> No_data
      | Some v -> Data (snd (Eliommod_sessiongroups.getsessgrp v))
  with
    | Not_found -> No_data
    | Eliom_Session_expired -> Data_session_expired

let set_volatile_data_session_group ?set_max ?session_name ~sp n = 
  let c = find_or_create_data_cookie ?session_name ~sp () in
  let n = 
    Eliommod_sessiongroups.make_full_group_name
      sp.sp_sitedata.site_dir_string (Some n) 
  in
  let grp = c.dc_session_group in
  List.iter 
    (close_data_session2 sp.sp_sitedata None)
    (Eliommod_sessiongroups.Data.move ?set_max
       sp.sp_sitedata.max_volatile_data_sessions_per_group c.dc_value !grp n);
  grp := n 

let unset_volatile_data_session_group ?session_name ~sp () = 
  try
    let c = find_data_cookie_only ?session_name ~sp () in
    let grp = c.dc_session_group in
    Eliommod_sessiongroups.Data.remove c.dc_value !grp;
    grp := None
  with
    | Not_found
    | Eliom_Session_expired -> ()

let get_volatile_data_session_group ?session_name ~sp () = 
  try
    let c = find_data_cookie_only ?session_name ~sp () in
    match !(c.dc_session_group) with
      | None -> No_data
      | Some v -> Data (snd (Eliommod_sessiongroups.getsessgrp v))
  with
    | Not_found -> No_data
    | Eliom_Session_expired -> Data_session_expired

let set_persistent_data_session_group ?set_max ?session_name ~sp n = 
  find_or_create_persistent_cookie ?session_name ~sp () >>= fun c ->
  let n = 
    Eliommod_sessiongroups.make_persistent_full_group_name
      sp.sp_sitedata.site_dir_string (Some n)
  in
  let grp = c.pc_session_group in
  Eliommod_sessiongroups.Pers.move ?set_max
    sp.sp_sitedata.max_persistent_data_sessions_per_group 
    c.pc_value !grp n >>= fun l ->
  Lwt_util.iter (close_persistent_session2 None) l >>= fun () ->
  Lwt.return (grp := n) 

let unset_persistent_data_session_group ?session_name ~sp () = 
  Lwt.catch
    (fun () ->
       find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
       let grp = c.pc_session_group in
       Eliommod_sessiongroups.Pers.remove c.pc_value !grp >>= fun () ->
       grp := None;
       Lwt.return ()
    )
    (function
       | Not_found
       | Eliom_Session_expired -> Lwt.return ()
       | e -> fail e)


let get_persistent_data_session_group ?session_name ~sp () = 
  catch
    (fun () ->
       find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
       Lwt.return (match !(c.pc_session_group) with
                     | None -> No_data
                     | Some v -> 
                         Data (snd (Eliommod_sessiongroups.getperssessgrp v))
                  )
    )
    (function
       | Not_found -> Lwt.return No_data
       | Eliom_Session_expired -> Lwt.return Data_session_expired
       | e -> fail e)


let set_default_max_service_sessions_per_group ~sp n =
  sp.sp_sitedata.max_service_sessions_per_group <- n

let set_default_max_volatile_data_sessions_per_group ~sp n =
  sp.sp_sitedata.max_volatile_data_sessions_per_group <- n

let set_default_max_persistent_data_sessions_per_group ~sp n =
  sp.sp_sitedata.max_persistent_data_sessions_per_group <- n





(* expiration dates *)

let set_service_session_cookie_exp_date ?session_name ~sp t = 
  let c = find_or_create_service_cookie ?session_name ~sp () in
  let exp = c.sc_cookie_exp in
  match t with
  | None -> exp := CEBrowser
  | Some t -> exp := CESome t

(*
let get_service_session_cookie_exp_date ?session_name ~sp () = 
  try
    let (_, _, _, _, exp) = find_service_cookie_only ?session_name ~sp () in
  let exp = c.sc_cookie_exp in
    !exp
  with Not_found -> CEBrowser
*)

let set_volatile_data_session_cookie_exp_date ?session_name ~sp t = 
  let c = find_or_create_data_cookie ?session_name ~sp () in
  let exp = c.dc_cookie_exp in
  match t with
  | None -> exp := CEBrowser
  | Some t -> exp := CESome t

(*
let get_volatile_data_session_cookie_exp_date ?session_name ~sp () = 
  try
    let c = find_data_cookie_only ?session_name ~sp () in
    let exp = c.dc_cookie_exp in
    !exp
  with Not_found -> CEBrowser
*)

let set_volatile_session_cookies_exp_date ?session_name ~sp t = 
  set_service_session_cookie_exp_date ?session_name ~sp t;
  set_volatile_data_session_cookie_exp_date ?session_name ~sp t


let set_persistent_data_session_cookie_exp_date ?session_name ~sp t = 
  find_or_create_persistent_cookie ?session_name ~sp () >>= 
  fun c ->
  let exp = c.pc_cookie_exp in
  return
      (match t with
      | None -> exp := CEBrowser
      | Some t -> exp := CESome t)

let get_persistent_data_session_cookie_exp_date ?session_name ~sp () = 
  catch
    (fun () ->
      find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
      return !(c.pc_cookie_exp))
    (function Not_found -> return CEBrowser | e -> fail e)





(* *)

let get_site_dir ~sp = sp.sp_sitedata.site_dir
let get_site_dir_string ~sp = sp.sp_sitedata.site_dir_string
let get_ri ~sp = sp.sp_ri

let get_tmp_filename fi = fi.tmp_filename
let get_filesize fi = fi.filesize
let get_original_filename fi = fi.original_filename

let get_global_table ~sp = sp.sp_sitedata.global_services
let get_sitedata ~sp = sp.sp_sitedata

(** If the session does not exist, we create it 
   (new cookie, new session service table) *)
let get_session_service_table ?session_name ~sp () = 
  let c = find_or_create_service_cookie ?session_name ~sp () in
  c.sc_table


let set_site_handler sitedata handler =
  sitedata.exn_handler <- handler




(*****************************************************************************)
(** {2 persistent sessions} *)

open Ocsipersist

type 'a persistent_table = (int64 * 'a) Ocsipersist.table

let create_persistent_table = create_persistent_table

let get_persistent_session_data ?session_name ~table ~sp () =
  catch 
    (fun () ->
      find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
      Ocsipersist.find table c.pc_value >>= fun (_, v) ->
      return (Data v)
    )
    (function
      | Eliom_Session_expired -> return Data_session_expired
      | Not_found -> return No_data
      | e -> fail e)

let set_persistent_session_data ?session_name ~table ~sp value =
  find_or_create_persistent_cookie ?session_name ~sp () >>= fun c ->
  Ocsipersist.add table c.pc_value (Int64.zero, value)

let remove_persistent_session_data ?session_name ~table ~sp () =
  catch
    (fun () ->
      find_persistent_cookie_only ?session_name ~sp () >>= fun c ->
      Ocsipersist.remove table c.pc_value)
    (function Not_found | Eliom_Session_expired -> return () | e -> fail e)


(*****************************************************************************)
(** {2 session data in memory} *)
type 'a volatile_table = 'a SessionCookies.t

let create_volatile_table ?sp () = 
  match sp with
  | None -> 
      (match global_register_allowed () with
      | Some get_current_sitedata -> create_volatile_table ()
      | None -> raise (Eliom_function_forbidden_outside_site_loading
                         "create_volatile_table"))
  | Some sp -> create_volatile_table_during_session sp

let get_volatile_session_data ?session_name ~table ~sp () =
  try 
    let c = find_data_cookie_only ?session_name ~sp () in
    Data (SessionCookies.find table c.dc_value)
  with 
  | Not_found -> No_data
  | Eliom_Session_expired -> Data_session_expired

let set_volatile_session_data ?session_name ~table ~sp value =
  let c = find_or_create_data_cookie ?session_name ~sp () in
  SessionCookies.replace table c.dc_value value

let remove_volatile_session_data ?session_name ~table ~sp () =
  try 
    let c = find_data_cookie_only ?session_name ~sp () in
    SessionCookies.remove table c.dc_value
  with Not_found | Eliom_Session_expired -> ()



(*****************************************************************************)
(** Close a session *)
let close_persistent_data_session = Eliommod.close_persistent_session

let close_volatile_session = Eliommod.close_volatile_session
let close_service_session = Eliommod.close_service_session
let close_volatile_data_session = Eliommod.close_data_session

let close_session ?close_group ?session_name ~sp () =
  close_volatile_session ?close_group ?session_name ~sp ();
  close_persistent_data_session ?close_group ?session_name ~sp ()

let close_all_volatile_data_sessions ?close_group ?session_name ?sp () =
  let sitedata = find_sitedata "close_all_data_sessions" sp in
  Eliommod.close_all_data_sessions ?close_group ?session_name sitedata

let close_all_service_sessions ?close_group ?session_name ?sp () =
  let sitedata = find_sitedata "close_all_service_sessions" sp in
  Eliommod.close_all_service_sessions ?close_group ?session_name sitedata

let close_all_volatile_sessions ?close_group ?session_name ?sp () =
  close_all_volatile_data_sessions ?close_group ?session_name ?sp () >>=
  close_all_service_sessions ?close_group ?session_name ?sp



let close_all_persistent_data_sessions ?close_group ?session_name ?sp () =
  let sitedata = find_sitedata "close_all_persistent_sessions" sp in
  Eliommod.close_all_persistent_sessions ?close_group ?session_name sitedata

let close_all_sessions ?close_group ?session_name ?sp () =
 close_all_volatile_sessions ?close_group ?session_name ?sp () >>=
 close_all_persistent_data_sessions ?close_group ?session_name ?sp


(*****************************************************************************)
(* Administration *)

module Session_admin = struct

  (** Type used to describe session timeouts *)

  type timeout = Eliommod.timeout =
    | TGlobal (** see global setting *)
    | TNone   (** explicitely set no timeout *)
    | TSome of float (** timeout duration in seconds *)

  type service_session = 
      string (* cookie value *)
        *
      (string                  (* session fullsessname *) *
       Eliommod.tables         (* session table *) * 
       float option ref        (* expiration date by timeout 
                                  (server side) *) *
       Eliommod.timeout ref    (* user timeout *) *
       Eliommod_sessiongroups.sessgrp option ref       (* session group *)) *
       Eliommod.sitedata

  type data_session =
      string (* cookie value *)
        *
      (string                  (* session fullsessname *) *
       float option ref        (* expiration date by timeout 
                                  (server side) *) *
       Eliommod.timeout ref   (* user timeout *) *
       Eliommod_sessiongroups.sessgrp option ref      (* session group *)) *
       Eliommod.sitedata

  type persistent_session =
      string (* cookie value *)
        *
      (string                  (* session fullsessname *) *
       float option            (* expiration date by timeout 
                                  (server side) *) *
       Eliommod.timeout        (* user timeout *) *
       Eliommod_sessiongroups.perssessgrp option           (* session group *))


  let close_service_session ?(close_group = false)
      ~session:(cookie, (_, _, _, _, sgr), sitedata) =
    if close_group then
      Eliommod.close_service_group sitedata !sgr
    else
      Eliommod.close_service_session2 sitedata !sgr cookie

  let close_volatile_data_session ?(close_group = false)
      ~session:(cookie, (_, _, _, sgr), sitedata) =
    if close_group then
      Eliommod.close_data_group sitedata !sgr
    else 
      Eliommod.close_data_session2 sitedata !sgr cookie

  let close_persistent_data_session ?(close_group = false)
      ~session:(cookie, (_, _, _, sg)) =
    if close_group then
      Eliommod.close_persistent_group sg
    else
      Eliommod.close_persistent_session2 sg cookie

  let get_volatile_session_data ~session:(cookie, _, _) ~table =
    SessionCookies.find table cookie

  let get_persistent_session_data ~session:(cookie, _) ~table =
    Ocsipersist.find table cookie >>= fun (_, a) -> return a

  let remove_volatile_session_data ~session:(cookie, _, _) ~table =
    SessionCookies.remove table cookie

  let remove_persistent_session_data ~session:(cookie, _) ~table =
    Ocsipersist.remove table cookie

  let get_service_session_name ~session:(_, (s, _, _, _, _), _) =
    try
      Some (snd (Ocsigen_lib.sep '|' s))
    with Not_found -> None
    
  let get_volatile_data_session_name ~session:(_, (s, _, _, _), _) =
    try
      Some (snd (Ocsigen_lib.sep '|' s))
    with Not_found -> None

  let get_persistent_data_session_name ~session:(_, (s, _, _, _)) =
    try
      Some (snd (Ocsigen_lib.sep '|' s))
    with Not_found -> None

  let set_service_session_timeout ~session:(_, (_, _, _, r, _), _) t = 
    match t with
    | None -> r := TNone
    | Some t -> r := TSome t

  let set_volatile_data_session_timeout ~session:(_, (_, _, r, _), _) t = 
    match t with
    | None -> r := TNone
    | Some t -> r := TSome t

  let set_persistent_data_session_timeout
      ~session:(cookie, (fullsessname, exp, _, sessgrp)) t = 
    let ti = match t with
    | None -> TNone
    | Some t -> TSome t
    in
    Ocsipersist.add
      persistent_cookies_table 
      cookie
      (fullsessname, exp, ti, sessgrp)

  let get_service_session_timeout ~session:(_, (_, _, _, r, _), _) = 
    !r

  let get_volatile_data_session_timeout ~session:(_, (_, _, r, _), _) = 
    !r

  let get_persistent_data_session_timeout ~session:(_, (_, _, r, _)) = 
    r


  let unset_service_session_timeout ~session:(_, (_, _, _, r, _), _) = 
    r := TGlobal

  let unset_volatile_data_session_timeout ~session:(cookie, (_, _, r, _), _) =
    r := TGlobal

  let unset_persistent_data_session_timeout 
      ~session:(cookie, (fullsessname, exp, _, sessgrp)) = 
    Ocsipersist.add
      persistent_cookies_table 
      cookie
      (fullsessname, exp, TGlobal, sessgrp)


  (** Iterator on service sessions *)
  let iter_service_sessions ?sp f = 
    let sitedata = find_sitedata "Admin.iter_service_sessions" sp in
    iter_service_sessions sitedata f
  
  (** Iterator on data sessions *)
  let iter_volatile_data_sessions ?sp f = 
    let sitedata = find_sitedata "Admin.iter_volatile_data_sessions" sp in
    iter_data_sessions sitedata f
  
  (** Iterator on persistent sessions *)
  let iter_persistent_data_sessions = iter_persistent_sessions

  (** Iterator on service sessions *)
  let fold_service_sessions ?sp f beg = 
  let sitedata = find_sitedata "Admin.fold_service_sessions" sp in
  fold_service_sessions sitedata f beg

  (** Iterator on data sessions *)
  let fold_volatile_data_sessions ?sp f beg = 
    let sitedata = find_sitedata "Admin.fold_volatile_data_sessions" sp in
    fold_data_sessions sitedata f beg

  (** Iterator on persistent sessions *)
  let fold_persistent_data_sessions = fold_persistent_sessions

end



(*****************************************************************************)
(* Exploration *)

let number_of_service_sessions = number_of_service_sessions

let number_of_volatile_data_sessions = number_of_data_sessions

let number_of_tables = Eliommod.number_of_tables

let number_of_table_elements = number_of_table_elements

let number_of_persistent_data_sessions = number_of_persistent_sessions

let number_of_persistent_tables = number_of_persistent_tables
  (* One table is the main table of sessions *)

let number_of_persistent_table_elements () =
  number_of_persistent_table_elements ()

(*****************************************************************************)
let sp_of_esp = Ocsigen_lib.id
let esp_of_sp = Ocsigen_lib.id
