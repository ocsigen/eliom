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


let get_config () = 
  match global_register_allowed () with
  | Some _ -> !Eliommod.config
  | None -> raise (Eliom_function_forbidden_outside_site_loading "get_config")

let find_site_dir fun_name = function
  | Some sp -> sp.sp_site_dir
  | None ->
      match global_register_allowed () with
      | Some get_current_hostdir -> snd (get_current_hostdir ())
      | _ -> raise (Eliom_function_forbidden_outside_site_loading fun_name)

let find_hostdir fun_name = function
  | Some sp -> ((sp.sp_global_table, 
                 sp.sp_cookie_table, 
                 (sp.sp_remove_sess_data,
                  sp.sp_are_empty_tables)), sp.sp_site_dir)
  | None ->
      match global_register_allowed () with
      | Some get_current_hostdir -> 
          get_current_hostdir ()
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
let get_current_path_string ~sp = sp.sp_ri.ri_path_string
let get_current_path ~sp = sp.sp_ri.ri_path
let get_hostname ~sp = sp.sp_ri.ri_host
let get_port ~sp = sp.sp_ri.ri_port
let get_other_get_params ~sp = sp.sp_si.si_other_get_params
let get_suffix ~sp = sp.sp_suffix
let get_session ~sp = sp.sp_fullsessname
let get_exn ~sp = sp.sp_si.si_exn
let get_config_file_charset ~sp = sp.sp_si.si_config_file_charset
let get_cookies ~sp = force sp.sp_ri.ri_cookies
let get_eliom_cookies ~sp = sp.sp_si.si_session_cookies
let get_eliom_persistent_cookies ~sp = sp.sp_si.si_persistent_session_cookies

let get_eliom_cookie ?session_name ~sp () = 
  try
    let (v, _, _, _, _) = find_cookie_only ?session_name ~sp () in
    Some v
  with Not_found -> None

let get_eliom_persistent_cookie ?session_name ~sp () =
  try
    let (v, _, _, _) = find_persistent_cookie_only ?session_name ~sp () in
    Some v
  with Not_found -> None

let get_default_timeout = Eliommod.get_default_timeout
let set_default_timeout = Eliommod.set_default_timeout
    
let set_global_timeout 
    ?session_name ?sp ?(recompute_expdates = false) timeout = 
  let ((_, cookie_table, (remove_session_data, _)), site_dir) = 
    find_hostdir "set_global_timeout" sp 
  in
  Eliommod.set_global_timeout 
    ~session_name ~recompute_expdates 
    site_dir !remove_session_data cookie_table timeout
  
let get_global_timeout ?session_name ?sp () = 
  let site_dir = find_site_dir "get_global_timeout" sp in
  Eliommod.get_global_timeout ?session_name site_dir

let get_default_persistent_timeout = Eliommod.get_default_persistent_timeout
let set_default_persistent_timeout = Eliommod.set_default_persistent_timeout

let set_global_persistent_timeout 
    ?session_name ?sp ?(recompute_expdates = false) timeout = 
  let site_dir = find_site_dir "set_global_persistent_timeout" sp in
  Eliommod.set_global_persistent_timeout
    ~session_name ~recompute_expdates site_dir timeout

let get_global_persistent_timeout ?session_name ?sp () =
  let site_dir = find_site_dir "get_global_persistent_timeout" sp in
  Eliommod.get_global_persistent_timeout ?session_name site_dir

let set_session_timeout ?session_name ~sp t = 
  let (_, _, tor, _, _) = find_or_create_cookie ?session_name ~sp () in
  tor := Some t

let unset_session_timeout ?session_name ~sp () = 
  try
    let (_, _, tor, _, _) = find_cookie_only ?session_name ~sp () in
    tor := None
  with Not_found -> ()

let get_session_timeout ?session_name ~sp () = 
  try
    let (_, _, tor, _, _) = find_cookie_only ?session_name ~sp () in
    match !tor with
    | None -> Eliommod.get_global_timeout ?session_name sp.sp_site_dir
    | Some t -> t
  with Not_found -> Eliommod.get_global_timeout ?session_name sp.sp_site_dir

let set_cookie_exp_date ?session_name ~sp t = 
  let (_, _, _, _, exp) = find_or_create_cookie ?session_name ~sp () in
  exp := Some t

let get_cookie_exp_date ?session_name ~sp () = 
  try
    let (_, _, _, _, exp) = find_cookie_only ?session_name ~sp () in
    match !exp with
    | None -> None
    | Some t -> t
  with Not_found -> None


let set_persistent_session_timeout ?session_name ~sp t =
  find_or_create_persistent_cookie ?session_name ~sp () >>= fun (_, _, tor, _) ->
  return (tor := Some t)

let unset_persistent_session_timeout ?session_name ~sp () = 
  try
    let (_, _, tor, _) = find_persistent_cookie_only ?session_name ~sp () in
    tor := None
  with Not_found -> ()

let get_persistent_session_timeout ?session_name ~sp () = 
  try
    let (_, _, tor, _) = find_persistent_cookie_only ?session_name ~sp () in
    match !tor with
    | None -> Eliommod.get_global_persistent_timeout
          ~session_name sp.sp_site_dir
    | Some t -> t
  with Not_found -> Eliommod.get_global_persistent_timeout 
      ~session_name sp.sp_site_dir


let set_persistent_cookie_exp_date ?session_name ~sp t = 
  find_or_create_persistent_cookie ?session_name ~sp () >>= fun (_, _, _, exp) ->
  return (exp := Some t)

let get_persistent_cookie_exp_date ?session_name ~sp () = 
  try
    let (_, _, _, exp) = find_persistent_cookie_only ?session_name ~sp () in
    match !exp with
    | None -> None
    | Some t -> t
  with Not_found -> None


let get_site_dir ~sp = sp.sp_site_dir
let get_site_dir_string ~sp = sp.sp_site_dir_string
let get_ri ~sp = sp.sp_ri

let get_tmp_filename fi = fi.tmp_filename
let get_filesize fi = fi.filesize
let get_original_filename fi = fi.original_filename

let set_exn_handler ?sp h = 
  let site_dir = find_site_dir "set_exn_handler" sp in
  set_site_handler site_dir h

let get_global_table ~sp = sp.sp_global_table

(** If the session does not exist, we create it 
   (new cookie, new session service table) *)
let get_session_table ?session_name ~sp () = 
  let (_, t, _, _, _) = find_or_create_cookie ?session_name ~sp () in
  t

(*****************************************************************************)
(** {2 persistent sessions} *)

open Ocsipersist

type 'a persistent_table = (int64 * 'a) Ocsipersist.table

let create_persistent_table = create_persistent_table

let get_persistent_data ?session_name ~table ~sp () =
  catch 
    (fun () ->
      let (c, k, _, _) = find_persistent_cookie_only ?session_name ~sp () in
      Ocsipersist.find table c >>= fun (k2, v) ->
      if k2 = k
      then return (Some v)
      else begin
        (* It was an old cookie. I don't trust it! *)
        Ocsipersist.remove table c >>= fun () -> 
        return None
      end)
    (function _ -> return None)
    (* ?? If an error occurs, assume no data? *)

let set_persistent_data ?session_name ~table ~sp value =
  find_or_create_persistent_cookie ?session_name ~sp () >>= fun (c, k, _, _) ->
  Ocsipersist.add table c (k, value)

let remove_persistent_data ?session_name ~table ~sp () =
  catch
    (fun () ->
      let (c, _, _, _) = find_persistent_cookie_only ?session_name ~sp () in
      Ocsipersist.remove table c)
    (fun _ -> return ())


(*****************************************************************************)
(** {2 session data in memory} *)
type 'a table = 'a Cookies.t

let create_table ?sp () = 
  match sp with
  | None -> 
      (match global_register_allowed () with
      | Some get_current_hostdir -> create_table ()
      | None -> raise (Eliom_function_forbidden_outside_site_loading
                         "create_table"))
  | Some sp -> create_table_during_session sp

let get_session_data ?session_name ~table ~sp () =
  try 
    let (c, _, _, _, _) = find_cookie_only ?session_name ~sp () in
    Some (Cookies.find table c)
  with _ -> None

let set_session_data ?session_name ~table ~sp value =
  let (c, _, _, _, _) = find_or_create_cookie ?session_name ~sp () in
  Cookies.replace table c value

let remove_session_data ?session_name ~table ~sp () =
  try 
    let (c, _, _, _, _) = find_cookie_only ?session_name ~sp () in
    Cookies.remove table c
  with _ -> ()



(*****************************************************************************)
(** Close a session *)
let close_persistent_session = Eliommod.close_persistent_session

let close_volatile_session = Eliommod.close_volatile_session

let close_session ?session_name ~sp () =
  close_volatile_session ?session_name ~sp ();
  close_persistent_session ?session_name ~sp ()

let close_all_volatile_sessions ?session_name ?sp () =
  let ((_, cookie_table, (remove_session_data, _)), site_dir) = 
    find_hostdir "set_global_timeout" sp 
  in
  Eliommod.close_all_volatile_sessions ?session_name
    !remove_session_data cookie_table site_dir

let close_all_persistent_sessions ?session_name ?sp () =
  let site_dir = find_site_dir "close_all_persistent_sessions" sp in
  Eliommod.close_all_persistent_sessions ?session_name site_dir

let close_all_sessions ?session_name ?sp () =
 close_all_volatile_sessions ?session_name ?sp () >>=
 close_all_persistent_sessions ?session_name ?sp


(*****************************************************************************)
(* Exploration *)

let number_of_sessions = number_of_sessions

let number_of_tables = Eliommod.number_of_tables

let number_of_table_elements = number_of_table_elements

let number_of_persistent_sessions = number_of_persistent_sessions

let number_of_persistent_tables = number_of_persistent_tables
  (* One table is the main table of sessions *)

let number_of_persistent_table_elements () =
  number_of_persistent_table_elements ()
