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


let get_user_agent ~sp:(ri,_,_) = ri.ri_user_agent
let get_full_url ~sp:(ri,_,_) = ri.ri_url_string
let get_ip ~sp:(ri,_,_) = ri.ri_ip
let get_inet_addr ~sp:(ri,_,_) = ri.ri_inet_addr
let get_get_params ~sp:(ri,_,_) = force ri.ri_get_params
let get_all_get_params ~sp:(_,si,_) = si.si_all_get_params
let get_get_params_string ~sp:(ri,_,_) = ri.ri_get_params_string
let get_post_params ~sp:(ri,_,_) = force ri.ri_post_params
let get_all_post_params ~sp:(_,si,_) = si.si_all_post_params
let get_current_path_string ~sp:(ri,_,_) = ri.ri_path_string
let get_current_path ~sp:(ri,_,_) = ri.ri_path
let get_hostname ~sp:(ri,_,_) = ri.ri_host
let get_port ~sp:(ri,_,_) = ri.ri_port
let get_other_get_params ~sp:(_,si,_) = si.si_other_get_params
let get_suffix ~sp:(_,_,(_,_,_,_,s)) = s
let get_exn ~sp:(_,si,_) = si.si_exn
let get_config_file_charset ~sp:(_,si,_) = si.si_config_file_charset
let get_cookies ~sp:(ri,_,_) = force ri.ri_cookies
let get_cookie ~sp:(_,si,_) = !(si.si_cookie)
let get_persistent_cookie ~sp:(_,si,_) = !(si.si_persistent_cookie)

let get_default_timeout = Eliommod.get_default_timeout
let set_global_timeout ?sp s = 
  match sp with
  | Some (_, _, (working_dir, _, _, _, _)) ->
      Eliommod.set_global_timeout working_dir s
  | None ->
      match global_register_allowed () with
      | Some get_current_hostdir ->
          Eliommod.set_global_timeout (snd (get_current_hostdir ())) s
      | _ -> raise (Eliom_function_forbidden_outside_site_loading 
                      "set_global_timeout")

let get_global_timeout ?sp () = 
  match sp with
  | Some (_, _, (working_dir, _, _, _, _)) ->
      Eliommod.find_global_timeout working_dir
  | None ->
      match global_register_allowed () with
      | Some get_current_hostdir ->
          Eliommod.find_global_timeout (snd (get_current_hostdir ()))
      | _ -> raise (Eliom_function_forbidden_outside_site_loading
                      "get_global_timeout")

let get_default_persistent_timeout = Eliommod.get_default_persistent_timeout

let set_global_persistent_timeout ?sp s = 
  match sp with
  | Some (_, _, (working_dir, _, _, _, _)) ->
      Eliommod.set_global_persistent_timeout working_dir s
  | None ->
      match global_register_allowed () with
      | Some get_current_hostdir ->
          Eliommod.set_global_persistent_timeout
            (snd (get_current_hostdir ())) s
      | _ -> raise (Eliom_function_forbidden_outside_site_loading
                      "set_global_persistent_timeout")

let get_global_persistent_timeout ?sp () =
  match sp with
  | Some (_, _, (working_dir, _, _, _, _)) ->
      Eliommod.find_global_persistent_timeout working_dir
  | None ->
      match global_register_allowed () with
      | Some get_current_hostdir ->
          Eliommod.find_global_persistent_timeout
            (snd (get_current_hostdir ()))
      | _ -> raise (Eliom_function_forbidden_outside_site_loading
                      "get_global_persistent_timeout")


let set_user_timeout ~sp:(_,_,(_,_,_,(tor,_,_,_),_)) t = tor := Some t
let unset_user_timeout ~sp:(_,_,(_,_,_,(tor,_,_,_),_)) = tor := None
let get_user_timeout ~sp:(_,_,(working_dir,_,_,(tor,_,_,_),_)) = 
  match !tor with
  | None -> Eliommod.find_global_timeout working_dir
  | Some t -> t

let set_user_expdate ~sp:(_,_,(_,_,_,(_,exp,_,_),_)) t = exp := t
let get_user_expdate ~sp:(_,_,(working_dir,_,_,(_,exp,_,_),_)) = !exp

let set_user_persistent_timeout ~sp:(_,_,(_,_,_,(_,_,tor,_),_)) t = tor := Some t
let unset_user_persistent_timeout ~sp:(_,_,(_,_,_,(_,_,tor,_),_)) = tor := None
let get_user_persistent_timeout ~sp:(_,_,(working_dir,_,_,(_,_,tor,_),_)) = 
  match !tor with
  | None -> Eliommod.find_global_persistent_timeout working_dir
  | Some t -> t

let set_user_persistent_expdate ~sp:(_,_,(_,_,_,(_,_,_,exp),_)) t = exp := t
let get_user_persistent_expdate ~sp:(_,_,(working_dir,_,_,(_,_,_,exp),_)) = !exp

let get_working_dir ~sp:(_,_,(working_dir,_,_,(_,_,_,exp),_)) = working_dir
let get_ri ~sp:(ri,_,(_,_,_,(_,_,_,exp),_)) = ri

let get_tmp_filename fi = fi.tmp_filename
let get_filesize fi = fi.filesize
let get_original_filename fi = fi.original_filename

let set_exn_handler ?sp h = 
  match sp with
  | Some (_, _, (working_dir, _, _, _, _)) ->
      set_site_handler working_dir h
  | None ->
      match global_register_allowed () with
      | Some get_current_hostdir ->
          set_site_handler (snd (get_current_hostdir ())) h
      | _ -> raise (Eliom_function_forbidden_outside_site_loading
                      "set_site_handler")

let get_global_tables
    ~sp:((_, _, (_, (globtables, _, _), _, _, _)) : server_params) = 
  globtables

let get_session_tables ~sp:((_, _, (_, _, sesstab, _, _)) : server_params) = 
  sesstab

(*****************************************************************************)
(** {2 persistent sessions} *)

open Ocsipersist

type 'a persistent_table = (int64 * 'a) Ocsipersist.table

let create_persistent_table = create_persistent_table

let get_persistent_data ~table ~sp =
  match get_persistent_cookie sp with
  | Some (c, k) -> 
      (catch
         (fun () ->
           find table c >>=
           (fun (k2, v) ->
             if k2 = k
             then return (Some v)
             else begin
               remove table c >>= (* It was an old cookie. I don't trust it! *)
               (fun () -> return None)
             end))
         (fun _ -> return None)) (* ?? If an error occurs, assume no data *)
         (* function 
           | Not_found -> return None
           | e -> fail e) *)
  | None -> return None

let set_persistent_data ~table ~sp ~value =
  create_persistent_cookie sp >>=
  (fun (c, k) -> add table c (k, value))

let remove_persistent_data ~table ~sp =
  match get_persistent_cookie sp with
  | Some (c,k) -> remove table c
  | None -> return ()

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


let get_session_data ~table ~sp =
  match (get_cookie sp) with
  | Some c -> 
      (try
        Some (Cookies.find table c)
      with _ -> None)
  | None -> None

let set_session_data ~table ~sp ~value =
  let c = create_cookie sp in
  Cookies.replace table c value

let remove_session_data ~table ~sp =
  match get_cookie sp with
  | Some c -> Cookies.remove table c
  | None -> ()

(*****************************************************************************)
(** Close a session *)
let close_persistent_session ~sp:(_,si,_) =
  (match !(si.si_persistent_cookie) with
  | Some (c, _) -> 
      catch
        (fun () -> remove_from_all_persistent_tables c)
        (fun _ -> return ())
  | None -> return ()) >>=
  (fun () ->
    si.si_persistent_cookie := None;
    return ())

let close_volatile_session ~sp:((_, si, (_,(_,_,_),sesstab,_,_)) as sp) = 
  remove_session sp;
  sesstab := empty_tables ();
  si.si_cookie := None

let close_session ~sp =
  close_volatile_session sp;
  close_persistent_session sp




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
