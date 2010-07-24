(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_persess.ml
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
(** Persistant data tables                                                   *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(* Persistent sessions: *)

open Lwt


let compute_cookie_info secure secure_ci cookie_info =
  let secure = match secure with
    | None -> true
    | Some s -> s
  in
  if secure 
  then match secure_ci with
    | None (* not ssl *) -> cookie_info
    | Some (_, _, c) -> c
  else cookie_info


let perstables = Eliom_common.perstables
let persistent_cookies_table = Eliom_common.persistent_cookies_table

let number_of_persistent_tables () =
  List.length !perstables

let number_of_persistent_table_elements () =
  List.fold_left
    (fun thr t ->
      thr >>= fun l -> 
      Ocsipersist.length (Ocsipersist.open_table t) >>= fun e -> 
      return ((t, e)::l)) (return []) !perstables

(* close a persistent session by cookie *)
let close_persistent_session2 fullsessgrp cookie =
  catch
    (fun () ->
      Ocsipersist.remove 
        (Lazy.force persistent_cookies_table) cookie >>= fun () ->
      Eliommod_sessiongroups.Pers.remove cookie fullsessgrp >>= fun () ->
      Eliom_common.remove_from_all_persistent_tables cookie
    )
    (function
      | Not_found -> return ()
      | e -> fail e)

let close_persistent_group fullsessgrp =
(*VVV VERIFY concurrent access *)
  Lwt.catch
    (fun () ->
       Eliommod_sessiongroups.Pers.find fullsessgrp >>= fun cooklist ->
       Lwt_util.iter (close_persistent_session2 None) cooklist >>= fun () ->
       Eliommod_sessiongroups.Pers.remove_group fullsessgrp)
    (function Not_found -> Lwt.return () | e -> Lwt.fail e)

(* close current persistent session *)
let close_persistent_session ?session_name ?(level = `Browser) ~secure ~sp () =
  catch
    (fun () ->
      let cookie_level = Eliom_common.cookie_level_of_level level in
      let fullsessname = 
        Eliom_common.make_fullsessname ~sp cookie_level session_name 
      in
      let ((_, _, cookie_info), secure_ci) =
        Eliom_common.get_cookie_info sp cookie_level
      in
      let cookie_info = compute_cookie_info secure secure_ci cookie_info in
      Lazy.force
        (Eliom_common.Fullsessionname_Table.find fullsessname !cookie_info)
      >>= fun (_, ior) ->

      match !ior with
      | Eliom_common.SC c ->
        ((if level = `Group
          then
            close_persistent_group !(c.Eliom_common.pc_session_group)
          else
            close_persistent_session2 !(c.Eliom_common.pc_session_group)
              c.Eliom_common.pc_value)
         >>= fun () ->
         ior := Eliom_common.SCNo_data;
         return ())
      | _ -> return ()
    )
    (function
      | Not_found -> return ()
      | e -> fail e)


let fullsessgrp ~level ~sp session_group =
  Eliommod_sessiongroups.make_persistent_full_group_name
    ~level
    sp.Eliom_common.sp_request.Ocsigen_extensions.request_info
    sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
    session_group


let rec find_or_create_persistent_cookie_
    ?set_max_in_group ?set_session_group ?session_name
    ?(cookie_level = `Browser) ~secure ~sp () =
  (* if it exists, do not create it, but returns its value *)


  let new_persistent_cookie sitedata fullsessname =

    (if cookie_level = `Tab
     then begin (* We create a group whose name is the
                   browser session cookie 
                   and put the tab session into it. *)
       find_or_create_persistent_cookie_
         ~set_max_in_group:
         (fst sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group)
         ?session_name
         ~cookie_level:`Browser
         ~secure
         ~sp
         () >>= fun r -> Lwt.return (Some r.Eliom_common.pc_value)
     end
     else Lwt.return set_session_group) >>= fun set_session_group ->

    let fullsessgrp = fullsessgrp ~level:(cookie_level :> Eliom_common.level)
      ~sp set_session_group
    in

    let c = Eliommod_cookies.make_new_session_id () in
  (* We do not need to verify if it already exists.
     make_new_session_id does never generate twice the same cookie. *)
    let usertimeout = ref Eliom_common.TGlobal (* See global table *) in
    Ocsipersist.add
      (Lazy.force persistent_cookies_table) c
      (fullsessname,
       None (* Some 0. *) (* exp on server - We'll change it later *),
       Eliom_common.TGlobal (* timeout - see global config *),
       fullsessgrp)
    >>= fun () ->
    Eliommod_sessiongroups.Pers.add
      ?set_max:set_max_in_group
      (fst sitedata.Eliom_common.max_persistent_data_sessions_per_group)
      c fullsessgrp >>= fun l ->
    Lwt_util.iter
      (close_persistent_session2 None) l >>= fun () ->
    return {Eliom_common.pc_value= c;
            Eliom_common.pc_timeout= usertimeout;
            Eliom_common.pc_cookie_exp=
        ref Eliom_common.CENothing (* exp on client *);
            Eliom_common.pc_session_group= ref fullsessgrp
           }
  in

  let fullsessname = 
    Eliom_common.make_fullsessname ~sp cookie_level session_name 
  in
  let ((_, _, cookie_info), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_level
  in
  let cookie_info = compute_cookie_info secure secure_ci cookie_info in
  catch
    (fun () ->
      Lazy.force
        (Eliom_common.Fullsessionname_Table.find fullsessname !cookie_info)
      >>= fun (old, ior) ->
      match !ior with
      | Eliom_common.SCData_session_expired
          (* We do not trust the value sent by the client,
             for security reasons *)
      | Eliom_common.SCNo_data ->
        new_persistent_cookie
          sp.Eliom_common.sp_sitedata
          fullsessname >>= fun v ->
        ior := Eliom_common.SC v;
        return v
      | Eliom_common.SC v -> return v)
    (function
      | Not_found ->
        (new_persistent_cookie
           sp.Eliom_common.sp_sitedata
           fullsessname >>= fun v ->
         cookie_info :=
           Eliom_common.Fullsessionname_Table.add
           fullsessname
           (Lazy.lazy_from_val (return (None, ref (Eliom_common.SC v))))
           !cookie_info;
         return v)
      | e -> fail e)

let find_or_create_persistent_cookie
    ?set_session_group ?session_name ?cookie_level ~secure ~sp () =
  find_or_create_persistent_cookie_
    ?set_session_group ?session_name ?cookie_level ~secure ~sp ()


let find_persistent_cookie_only ?session_name
    ?(cookie_level = `Browser) ~secure ~sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let fullsessname = 
    Eliom_common.make_fullsessname ~sp cookie_level session_name 
  in
  let ((_, _, cookie_info), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_level
  in
  let cookie_info = compute_cookie_info secure secure_ci cookie_info in
  Lazy.force (Eliom_common.Fullsessionname_Table.find fullsessname !cookie_info)
  >>= fun (_, ior) ->
  match !ior with
  | Eliom_common.SCNo_data -> raise Not_found
  | Eliom_common.SCData_session_expired ->
      raise Eliom_common.Eliom_Session_expired
  | Eliom_common.SC v -> return v






