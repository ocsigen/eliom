(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_persess.ml
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

let close_persistent_session2 =
  Eliommod_sessiongroups.Pers.close_persistent_session2

(* close current persistent session *)
let close_persistent_session ~scope ~secure ?sp () =
  let sp = Eliom_common.sp_of_option sp in
  catch
    (fun () ->
      let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
      let fullsessname =
        Eliom_common.make_fullsessname ~sp scope
      in
      let ((_, _, cookie_info), secure_ci) =
        Eliom_common.get_cookie_info sp cookie_scope
      in
      let cookie_info = compute_cookie_info secure secure_ci cookie_info in
      Lazy.force
        (Eliom_common.Fullsessionname_Table.find fullsessname !cookie_info)
      >>= fun (_, ior) ->

      match !ior with
      | Eliom_common.SC c ->
        ((match scope with
          | `Session_group _ ->
            Eliommod_sessiongroups.Pers.remove_group
              ~cookie_scope:`Session
              sp.Eliom_common.sp_sitedata
              !(c.Eliom_common.pc_session_group)
          | `Session _ ->
            close_persistent_session2
              ~cookie_scope:`Session
              sp.Eliom_common.sp_sitedata
              !(c.Eliom_common.pc_session_group)
              c.Eliom_common.pc_value
          | `Client_process _ ->
            close_persistent_session2
              ~cookie_scope:`Client_process
              sp.Eliom_common.sp_sitedata
              !(c.Eliom_common.pc_session_group)
              c.Eliom_common.pc_value)
         >>= fun () ->
         ior := Eliom_common.SCNo_data;
         return ())
      | _ -> return ()
    )
    (function
      | Not_found -> return ()
      | e -> fail e)


let fullsessgrp ~cookie_scope ~sp session_group =
  Eliommod_sessiongroups.make_persistent_full_group_name
    ~cookie_scope
    sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
    session_group


let rec find_or_create_persistent_cookie_
    ?set_max_in_group ?set_session_group ~scope ~secure ~sp () =
  (* if it exists, do not create it, but returns its value *)
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in

  let new_persistent_cookie sitedata fullsessname =

    lwt set_session_group =
      match scope with
	| `Client_process n ->
	  begin (* We create a group whose name is the
                   browser session cookie
                   and put the tab session into it. *)
	    lwt r = find_or_create_persistent_cookie_
              ~set_max_in_group:
              (fst sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group)
              ~scope:(`Session n)
              ~secure
              ~sp
              () in
	    Lwt.return (Some r.Eliom_common.pc_value)
	  end
	| #Eliom_common.user_scope  -> Lwt.return set_session_group in

    let fullsessgrp = fullsessgrp ~cookie_scope ~sp set_session_group in

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
      (close_persistent_session2 ~cookie_scope sitedata None) l
    >>= fun () ->
    Lwt.return
      { Eliom_common.pc_value= c;
        Eliom_common.pc_timeout= usertimeout;
        Eliom_common.pc_cookie_exp =
          ref Eliom_common.CENothing (* exp on client *);
        Eliom_common.pc_session_group= ref fullsessgrp
      }
  in

  let fullsessname =
    Eliom_common.make_fullsessname ~sp scope
  in
  let ((_, _, cookie_info), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_scope
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
    ?set_session_group ~scope ~secure ?sp () =
  let sp = Eliom_common.sp_of_option sp in
  find_or_create_persistent_cookie_
    ?set_session_group ~scope:(scope:>Eliom_common.user_scope) ~secure ~sp ()


let find_persistent_cookie_only ~scope ~secure ?sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let sp = Eliom_common.sp_of_option sp in
  let cookie_scope = Eliom_common.cookie_scope_of_user_scope scope in
  let fullsessname =
    Eliom_common.make_fullsessname ~sp scope
  in
  let ((_, _, cookie_info), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_scope
  in
  let cookie_info = compute_cookie_info secure secure_ci cookie_info in
  Lazy.force (Eliom_common.Fullsessionname_Table.find fullsessname !cookie_info)
  >>= fun (_, ior) ->
  match !ior with
  | Eliom_common.SCNo_data -> raise Not_found
  | Eliom_common.SCData_session_expired ->
      raise Eliom_common.Eliom_Session_expired
  | Eliom_common.SC v -> return v






