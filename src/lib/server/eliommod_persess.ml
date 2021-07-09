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
(** Persistent data tables                                                   *)
(*****************************************************************************)
(*****************************************************************************)


(*****************************************************************************)
(* Persistent sessions: *)

open Lwt


let compute_cookie_info sitedata secure_o secure_ci cookie_info =
  let secure = Eliom_common.get_secure ~secure_o ~sitedata () in
  if secure
  then let (_, _, c) = secure_ci in c, true
  else cookie_info, false


let perstables = Eliom_common.perstables

module Persistent_cookies = Eliom_common.Persistent_cookies

let number_of_persistent_tables () =
  List.length !perstables

let number_of_persistent_table_elements () =
  List.fold_left
    (fun thr t ->
      thr >>= fun l ->
      Ocsipersist.open_table t >>= fun table ->
      Ocsipersist.length table >>= fun e ->
      return ((t, e)::l)) (return_nil) !perstables

let close_persistent_state2
    ~(scope : [< Eliom_common.user_scope ]) sitedata sg v =
(* check *)
  match scope with
    | `Session_group _ ->
      Eliommod_sessiongroups.Pers.remove_group
        ~cookie_level:`Session
        sitedata
        sg
    | _ ->
      Eliommod_sessiongroups.Pers.close_persistent_session2
        ~cookie_level:(Eliom_common.cookie_level_of_user_scope scope)
        sitedata
        sg
        v

(* close current persistent session *)
let close_persistent_state ~scope ~secure_o ?sp () =
  let sp = Eliom_common.sp_of_option sp in
  catch
    (fun () ->
      let cookie_level = Eliom_common.cookie_level_of_user_scope scope in
      let ((_, _, cookie_info), secure_ci) =
        Eliom_common.get_cookie_info sp cookie_level
      in
      let sitedata = Eliom_request_info.get_sitedata_sp sp in
      let cookie_info, secure =
        compute_cookie_info sitedata secure_o secure_ci cookie_info
      in
      let full_st_name = Eliom_common.make_full_state_name ~sp ~secure ~scope in
      Lazy.force
        (Eliom_common.Full_state_name_table.find full_st_name !cookie_info)
      >>= fun (_, ior) ->

      match !ior with
      | Eliom_common.SC c ->
        (close_persistent_state2
           ~scope:(scope :> Eliom_common.user_scope)
           sp.Eliom_common.sp_sitedata
           !(c.Eliom_common.pc_session_group)
           (Eliom_common.(Hashed_cookies.to_string c.pc_hvalue)))
        >>= fun () ->
        ior := Eliom_common.SCNo_data;
        return_unit
      | _ -> return_unit
    )
    (function
      | Not_found -> return_unit
      | e -> fail e)


let fullsessgrp ~cookie_level ~sp session_group =
  Eliommod_sessiongroups.make_persistent_full_group_name
    ~cookie_level
    sp.Eliom_common.sp_sitedata.Eliom_common.site_dir_string
    session_group


let rec find_or_create_persistent_cookie_
    ?set_max_in_group ?set_session_group ~cookie_scope ~secure_o ~sp () =
  (* if it exists, do not create it, but returns its value *)
  let cookie_level = Eliom_common.cookie_level_of_user_scope cookie_scope in

  let new_persistent_cookie sitedata full_st_name =

    let%lwt set_session_group =
      match cookie_scope with
        | `Client_process n ->
          begin (* We create a group whose name is the
                   browser session cookie
                   and put the tab session into it. *)
            let%lwt r = find_or_create_persistent_cookie_
              ~set_max_in_group:
              (fst sitedata.Eliom_common.max_persistent_data_tab_sessions_per_group)
              ~cookie_scope:(`Session n)
              ~secure_o
              ~sp
              () in
            Lwt.return_some Eliom_common.(Hashed_cookies.to_string r.pc_hvalue)
          end
        | _  -> Lwt.return set_session_group in

    let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in

    let c = Eliommod_cookies.make_new_session_id () in
    let hc = Eliom_common.Hashed_cookies.hash c in
    let hc_string = Eliom_common.Hashed_cookies.to_string hc in
  (* We do not need to verify if it already exists.
     make_new_session_id does never generate twice the same cookie. *)
    let usertimeout = ref Eliom_common.TGlobal (* See global table *) in
    Persistent_cookies.add
      hc_string
      (full_st_name,
       None (* Some 0. *) (* exp on server - We'll change it later *),
       Eliom_common.TGlobal (* timeout - see global config *),
       fullsessgrp)
    >>= fun () ->
    Eliommod_sessiongroups.Pers.add
      ?set_max:set_max_in_group
      (fst sitedata.Eliom_common.max_persistent_data_sessions_per_group)
      hc_string fullsessgrp >>= fun l ->
    Lwt_list.iter_p (close_persistent_state2
                     ~scope:(cookie_scope :> Eliom_common.user_scope)
                     sitedata None) l
    >>= fun () ->
    Lwt.return
      { Eliom_common.pc_hvalue= hc;
        Eliom_common.pc_set_value= `Set c;
        Eliom_common.pc_timeout= usertimeout;
        Eliom_common.pc_cookie_exp =
          ref (Eliom_common.default_client_cookie_exp ()) (* exp on client *);
        Eliom_common.pc_session_group= ref fullsessgrp
      }
  in

  let ((_, _, cookie_info), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_level
  in
  let sitedata = Eliom_request_info.get_sitedata_sp sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Eliom_common.make_full_state_name ~sp ~secure ~scope:cookie_scope in
  catch
    (fun () ->
      Lazy.force
        (Eliom_common.Full_state_name_table.find full_st_name !cookie_info)
      >>= fun (old, ior) ->
      match !ior with
      | Eliom_common.SCData_session_expired
          (* We do not trust the value sent by the client,
             for security reasons *)
      | Eliom_common.SCNo_data ->
        new_persistent_cookie
          sitedata
          full_st_name >>= fun v ->
        ior := Eliom_common.SC v;
        return v
      | Eliom_common.SC v -> return v)
    (function
      | Not_found ->
        (new_persistent_cookie
           sitedata
           full_st_name >>= fun v ->
         cookie_info :=
           Eliom_common.Full_state_name_table.add
           full_st_name
           (Lazy.from_val (return (None, ref (Eliom_common.SC v))))
           !cookie_info;
         return v)
      | e -> fail e)

let find_or_create_persistent_cookie
    ?set_session_group ~cookie_scope ~secure_o ?sp () =
  let sp = Eliom_common.sp_of_option sp in
  find_or_create_persistent_cookie_
    ?set_session_group ~cookie_scope ~secure_o ~sp ()

let find_or_create_persistent_cookie =
  (find_or_create_persistent_cookie :
     ?set_session_group:string ->
   cookie_scope:Eliom_common.cookie_scope ->
   secure_o:bool option ->
   ?sp:Eliom_common.server_params ->
   unit -> Eliom_common.one_persistent_cookie_info Lwt.t
   :>
     ?set_session_group:string ->
   cookie_scope:[< Eliom_common.cookie_scope ] ->
   secure_o:bool option ->
   ?sp:Eliom_common.server_params ->
   unit -> Eliom_common.one_persistent_cookie_info Lwt.t
  )


let find_persistent_cookie_only ~cookie_scope ~secure_o ?sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let sp = Eliom_common.sp_of_option sp in
  let cookie_level = Eliom_common.cookie_level_of_user_scope cookie_scope in
  let ((_, _, cookie_info), secure_ci) =
    Eliom_common.get_cookie_info sp cookie_level
  in
  let sitedata = Eliom_request_info.get_sitedata_sp sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Eliom_common.make_full_state_name ~sp ~secure ~scope:cookie_scope in
  Lazy.force (Eliom_common.Full_state_name_table.find full_st_name !cookie_info)
  >>= fun (_, ior) ->
  match !ior with
  | Eliom_common.SCNo_data -> raise Not_found
  | Eliom_common.SCData_session_expired ->
      raise Eliom_common.Eliom_Session_expired
  | Eliom_common.SC v -> return v
