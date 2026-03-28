open Lwt.Syntax

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
  let secure = Common.get_secure ~secure_o ~sitedata () in
  if secure
  then
    let _, _, c = secure_ci in
    c, true
  else cookie_info, false

let close_persistent_state2 ~(scope : [< Common.user_scope]) sitedata sg v
  =
  (* check *)
  match scope with
  | `Session_group _ ->
      Eliommod_sessiongroups.Pers.remove_group ~cookie_level:`Session sitedata
        sg
  | _ ->
      Eliommod_sessiongroups.Pers.close_persistent_session2
        ~cookie_level:(Common.cookie_level_of_user_scope scope)
        sitedata sg v

(* close current persistent session *)
let close_persistent_state ~scope ~secure_o ?sp () =
  let sp = Common.sp_of_option sp in
  catch
    (fun () ->
       let cookie_level = Common.cookie_level_of_user_scope scope in
       let (_, _, cookie_info), secure_ci =
         Common.get_cookie_info sp cookie_level
       in
       let sitedata = Request_info.get_sitedata_sp ~sp in
       let cookie_info, secure =
         compute_cookie_info sitedata secure_o secure_ci cookie_info
       in
       let full_st_name =
         Common.make_full_state_name ~sp ~secure ~scope
       in
       Lazy.force
         (Common.Full_state_name_table.find full_st_name !cookie_info)
       >>= fun (_, ior) ->
       match !ior with
       | Common.SC c ->
           close_persistent_state2
             ~scope:(scope :> Common.user_scope)
             sp.Common.sp_sitedata
             !(c.Common.pc_session_group)
             Common.(Hashed_cookies.to_string c.pc_hvalue)
           >>= fun () ->
           ior := Common.SCNo_data;
           return_unit
       | _ -> return_unit)
    (function Not_found -> return_unit | e -> fail e)

let fullsessgrp ~cookie_level ~sp session_group =
  Eliommod_sessiongroups.make_persistent_full_group_name ~cookie_level
    (Common.get_site_dir_string sp.Common.sp_sitedata)
    session_group

let rec find_or_create_persistent_cookie_
          ?set_max_in_group
          ?set_session_group
          ~cookie_scope
          ~secure_o
          ~sp
          ()
  =
  (* if it exists, do not create it, but returns its value *)
  let cookie_level = Common.cookie_level_of_user_scope cookie_scope in
  let new_persistent_cookie sitedata full_state_name =
    let* set_session_group =
      match cookie_scope with
      | `Client_process n ->
          (* We create a group whose name is the
                   browser session cookie
                   and put the tab session into it. *)
          let* r =
            find_or_create_persistent_cookie_
              ~set_max_in_group:
                (fst
                   sitedata
                     .Common.max_persistent_data_tab_sessions_per_group)
              ~cookie_scope:(`Session n) ~secure_o ~sp ()
          in
          Lwt.return_some Common.(Hashed_cookies.to_string r.pc_hvalue)
      | _ -> Lwt.return set_session_group
    in
    let fullsessgrp = fullsessgrp ~cookie_level ~sp set_session_group in
    let c = Eliommod_cookies.make_new_session_id () in
    let hc = Common.Hashed_cookies.hash c in
    let hc_string = Common.Hashed_cookies.to_string hc in
    (* We do not need to verify if it already exists.
     make_new_session_id does never generate twice the same cookie. *)
    let usertimeout =
      ref Common.TGlobal
      (* See global table *)
    in
    let* () =
      Eliommod_cookies.Persistent_cookies.add hc_string
        { Eliommod_cookies.full_state_name
        ; expiry = None
        ; (* exp on server - We'll change it later *)
          timeout = Common.TGlobal
        ; session_group = fullsessgrp }
    in
    Eliommod_sessiongroups.Pers.add ?set_max:set_max_in_group
      (fst sitedata.Common.max_persistent_data_sessions_per_group)
      hc_string fullsessgrp
    >>= fun l ->
    Lwt_list.iter_p
      (close_persistent_state2
         ~scope:(cookie_scope :> Common.user_scope)
         sitedata None)
      l
    >>= fun () ->
    Lwt.return
      { Common.pc_hvalue = hc
      ; Common.pc_set_value = Some c
      ; Common.pc_timeout = usertimeout
      ; Common.pc_cookie_exp =
          ref (Common.default_client_cookie_exp ()) (* exp on client *)
      ; Common.pc_session_group = ref fullsessgrp }
  in
  let (_, _, cookie_info), secure_ci =
    Common.get_cookie_info sp cookie_level
  in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Common.make_full_state_name ~sp ~secure ~scope:cookie_scope
  in
  catch
    (fun () ->
       Lazy.force
         (Common.Full_state_name_table.find full_st_name !cookie_info)
       >>= fun (_old, ior) ->
       match !ior with
       | Common.SCData_session_expired
         (* We do not trust the value sent by the client,
             for security reasons *)
       | Common.SCNo_data ->
           new_persistent_cookie sitedata full_st_name >>= fun v ->
           ior := Common.SC v;
           return v
       | Common.SC v -> return v)
    (function
      | Not_found ->
          new_persistent_cookie sitedata full_st_name >>= fun v ->
          cookie_info :=
            Common.Full_state_name_table.add full_st_name
              (Lazy.from_val (return (None, ref (Common.SC v))))
              !cookie_info;
          return v
      | e -> fail e)

let find_or_create_persistent_cookie
      ?set_session_group
      ~cookie_scope
      ~secure_o
      ?sp
      ()
  =
  let sp = Common.sp_of_option sp in
  find_or_create_persistent_cookie_ ?set_session_group ~cookie_scope ~secure_o
    ~sp ()

let find_or_create_persistent_cookie =
  (find_or_create_persistent_cookie
    : ?set_session_group:string
      -> cookie_scope:Common.cookie_scope
      -> secure_o:bool option
      -> ?sp:Common.server_params
      -> unit
      -> Common.one_persistent_cookie_info Lwt.t
    :> ?set_session_group:string
       -> cookie_scope:[< Common.cookie_scope]
       -> secure_o:bool option
       -> ?sp:Common.server_params
       -> unit
       -> Common.one_persistent_cookie_info Lwt.t)

let find_persistent_cookie_only ~cookie_scope ~secure_o ?sp () =
  (* If the cookie does not exist, do not create it, raise Not_found.
     Returns the cookie info for the cookie *)
  let sp = Common.sp_of_option sp in
  let cookie_level = Common.cookie_level_of_user_scope cookie_scope in
  let (_, _, cookie_info), secure_ci =
    Common.get_cookie_info sp cookie_level
  in
  let sitedata = Request_info.get_sitedata_sp ~sp in
  let cookie_info, secure =
    compute_cookie_info sitedata secure_o secure_ci cookie_info
  in
  let full_st_name =
    Common.make_full_state_name ~sp ~secure ~scope:cookie_scope
  in
  Lazy.force (Common.Full_state_name_table.find full_st_name !cookie_info)
  >>= fun (_, ior) ->
  match !ior with
  | Common.SCNo_data -> raise Not_found
  | Common.SCData_session_expired ->
      raise Common.Eliom_Session_expired
  | Common.SC v -> return v
