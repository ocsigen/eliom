(* Ocsigen
 * http://www.ocsigen.org
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

include Eliom_service_base

exception Wrong_session_table_for_CSRF_safe_coservice

let eliom_appl_answer_content_type = "application/x-eliom"

(* If there is a client side process, we do an XHR with tab cookies *)
let xhr_with_cookies s =
  if is_external s then
    None
  else
    match s.send_appl_content with
    | XAlways ->
      Some None
    | XNever ->
      None (* actually this will be tested again later in
              get_onload_form_creators *)
    | XSame_appl (_, tmpl) ->
      Some tmpl (* Some an = current_page_appl_name *)
(* for now we do not know the current_page_appl_name.  We will know it
   only after calling send.  In case it is not the same name, we will
   not send the onload_form_creator_info. *)

let register_eliom_module name f =
  Ocsigen_loader.set_module_init_function name f

exception Unregistered_CSRF_safe_coservice

let register_delayed_get_or_na_coservice ~sp (k, scope, secure) =
  let f =
    try
      let table =
        !(Eliom_state.get_session_service_table_if_exists ~sp
            ~scope:(scope:>Eliom_common.user_scope) ?secure ())
      in
      Eliom_lib.Int.Table.find
        k table.Eliom_common.csrf_get_or_na_registration_functions
    with Not_found ->
      let table = Eliom_state.get_global_table () in
      try
        Eliom_lib.Int.Table.find
          k table.Eliom_common.csrf_get_or_na_registration_functions
      with Not_found ->
        raise Unregistered_CSRF_safe_coservice
  in
  f ~sp

let register_delayed_post_coservice ~sp (k, scope, secure) getname =
  let f =
    try
      let table =
        !(Eliom_state.get_session_service_table_if_exists ~sp
            ~scope:(scope:>Eliom_common.user_scope) ?secure ())
      in
      Eliom_lib.Int.Table.find
        k table.Eliom_common.csrf_post_registration_functions
    with Not_found ->
      let table = Eliom_state.get_global_table () in
      try
        Eliom_lib.Int.Table.find
          k table.Eliom_common.csrf_post_registration_functions
      with
        Not_found -> raise Unregistered_CSRF_safe_coservice
  in
  f ~sp getname

let set_delayed_get_or_na_registration_function tables k f =
  tables.Eliom_common.csrf_get_or_na_registration_functions <-
    Eliom_lib.Int.Table.add k f
      tables.Eliom_common.csrf_get_or_na_registration_functions

let set_delayed_post_registration_function tables k f =
  tables.Eliom_common.csrf_post_registration_functions <-
    Eliom_lib.Int.Table.add k f
      tables.Eliom_common.csrf_post_registration_functions

let remove_service
    table
    (type m) (type a)
    (service : (_, _, m, a, _, _, _, _, _, _, _) t) =
  match info service with
  | Attached attser ->
    let key_kind = which_meth_untyped service in
    let attserget = get_name attser in
    let attserpost = post_name attser in
    let sgpt = get_params_type service in
    let sppt = post_params_type service in
    Eliom_route.remove_service
      table
      (sub_path attser)
      {Eliom_common.key_state = (attserget, attserpost);
       Eliom_common.key_meth = key_kind}
      (if attserget = Eliom_common.SAtt_no
       || attserpost = Eliom_common.SAtt_no
       then
         Eliom_parameter.(
           anonymise_params_type sgpt,
           anonymise_params_type sppt)
       else (0, 0))
  | Nonattached naser ->
    let na_name = na_name naser in
    Eliom_route.remove_naservice table na_name

let unregister ?scope ?secure
    (type m) (service : (_, _, m, _, _, _, _, _, _, _, _) t) =
  let sp = Eliom_common.get_sp_option () in
  match scope with
  | None
  | Some `Site ->
    let table =
      match sp with
      | None ->
        (match Eliom_common.global_register_allowed () with
         | Some get_current_sitedata ->
           let sitedata = get_current_sitedata () in
           sitedata.Eliom_common.global_services
         | _ ->
           raise
             (Eliom_common.Eliom_site_information_not_available
                "unregister"))
      | Some sp ->
        Eliom_state.get_global_table ()
    in
    remove_service table service
  | Some (#Eliom_common.user_scope as scope) ->
    match sp with
    | None ->
      raise
        (failwith
           "Unregistering service for non global scope \
            must be done during a request")
    | Some sp ->
      let table =
        !(Eliom_state.get_session_service_table ~sp ?secure ~scope ())
      in
      remove_service table service

let has_client_fun _ = false
