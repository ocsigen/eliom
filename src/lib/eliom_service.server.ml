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

let plain_service
    (type m) (type gp) (type gn) (type pp) (type pn) (type gp')
    ?(https = false)
    ~path
    ?keep_nl_params
    ?priority
    ~(meth : (m, gp, gn, pp, pn, _, gp') meth)
    () =
  let get_params, post_params = params_of_meth meth
  and meth = which_meth_internal meth in
  let redirect_suffix = Eliom_parameter.contains_suffix get_params in
  let path =
    (match redirect_suffix with
     | None ->
       path
     | Some _  ->
       path @ [Eliom_common.eliom_suffix_internal_name])
    |> Url.remove_slash_at_beginning
    |> Url.change_empty_list
    |> Url.remove_internal_slash
  in
  let site_dir =
    match Eliom_common.get_sp_option () with
    | Some sp ->
      Eliom_request_info.get_site_dir_sp sp
    | None ->
      (match Eliom_common.global_register_allowed () with
       | Some current_site_data ->
         let sitedata = current_site_data () in
         Eliom_common.add_unregistered sitedata path;
         Eliom_common.get_site_dir sitedata
       | None ->
         raise
           (Eliom_common.Eliom_site_information_not_available "service"))
  in
  let reload_fun = Rf_client_fun in
  main_service
    ~https
    ~prefix:""
    ~path
    ~site_dir
    ~kind:`Service
    ~meth
    ?redirect_suffix
    ?keep_nl_params
    ?priority
    ~get_params
    ~post_params
    ~reload_fun
    ()

let attach
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ?keep_nl_params
    ~fallback
    ~get_params
    ~post_params
    ~meth
    () =
  let is_post = is_post' meth in
  let csrf_scope = default_csrf_scope csrf_scope
  and get_params_type, post_params_type =
    if is_post then
      get_params,
      Eliom_parameter.add_pref_params
        Eliom_common.co_param_prefix post_params
    else
      Eliom_parameter.add_pref_params
        Eliom_common.co_param_prefix get_params,
      post_params
  and k = attached_info fallback in {
    pre_applied_parameters = fallback.pre_applied_parameters;
    get_params_type;
    post_params_type;
    send_appl_content = fallback.send_appl_content;
    service_mark = service_mark ();
    max_use;
    timeout;
    meth;
    kind = `AttachedCoservice;
    info =
      (let att_name =
         if csrf_safe then
           Eliom_common.SAtt_csrf_safe
             (uniqueid (),
              (csrf_scope :> Eliom_common.user_scope),
              csrf_secure)
         else
           match name with
           | None -> Eliom_common.SAtt_anon (new_state ())
           | Some name -> Eliom_common.SAtt_named name
       in
       Attached {
         k with
         get_name = if is_post then k.get_name else att_name;
         post_name = if not is_post then k.post_name else att_name
       });
    https = https || fallback.https;
    keep_nl_params =
      (match keep_nl_params with
       | None -> fallback.keep_nl_params
       | Some k -> k);
    client_fun = None;
    reload_fun = Rf_client_fun
  }

let attach_get =
  attach ~meth:Get' ~post_params:Eliom_parameter.unit

let attach_post
    ?name ?csrf_safe ?csrf_scope ?csrf_secure
    ?max_use ?timeout ?https ?keep_nl_params
    ~fallback ~post_params
    () =
  let get_params = get_params_type fallback in
  attach ~meth:Post'
    ?name ?csrf_safe ?csrf_scope ?csrf_secure
    ?max_use ?timeout ?https ?keep_nl_params
    ~fallback ~post_params ~get_params ()

let attach_get_unsafe = attach_get

let attach_post_unsafe = attach_post

let coservice'
    (type m) (type gp) (type gn) (type pp) (type pn) (type gp')
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ?(keep_nl_params = `Persistent)
    ~(meth : (m, gp, gn, pp, pn, _, unit) meth)
    () =
  let get_params, post_params = params_of_meth meth in
  let meth = which_meth_internal meth
  and is_post = is_post meth in
  let csrf_scope = default_csrf_scope csrf_scope in {
    max_use;
    timeout;
    pre_applied_parameters = Eliom_lib.String.Table.empty, [];
    get_params_type =
      Eliom_parameter.add_pref_params
        Eliom_common.na_co_param_prefix get_params;
    post_params_type = post_params;
    meth;
    kind = `NonattachedCoservice;
    info = Nonattached {
      na_name =
        (if csrf_safe then
           if is_post then
             Eliom_common.SNa_post_csrf_safe
               (uniqueid (),
                (csrf_scope :> Eliom_common.user_scope),
                csrf_secure)
           else
             Eliom_common.SNa_get_csrf_safe
               (uniqueid (),
                (csrf_scope :> Eliom_common.user_scope),
                csrf_secure)
         else
           match name, is_post with
           | None      , true  -> Eliom_common.SNa_post' (new_state ())
           | None      , false -> Eliom_common.SNa_get' (new_state ())
           | Some name , true  -> Eliom_common.SNa_post_ name
           | Some name , false -> Eliom_common.SNa_get_ name);
      keep_get_na_params = true
    };
    https;
    keep_nl_params;
    send_appl_content = XNever;
    service_mark = service_mark ();
    client_fun = None;
    reload_fun = Rf_client_fun
  }

let create
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ?(keep_nl_params = `Persistent)
    ?priority
    (type m) (type gp) (type gn) (type pp) (type pn) (type gp')
    (type att_) (type co_) (type ext_) (type reg_) (type rr)
    ~(meth : (m, gp, gn, pp, pn, _, gp') meth)
    ~(path : (att_, co_, gp') path_option)
    ()
  : (gp, pp, m, att_, co_, ext_, reg_, _, gn, pn, rr) t =
  match path with
  | Path path ->
    plain_service ~https ~keep_nl_params ?priority ~path ~meth ()
  | No_path ->
    coservice'
      ?name ~csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ~https
      ~keep_nl_params ~meth ()

let create_unsafe = create

let create_ocaml = create

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
