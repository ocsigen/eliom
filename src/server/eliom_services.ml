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

open Eliom_pervasives

open Eliom_state
open Eliom_parameters

open Lwt
open Lazy

(* Manipulation of services - this code can be use only on server side. *)

include Eliom_services_base

exception Wrong_session_table_for_CSRF_safe_coservice

(*********)
let need_process_cookies s = not (is_external s)
(* If there is a client side process, we do an XHR with tab cookies *)

let appl_content_capable s =
  match s.send_appl_content with
    | XAlways -> true
    | XNever -> false (* actually this will be tested again later
                         in get_onload_form_creators *)
    | XSame_appl an -> true (* Some an = current_page_appl_name *)
      (* for now we do not know the current_page_appl_name.
         We will know it only after calling send.
         In case it is not the same name, we will not send the
         onload_form_creator_info.
      *)

let xhr_with_cookies s =
  need_process_cookies s && appl_content_capable s

(**********)
let new_state = Eliommod_cookies.make_new_session_id
(* WAS:
  (* This does not need to be cryptographickly robust.
     We just want to avoid the same values when the server is relaunched.
   *)
  let c = ref (Int64.bits_of_float (Unix.gettimeofday ())) in
  fun () ->
    c := Int64.add !c Int64.one ;
    (Printf.sprintf "%x" (Random.int 0xFFFF))^(Printf.sprintf "%Lx" !c)

   But I turned this into cryptographickly robust version
   to implement CSRF-safe services.
*)

let get_or_post_ s = match s.get_or_post with
  | `Get -> Ocsigen_http_frame.Http_header.GET
  | `Post -> Ocsigen_http_frame.Http_header.POST


(*****************************************************************************)
(*****************************************************************************)
(* Registration of static module initialization functions                    *)
(*****************************************************************************)
(*****************************************************************************)

let register_eliom_module name f =
  Ocsigen_loader.set_module_init_function name f

(*****************************************************************************)
(*****************************************************************************)
(* Page registration, handling of links and forms                            *)
(*****************************************************************************)
(*****************************************************************************)

let uniqueid =
  let r = ref (-1) in
  fun () -> r := !r + 1; !r

(****************************************************************************)
(****************************************************************************)

(** Definition of services *)
let service_aux
    ~https
    ~path
    ?redirect_suffix
    ?keep_nl_params
    ?priority
    ~get_params =
  let sp = Eliom_common.get_sp_option () in
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          let path = 
            Url.remove_internal_slash
              (Url.change_empty_list 
                 (Url.remove_slash_at_beginning path))
          in
          let u = service_aux_aux
            ~https
            ~prefix:""
            ~path
            ~site_dir: sitedata.Eliom_common.site_dir
            ~kind:(`Internal `Service)
            ~getorpost:`Get
            ?redirect_suffix
            ?keep_nl_params
            ?priority
            ~get_params
            ~post_params:unit
            ()
          in
          Eliom_common.add_unregistered sitedata path;
          u
      | None ->
          raise (Eliom_common.Eliom_site_information_not_available
                   "service"))
  | Some sp ->
      let path = 
        Url.remove_internal_slash
          (Url.change_empty_list 
             (Url.remove_slash_at_beginning path))
      in
      service_aux_aux
        ~https
        ~prefix:""
        ~path:path
        ~site_dir:(Eliom_request_info.get_site_dir_sp sp)
        ~kind:(`Internal `Service)
        ~getorpost:`Get
        ?redirect_suffix
        ?keep_nl_params
        ?priority
        ~get_params
        ~post_params:unit
        ()

let service
    ?(https = false)
    ~path
    ?keep_nl_params
    ?priority
    ~get_params
    () =
  let suffix = contains_suffix get_params in
  service_aux
    ~https
    ~path:(match suffix with
             | None -> path
             | _ -> path@[Eliom_common.eliom_suffix_internal_name])
    ?keep_nl_params
    ?redirect_suffix:suffix
    ?priority
    ~get_params

let default_csrf_scope = function
    (* We do not use the classical syntax for default
       value. Otherwise, the type for csrf_scope was:
       [< Eliom_common.user_scope > `Session] *)
  | None -> `Session `Default_ref_name
  | Some c -> (c :> [Eliom_common.user_scope])

let coservice
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ~fallback
    ?keep_nl_params
    ~get_params
    () =
  let csrf_scope = default_csrf_scope csrf_scope in
  let `Attached k = fallback.kind in
  (* (match Eliom_common.global_register_allowed () with
     | Some _ -> Eliom_common.add_unregistered k.path;
     | _ -> ()); *)
  {fallback with
   max_use= max_use;
   timeout= timeout;
   get_params_type = add_pref_params Eliom_common.co_param_prefix get_params;
   kind = `Attached
     {k with
      get_name =
         (if csrf_safe
          then Eliom_common.SAtt_csrf_safe (uniqueid (),
                                            (csrf_scope:>Eliom_common.user_scope),
                                            csrf_secure)
          else
            (match name with
               | None -> Eliom_common.SAtt_anon (new_state ())
               | Some name -> Eliom_common.SAtt_named name));
        att_kind = `Internal `Coservice;
        get_or_post = `Get;
     };
   https = https || fallback.https;
   keep_nl_params = match keep_nl_params with 
     | None -> fallback.keep_nl_params | Some k -> k;
 }
(* Warning: here no GET parameters for the fallback.
   Preapply services if you want fallbacks with GET parameters *)


let coservice' 
    ?name 
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ?(keep_nl_params = `Persistent)
    ~get_params
    () =
  let csrf_scope = default_csrf_scope csrf_scope in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered_na n;
  | _ -> () (* Do we accept unregistered non-attached coservices? *)); *)
  (* (* Do we accept unregistered non-attached named coservices? *)
     match sp with
     | None ->
     ...
  *)
        {
(*VVV allow timeout and max_use for named coservices? *)
          max_use= max_use;
          timeout= timeout;
          pre_applied_parameters = String.Table.empty, [];
          get_params_type = 
            add_pref_params Eliom_common.na_co_param_prefix get_params;
          post_params_type = unit;
          kind = `Nonattached
            {na_name =
                (if csrf_safe
                 then Eliom_common.SNa_get_csrf_safe (uniqueid (),
                                                      (csrf_scope:>Eliom_common.user_scope),
                                                      csrf_secure)
                 else
                   match name with
                     | None -> Eliom_common.SNa_get' (new_state ())
                     | Some name -> Eliom_common.SNa_get_ name);
             na_kind = `Get;
            };
          https = https;
          keep_nl_params = keep_nl_params;
          send_appl_content = XNever;
	  service_mark = service_mark ();
        }


(****************************************************************************)
(* Create a service with post parameters in the server *)
let post_service_aux ~https ~fallback 
    ?(keep_nl_params = `None) ?(priority = default_priority) ~post_params =
(* Create a main service (not a coservice) internal, post only *)
(* ici faire une vérification "duplicate parameter" ? *)
  let `Attached k1 = fallback.kind in
  let `Internal k = k1.att_kind in
  {
   pre_applied_parameters = fallback.pre_applied_parameters;
   get_params_type = fallback.get_params_type;
   post_params_type = post_params;
   max_use= None;
   timeout= None;
   kind = `Attached
     {prefix = k1.prefix;
      subpath = k1.subpath;
      fullpath = k1.fullpath;
      att_kind = `Internal k;
      get_or_post = `Post;
      get_name = k1.get_name;
      post_name = Eliom_common.SAtt_no;
      redirect_suffix = false;
      priority;
    };
   https = https;
   keep_nl_params = keep_nl_params;
   send_appl_content = XNever;
   service_mark = service_mark ();
 }

let post_service ?(https = false) ~fallback 
    ?keep_nl_params ?priority ~post_params () =
  (* POST service without POST parameters means
     that the service will answer to a POST request only.
    *)
  let `Attached k1 = fallback.kind in
  let `Internal kind = k1.att_kind in
  let path = k1.subpath in
  let sp = Eliom_common.get_sp_option () in
  let u = post_service_aux
    ~https ~fallback ?keep_nl_params ?priority ~post_params in
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          Eliom_common.add_unregistered (get_current_sitedata ()) path;
          u
      | None ->
          if kind = `Service
          then
            raise (Eliom_common.Eliom_site_information_not_available
                     "post_service")
          else u)
  | _ -> u
(* if the fallback is a coservice, do we get a coservice or a service? *)


let post_coservice
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ~fallback
    ?keep_nl_params
    ~post_params
    () =
  let csrf_scope = default_csrf_scope csrf_scope in
  let `Attached k1 = fallback.kind in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered k1.path;
  | _ -> ()); *)
  {fallback with
   post_params_type = post_params;
   max_use= max_use;
   timeout= timeout;
   kind = `Attached
     {k1 with
        att_kind = `Internal `Coservice;
        get_or_post = `Post;
        post_name = 
         (if csrf_safe
          then Eliom_common.SAtt_csrf_safe (uniqueid (),
                                            (csrf_scope:>Eliom_common.user_scope),
                                            csrf_secure)
          else
            (match name with
               | None -> Eliom_common.SAtt_anon (new_state ())
               | Some name -> Eliom_common.SAtt_named name));
     };
   https = https;
   keep_nl_params = match keep_nl_params with 
     | None -> fallback.keep_nl_params | Some k -> k;
 }
(* It is not possible to make a post_coservice function
   with an optional ?fallback parameter
   because the type 'get of the result depends on the 'get of the
   fallback. Or we must impose 'get = unit ...
 *)


let post_coservice'
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use ?timeout
    ?(https = false)
    ?(keep_nl_params = `All)
    ?(keep_get_na_params = true)
    ~post_params () =
  let csrf_scope = default_csrf_scope csrf_scope in
  (* match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered None
  | _ -> () *)
  {
(*VVV allow timeout and max_use for named coservices? *)
    max_use= max_use;
    timeout= timeout;
    pre_applied_parameters = String.Table.empty, [];
    get_params_type = unit;
    post_params_type = post_params;
    kind = `Nonattached
      {na_name = 
          (if csrf_safe
           then Eliom_common.SNa_post_csrf_safe (uniqueid (),
                                                 (csrf_scope:>Eliom_common.user_scope),
                                                 csrf_secure)
           else
             (match name with
                | None ->
                    Eliom_common.SNa_post' (new_state ())
                | Some name -> Eliom_common.SNa_post_ name));
       na_kind = `Post keep_get_na_params;
      };
    https = https;
    keep_nl_params = keep_nl_params;
    send_appl_content = XNever;
    service_mark = service_mark ();
  }




(*****************************************************************************)

let add_service = Eliommod_services.add_service
let add_naservice = Eliommod_naservices.add_naservice



(*****************************************************************************)
exception Unregistered_CSRF_safe_coservice

let register_delayed_get_or_na_coservice ~sp (k, scope, secure) =
  let f =
    try
      let table = !(Eliom_state.get_session_service_table_if_exists ~sp
                      ~scope:(scope:>Eliom_common.user_scope) ?secure ())
      in
      Int.Table.find
        k table.Eliom_common.csrf_get_or_na_registration_functions
    with Not_found ->
      let table = Eliom_state.get_global_table () in
      try
        Int.Table.find
          k table.Eliom_common.csrf_get_or_na_registration_functions
      with Not_found -> raise Unregistered_CSRF_safe_coservice
  in
  f ~sp


let register_delayed_post_coservice ~sp (k, scope, secure) getname =
  let f =
    try
      let table = !(Eliom_state.get_session_service_table_if_exists ~sp
                      ~scope:(scope:>Eliom_common.user_scope) ?secure ())
      in
      Int.Table.find
        k table.Eliom_common.csrf_post_registration_functions
    with Not_found ->
      let table = Eliom_state.get_global_table () in
      try
        Int.Table.find
          k table.Eliom_common.csrf_post_registration_functions
      with Not_found -> raise Unregistered_CSRF_safe_coservice
  in
  f ~sp getname


let set_delayed_get_or_na_registration_function tables k f =
  tables.Eliom_common.csrf_get_or_na_registration_functions <-
    Int.Table.add
      k
      f
      tables.Eliom_common.csrf_get_or_na_registration_functions

let set_delayed_post_registration_function tables k f =
  tables.Eliom_common.csrf_post_registration_functions <-
    Int.Table.add
    k
    f
    tables.Eliom_common.csrf_post_registration_functions


(*****************************************************************************)
let remove_service table service =
  match get_kind_ service with
    | `Attached attser ->
        let key_kind = get_or_post_ attser in
        let attserget = get_get_name_ attser in
        let attserpost = get_post_name_ attser in
        let sgpt = get_get_params_type_ service in
        let sppt = get_post_params_type_ service in
        Eliommod_services.remove_service table 
          (get_sub_path_ attser)
          {Eliom_common.key_state = (attserget, attserpost);
           Eliom_common.key_kind = key_kind}
          (if attserget = Eliom_common.SAtt_no
             || attserpost = Eliom_common.SAtt_no
           then (anonymise_params_type sgpt,
                 anonymise_params_type sppt)
           else (0, 0))
    | `Nonattached naser ->
        let na_name = get_na_name_ naser in
        Eliommod_naservices.remove_naservice table na_name

let unregister ?scope
    ?secure service =
  let sp = Eliom_common.get_sp_option () in
  match scope with
    | None
    | Some `Global ->
      let table =
	match sp with
          | None ->
            (match Eliom_common.global_register_allowed () with
              | Some get_current_sitedata ->
		let sitedata = get_current_sitedata () in
		sitedata.Eliom_common.global_services
              | _ -> raise
		(Eliom_common.Eliom_site_information_not_available
                   "unregister"))
          | Some sp -> get_global_table ()
      in
      remove_service table service
    | Some (#Eliom_common.user_scope as scope) ->
      match sp with
	| None ->
          raise (failwith "Unregistering service for non global scope must be done during a request")
	| Some sp ->
          let table =
            !(Eliom_state.get_session_service_table ~sp ?secure ~scope ())
          in
          remove_service table service




(*****************************************************************************)
(** {2 on_load and on_unload for Eliom_appl services } *)

(* We keep them in rc because we want them to apply to the next page that
   will be displayed. That is, event after an action or a (stateful)
   redirection.
*)

let on_load_key : Dom_html.event XML.caml_event_handler list Polytables.key = Polytables.make_key ()

let get_onload sp =
  let rc = Eliom_request_info.get_request_cache_sp sp in
  try
    List.rev (Polytables.get ~table:rc ~key:on_load_key)
  with Not_found -> []

let on_unload_key : Dom_html.event XML.caml_event_handler list Polytables.key = Polytables.make_key ()

let get_onunload sp =
  let rc = Eliom_request_info.get_request_cache_sp sp in
  try
    List.rev (Polytables.get ~table:rc ~key:on_unload_key)
  with Not_found -> []

let onload s =
  let rc = Eliom_request_info.get_request_cache () in
  let s0 = try Polytables.get ~table:rc ~key:on_load_key
    with Not_found -> []
  in
  Polytables.set ~table:rc ~key:on_load_key ~value:(s :: s0)

let onunload s =
  let rc = Eliom_request_info.get_request_cache () in
  let s0 = try Polytables.get ~table:rc ~key:on_unload_key
    with Not_found -> []
  in
  Polytables.set ~table:rc ~key:on_unload_key ~value:(s :: s0)

(*****************************************************************************)
let pre_wrap s =
  {s with
    get_params_type = Eliom_parameters.wrap_param_type s.get_params_type;
    post_params_type = Eliom_parameters.wrap_param_type s.post_params_type;
  }

(* let wrap s = Eliom_types.wrap_parameters (pre_wrap s) *)
