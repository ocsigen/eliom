(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
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


open Lwt
open Ocsigen_extensions
open Eliom_state
open Eliom_services
open Eliom_parameters
open Lazy


(****************************************************************************)

module type REGCREATE =
  sig


    type page

    type options

    type return

    val send :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      page -> 
      Ocsigen_http_frame.result Lwt.t

    (** This function is executed just before the service
        when we know exactly which service will answer
        (and after decoding parameters).
        Usually it does nothing.
    *)
    val pre_service :
      ?options:options ->
      unit -> unit Lwt.t

    (** The following field is usually [Eliom_services.XNever]. 
        This value is recorded inside each service just after registration.
        (Use in [Eliom_output.Eliom_appl])
    *)
    val do_appl_xhr : Eliom_services.do_appl_xhr

  end


module type ELIOMREGSIG =
(* pasted from mli *)
  sig



    type page

    type options

    type return

    val send :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      page -> 
      Ocsigen_http_frame.result Lwt.t

    val register :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      service:('get, 'post,
               [< internal_service_kind ],
               [< suff ], 'gn, 'pn, [ `Registrable ], return) service ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> 'post -> page Lwt.t) ->
      unit
(** Register a service with the associated handler function.
   [register s t f] will associate the service [s] to the function [f].
   [f] is the function that creates a page, called {e service handler}.

   The handler function takes two parameters.
    - The second and third ones are respectively GET and POST parameters.

   For example if [t] is [Eliom_parameters.int "s"], then [ 'get] is [int].

   The [?scope] optional parameter is [`Global] by default, which means that the
   service will be registered in the global table and be available to any client.
   If you want to restrict the visibility of the service to a browser session,
   use [~scope:`Session].
   If you want to restrict the visibility of the service to a group of sessions,
   use [~scope:`Session_group].
   If you have a client side Eliom program running, and you want to restrict
   the visibility of the service to this instance of the program,
   use [~scope:`Client_process].

   If the same service is registered several times with different visibilities,
   Eliom will choose the service for handling a request in that order:
   [`Client_process], [`Session], [`Session_group] and finally [`Global]. It means for example
   that you can register a specialized version of a public service for a session.

    Warning: All public services created during initialization must be
    registered in the public table during initialisation, never after,

   Registering services and coservices is always done in memory as there is
   no means of marshalling closures.

    If you register new services dynamically, be aware that they will disappear
    if you stop the server. If you create dynamically new URLs,
    be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks may be broken!

    Some output modules (for example Redirectmod) define their own options
    for that function. Use the [?options] parameter to set them.

    The optional parameters [?charset], [?code], [?content_type] and [?headers]
    can be used to modify the HTTP answer sent by Eliom. Use this with care.

    [?state_name] is the name of the session (browser session or "tab" session),
    if you want several
    service sessions on the same site. It has no effect for scope [`Global].
    
    If [~secure_session] is false when the protocol is https, the service will be 
    registered in the unsecure session, 
    otherwise in the secure session with https, the unsecure one with http.
    (Secure session means that Eliom will ask the browser to send the cookie
    only through HTTPS). It has no effect for scope [`Global].

    Note that in the case of CSRF safe coservices, parameters
    [?state_name] and [?secure_session] must match exactly the session name
    and secure option specified while creating the CSRF safe service. 
    Otherwise, the registration will fail
    with {Eliom_services.Wrong_session_table_for_CSRF_safe_coservice}
 *)



    val register_service :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?https:bool ->
      ?priority:int ->
      path:Ocsigen_lib.url_path ->
      get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return) service
(** Same as [service] followed by [register] *)

    val register_coservice :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      fallback:(unit, unit,
                [ `Attached of ([ `Internal of [ `Service ] ], [`Get]) a_s ],
                [ `WithoutSuffix ] as 'tipo,
                unit, unit, [< registrable ], return)
        service ->
      get_params:
        ('get, [`WithoutSuffix], 'gn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Get]) a_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return)
        service
(** Same as [coservice] followed by [register] *)

    val register_coservice' :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      get_params:
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Nonattached of [> `Get] na_s ],
       'tipo, 'gn, unit, [> `Registrable ], return)
        service
(** Same as [coservice'] followed by [register] *)

    val register_post_service :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?https:bool ->
      ?priority:int ->
      fallback:('get, unit,
                [ `Attached of
                    ([ `Internal of
                         ([ `Service | `Coservice ] as 'kind) ], [`Get]) a_s ],
                [< suff ] as 'tipo, 'gn,
                unit, [< `Registrable ], return)
        service ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> 'post -> page Lwt.t) ->
      ('get, 'post, [> `Attached of
                       ([> `Internal of 'kind ], [> `Post]) a_s ],
       'tipo, 'gn, 'pn, [> `Registrable ], return)
        service
(** Same as [post_service] followed by [register] *)

    val register_post_coservice :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      fallback:('get, unit ,
                [ `Attached of
                    ([ `Internal of [< `Service | `Coservice ] ], [`Get]) a_s ],
                [< suff ] as 'tipo,
                'gn, unit, [< `Registrable ], return)
        service ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> 'post -> page Lwt.t) ->
      ('get, 'post,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Post]) a_s ],
       'tipo, 'gn, 'pn, [> `Registrable ], return)
        service
(** Same as [post_coservice] followed by [register] *)

    val register_post_coservice' :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?keep_get_na_params:bool ->
      ?https:bool ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      (unit -> 'post -> page Lwt.t) ->
      (unit, 'post, [> `Nonattached of [> `Post] na_s ],
       [ `WithoutSuffix ], unit, 'pn,
       [> `Registrable ], return)
        service
(** Same as [post_coservice'] followed by [register] *)


  end





module MakeRegister = functor
  (Pages : REGCREATE) ->
    (struct

      type page = Pages.page

      type options = Pages.options

      type return = Pages.return

      let send = Pages.send

      let register_aux
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          table
          ~service
          ?(error_handler = fun l -> raise (Eliom_common.Eliom_Typing_Error l))
          page_generator =
        Eliom_services.set_do_appl_xhr service (Pages.do_appl_xhr);
        begin
          match get_kind_ service with
            | `Attached attser ->
              let key_kind = get_or_post_ attser in
              let attserget = get_get_name_ attser in
              let attserpost = get_post_name_ attser in
              let suffix_with_redirect = get_redirect_suffix_ attser in
              let priority = get_priority_ attser in
              let sgpt = get_get_params_type_ service in
              let sppt = get_post_params_type_ service in
              let f table ((attserget, attserpost) as attsernames) = 
                Eliommod_services.add_service
                  priority
                  table
                  (get_sub_path_ attser)
                  {Eliom_common.key_state = attsernames;
                   Eliom_common.key_kind = key_kind}
                  ((if attserget = Eliom_common.SAtt_no
                    || attserpost = Eliom_common.SAtt_no
                    then (anonymise_params_type sgpt,
                          anonymise_params_type sppt)
                    else (0, 0)),
                    ((match get_max_use_ service with
                      | None -> None
                      | Some i -> Some (ref i)),
                     (match get_timeout_ service with
                       | None -> None
                       | Some t -> Some (t, ref (t +. Unix.time ()))),
                     (fun nosuffixversion sp ->
                       Lwt.with_value Eliom_common.sp_key (Some sp)
                         (fun () ->
                           let ri = Eliom_request_info.get_ri_sp sp
                           and suff = Eliom_request_info.get_suffix_sp sp in
                           (catch (fun () ->
                             reconstruct_params
                               ~sp
                               sgpt
                               (Some (Lwt.return (force ri.ri_get_params)))
                               (Some (Lwt.return []))
                               nosuffixversion
                               suff
                             >>= fun g ->
                             let post_params = 
                               Eliom_request_info.get_post_params_sp sp
                             in
                             let files =
                               Eliom_request_info.get_files_sp sp
                             in
                             reconstruct_params
                               ~sp
                               sppt
                               post_params
                               files
                               false
                               None
                             >>= fun p ->
                             (match files, post_params with
                               | Some files, Some post_params ->
                                 (files >>= fun files ->
                                  post_params >>= fun post_params ->
                                  if nosuffixversion && suffix_with_redirect &&
                                    files = [] && post_params = []
                                  then (* it is a suffix service in version 
                                          without suffix. We redirect. *)
                                    Lwt.fail
                                      (Eliom_common.Eliom_do_redirection
                                         (Eliom_uri.make_string_uri
                                            ~absolute:true
                                            ~service:
                                            (service : 
                                               ('a, 'b, [< Eliom_services.internal_service_kind ],
                                                [< Eliom_services.suff ], 'c, 'd, [ `Registrable ],
                                                'return) Eliom_services.service :> 
                                               ('a, 'b, Eliom_services.service_kind,
                                                [< Eliom_services.suff ], 'c, 'd, 
                                                [< Eliom_services.registrable ], 'return)
                                               Eliom_services.service)
                                            g))
                                  else Lwt.return ())
                               | _ -> Lwt.return ())
                             >>= fun () ->
                             let redir =
                            (* If it is an xmlHTTPrequest who
                               asked for an internal application
                               service but the current service 
                               does not belong to the same application,
                               we ask the browser to stop the program and
                               do a redirection.
                               This can happen for example after an action,
                               when the fallback service does not belong
                               to the application.
                               We can not do a regular redirection because
                               it is an XHR. We use our own redirections.
                            *)
                            (*VVV
                              An alternative, to avoid the redirection with rc, 
                              would be to answer the full page and to detect on client side that it is not
                              the answer of an XRH (using content-type) and ask the browser to act as if
                              it were a regular request. Is it possible to do that?
                              Drawback: The URL will be wrong

                              Other solution: send the page and ask the browser to put it in the cache 
                              during a few seconds. Then redirect. But can we trust the browser cache?
                            *)
                                 match sp.Eliom_common.sp_client_appl_name with
                              (* the appl name as sent by browser *)
                                   | None -> false (* the browser did not ask
                                                      application eliom data,
                                                      we do not send a redirection 
                                                  *)
                                   | Some anr ->
                                (* the browser asked application eliom data
                                   (content only) for application anr *)
                                     match Eliom_services.get_do_appl_xhr service
                                (* the appl name of the service *)
                                     with
                                       | Eliom_services.XSame_appl an
                                           when (an = anr)
                                             -> (* Same appl, it is ok *) false
                                       | Eliom_services.XAlways ->
                                     (* It is an action *) false
                                       | _ -> true
                               in
                               if redir
                               then
                                 Lwt.fail
                                   (* we answer to the xhr
                                      by asking an HTTP redirection *)
                                   (Eliom_common.Eliom_do_half_xhr_redirection
                                      ("/"^
                                          Ocsigen_lib.concat_strings 
                                     ri.Ocsigen_extensions.ri_original_full_path_string
                                     "?"
                                     (Eliom_parameters.construct_params_string
                                        (Lazy.force
                                           ri.Ocsigen_extensions.ri_get_params)
                                     )))
                          (* We do not put hostname and port.
                             It is ok with this kind of redirections. *)
                          (* If an action occured before, 
                             it may have removed some get params form ri *)
                               else
                                 (Pages.pre_service ?options () >>= fun () ->
                                  page_generator g p))
                              (function
                                | Eliom_common.Eliom_Typing_Error l ->
                                  error_handler l
                                | e -> fail e)
                             >>= fun content ->
                            Pages.send
                              ?options
                              ?charset
                              ?code
                              ?content_type
                              ?headers
                              content)))))
              in
              (match (key_kind, attserget, attserpost) with
                | (Ocsigen_http_frame.Http_header.POST, _,
                   Eliom_common.SAtt_csrf_safe (id, state_name,
                                                scope, secure_session)) ->
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> state_name || secure_session <> sec
                          || scope <> ct
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_state.get_session_service_table
                            ?secure:secure_session
                            ?state_name ~scope ~sp ()),
                          true
                  in
                  Eliom_services.set_delayed_post_registration_function
                    tablereg
                    id
                    (fun ~sp attserget ->
                      let n = Eliom_services.new_state () in
                      let attserpost = Eliom_common.SAtt_anon n in
                      let table = 
                        if forsession
                        then tablereg
                        else
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_state.get_session_service_table
                              ?secure:secure_session
                              ?state_name ~scope ~sp ())
                      in
                      f table (attserget, attserpost);
                      n)
                | (Ocsigen_http_frame.Http_header.GET,
                   Eliom_common.SAtt_csrf_safe (id, state_name,
                                                scope, secure_session),
                   _) ->
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> state_name || secure_session <> sec
                          || ct <> scope
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_state.get_session_service_table
                            ?secure:secure_session
                            ?state_name ~scope ~sp ()), true
                  in
                  Eliom_services.set_delayed_get_or_na_registration_function
                    tablereg
                    id
                    (fun ~sp ->
                      let n = Eliom_services.new_state () in
                      let attserget = Eliom_common.SAtt_anon n in
                      let table = 
                        if forsession
                        then tablereg
                        else
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_state.get_session_service_table
                              ?secure:secure_session
                              ?state_name ~scope ~sp ())
                      in
                      f table (attserget, attserpost);
                      n)
                | _ -> 
                  let tablereg =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl
                      | Ocsigen_lib.Right (sp, state_name, 
                                           scope, secure_session) ->
                        !(Eliom_state.get_session_service_table
                            ?secure:secure_session ?state_name ~scope ~sp ())
                  in
                  f tablereg (attserget, attserpost))
            | `Nonattached naser ->
              let na_name = get_na_name_ naser in
              let f table na_name = 
                Eliommod_naservices.add_naservice
                  table
                  na_name
                  ((match get_max_use_ service with
                    | None -> None
                    | Some i -> Some (ref i)),
                   (match get_timeout_ service with
                     | None -> None
                     | Some t -> Some (t, ref (t +. Unix.time ()))),
                   (fun sp ->
                     Lwt.with_value Eliom_common.sp_key (Some sp) 
                       (fun () ->
                         let ri = Eliom_request_info.get_ri_sp sp in
                         catch
                           (fun () ->
                             reconstruct_params
                               ~sp
                               (get_get_params_type_ service)
                               (Some (Lwt.return (force ri.ri_get_params)))
                               (Some (Lwt.return []))
                               false
                               None
                             >>= fun g ->
                             let post_params = 
                               Eliom_request_info.get_post_params_sp sp
                             in
                             let files = Eliom_request_info.get_files_sp sp in
                             reconstruct_params
                               ~sp
                               (get_post_params_type_ service)
                               post_params
                               files
                               false
                               None
                             >>= fun p ->
                             Pages.pre_service ?options () >>= fun () ->
                             page_generator g p)
                           (function
                             | Eliom_common.Eliom_Typing_Error l ->
                               error_handler l
                             | e -> fail e) >>= fun content ->
                         Pages.send
                           ?options
                           ?charset
                           ?code
                           ?content_type
                           ?headers
                           content)
                   ))
              in
              match na_name with
                | Eliom_common.SNa_get_csrf_safe (id, state_name, 
                                                  scope, secure_session) ->
                  (* CSRF safe coservice: we'll do the registration later *)
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> state_name || secure_session <> sec
                          || ct <> scope
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_state.get_session_service_table
                            ?secure:secure_session
                            ?state_name ~scope ~sp ()), true
                  in
                  set_delayed_get_or_na_registration_function
                    tablereg
                    id
                    (fun ~sp ->
                      let n = Eliom_services.new_state () in
                      let na_name = Eliom_common.SNa_get' n in
                      let table =
                        if forsession
                        then tablereg
                        else
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_state.get_session_service_table
                              ?secure:secure_session
                              ?state_name ~scope ~sp ())
                      in
                      f table na_name;
                      n)
                | Eliom_common.SNa_post_csrf_safe (id, state_name, 
                                                   scope, secure_session) ->
                  (* CSRF safe coservice: we'll do the registration later *)
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> state_name || secure_session <> sec
                          || ct <> scope
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_state.get_session_service_table
                            ?secure:secure_session
                            ?state_name ~scope ~sp ()), true
                  in
                  set_delayed_get_or_na_registration_function
                    tablereg
                    id
                    (fun ~sp ->
                      let n = Eliom_services.new_state () in
                      let na_name = Eliom_common.SNa_post' n in
                      let table =
                        if forsession
                        then tablereg
                        else
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_state.get_session_service_table
                              ?secure:secure_session
                              ?state_name ~scope ~sp ())
                      in
                      f table na_name;
                      n)
                | _ -> 
                  let tablereg =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl
                      | Ocsigen_lib.Right (sp, state_name, 
                                           scope, secure_session) ->
                        !(Eliom_state.get_session_service_table
                            ?secure:secure_session ?state_name ~scope ~sp ())
                  in
                  f tablereg na_name
        end

      let register 
          ?(scope = `Global)
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ~service
          ?error_handler
          page_gen =
        let sp = Eliom_common.get_sp_option () in
        match scope, sp with
          | `Global, None ->
            (match Eliom_common.global_register_allowed () with
              | Some get_current_sitedata ->
                let sitedata = get_current_sitedata () in
                (match get_kind_ service with
                  | `Attached attser ->
                    Eliom_common.remove_unregistered
                      sitedata (get_sub_path_ attser)
                  | `Nonattached naser ->
                    Eliom_common.remove_unregistered_na
                      sitedata (get_na_name_ naser));
                register_aux
                  ?options
                  ?charset
                  ?code
                  ?content_type
                  ?headers
                  (Ocsigen_lib.Left sitedata.Eliom_common.global_services)
                  ~service ?error_handler page_gen
              | _ -> raise
                (Eliom_common.Eliom_site_information_not_available
                   "register"))
          | `Global, Some sp ->
            register_aux
              ?options
              ?charset
              ?code
              ?content_type
              ?headers
              ?error_handler
              (Ocsigen_lib.Left (get_global_table ()))
              ~service
              page_gen
          | _, None ->
            raise (failwith "Missing sp while registering service")
          | scope, Some sp ->
            let scope = Eliom_common.user_scope_of_scope scope in
            register_aux
              ?options
              ?charset
              ?code
              ?content_type
              ?headers
              ?error_handler
              (Ocsigen_lib.Right (sp, state_name, scope, secure_session))
              ~service page_gen

      (* WARNING: if we create a new service without registering it,
         we can have a link towards a page that does not exist!!! :-(
         That's why I impose to register all service during init.
         The only other way I see to avoid this is to impose a syntax extension
         like "let rec" for service...
      *)


      let register_service
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ?https
          ?priority
          ~path
          ~get_params
          ?error_handler
          page =
        let u = service ?https ?priority ~path ~get_params () in
        register
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ~service:u ?error_handler page;
        u

      let register_coservice
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ?name
          ?csrf_safe
          ?csrf_state_name
          ?csrf_scope
          ?csrf_secure
          ?max_use
          ?timeout
          ?https
          ~fallback
          ~get_params
          ?error_handler
          page =
        let u = 
          coservice ?name
            ?csrf_safe
            ?csrf_state_name
            ?csrf_scope
            ?csrf_secure
            ?max_use ?timeout ?https
            ~fallback ~get_params () 
        in
        register
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ~service:u ?error_handler page;
        u

      let register_coservice'
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ?name
          ?csrf_safe
          ?csrf_state_name
          ?csrf_scope
          ?csrf_secure
          ?max_use
          ?timeout
          ?https
          ~get_params
          ?error_handler
          page =
        let u = 
          coservice' 
            ?name
            ?csrf_safe
            ?csrf_state_name
            ?csrf_scope
            ?csrf_secure
            ?max_use ?timeout ?https ~get_params () 
        in
        register
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ~service:u ?error_handler page;
        u

      let register_post_service
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ?https
          ?priority
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        let u = post_service ?https ?priority
          ~fallback:fallback ~post_params:post_params () in
        register
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ~service:u ?error_handler page_gen;
        u

      let register_post_coservice
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ?name
          ?csrf_safe
          ?csrf_state_name
          ?csrf_scope
          ?csrf_secure
          ?max_use
          ?timeout
          ?https
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        let u =
          post_coservice ?name 
            ?csrf_safe
            ?csrf_state_name
            ?csrf_scope
            ?csrf_secure
            ?max_use ?timeout ?https
            ~fallback ~post_params () in
        register
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ~service:u ?error_handler page_gen;
        u

      let register_post_coservice'
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ?name
          ?csrf_safe
          ?csrf_state_name
          ?csrf_scope
          ?csrf_secure
          ?max_use
          ?timeout
          ?keep_get_na_params
          ?https
          ~post_params
          ?error_handler
          page_gen =
        let u =
          post_coservice'
            ?name
            ?csrf_safe
            ?csrf_state_name
            ?csrf_scope
            ?csrf_secure
            ?keep_get_na_params
            ?max_use
            ?timeout
            ?https
            ~post_params ()
        in
        register
          ?scope
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?state_name
          ?secure_session
          ~service:u ?error_handler page_gen;
        u



     end : ELIOMREGSIG with
                         type page = Pages.page
                       and type options = Pages.options
                       and type return = Pages.return)

