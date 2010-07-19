(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
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
open Ocsigen_extensions
open Eliom_sessions
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
      sp:Eliom_sessions.server_params ->
      page -> 
      Ocsigen_http_frame.result Lwt.t

    (** This function is executed just before the service
        when we know exactly which service will answer
        (and after decoding parameters).
        Usually it does nothing.
    *)
    val pre_service :
      ?options:options ->
      sp:Eliom_sessions.server_params -> unit Lwt.t

    (** The following field is usually [Eliom_services.XNever]. 
        This value is recorded inside each service just after registration.
        (Use in [Eliom_predefmod.Eliom_appl])
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
      sp:Eliom_sessions.server_params ->
      page -> 
      Ocsigen_http_frame.result Lwt.t

    val register :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?sp: Eliom_sessions.server_params ->
      service:('get, 'post,
               [< internal_service_kind ],
               [< suff ], 'gn, 'pn, [ `Registrable ], return) service ->
      ?error_handler:(Eliom_sessions.server_params ->
                        (string * exn) list -> page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
      unit
(** registers an service in the public service table
   with the associated handler function.
   [register service t f] will associate the service [service]
   to the function [f].
   [f] is the function that creates a page, called {e service handler}.

   That function takes three parameters.
    - The first one has type [Eliom_sessions.server_params]
   and allows to have acces to informations about the request and the session.
    - The second and third ones are respectively GET and POST parameters.

   For example if [t] is [Eliom_parameters.int "s"], then [ 'get] is [int].

    {e Warning: If you want to register a service in the global table
    after the initialisation phase,
    you must give the [~sp] parameter, otherwise it will raise the
    exception {!Eliom_common.Eliom_function_forbidden_outside_site_loading}.}

   Registering services and coservices is always done in memory as there is
   no means of marshalling closures.

    Registering after initialization is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
    If you use that for main services, you will dynamically create new URLs!
    This may be dangerous as they will disappear if you stop the server.
    Be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks may be broken!

    Some output modules (for example Redirectmod) define their own options
    for that function.
    Use the [?options] parameter to set them.

 *)

    val register_for_session :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?session_name:string ->
      ?cookie_type:Eliom_common.cookie_type ->
      ?secure:bool ->
      sp:Eliom_sessions.server_params ->
      service:('get, 'post, [< internal_service_kind ],
               [< suff ], 'gn, 'pn, [ `Registrable ], return) service ->
      ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                        page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) -> 
      unit
(** registers a handler for a service in the session table.
    If the same client does a request to this service, this function will be
    used instead of the one from the public table.

    Warning:
    - All main services created during initialization must be
    registered in the public table during initialisation,
    but never after,
    - You can't register a service in a session table
    when no session is active (i.e. outside a service handler, 
    when you do not have sp)
    
    [?session_name] is the name of the session, if you want several
    service sessions on the same site.
    
    [?cookie_type] allows to choose if you want a traditional browser
    session (default) or a tab session (works only if there is a client
    side program running).

    If [~secure] is false when the protocol is https, the service will be 
    registered in the unsecure session, 
    otherwise in the secure session with https, the unsecure one with http.
    (Secure session means that Eliom will ask the browser to send the cookie
    only through HTTPS).

    Note that in the case of CSRF safe coservices, parameters
    [?session_name] and [?secure] must match exactly the session name
    and secure option specified while creating the CSRF safe service. 
    Otherwise, the registration will fail
    with {Eliom_services.Wrong_session_table_for_CSRF_safe_coservice}
 *)


    val register_new_service :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?sp: Eliom_sessions.server_params ->
      ?https:bool ->
      path:Ocsigen_lib.url_path ->
      get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
      ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                        page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return) service
(** Same as [new_service] followed by [register] *)

    val register_new_coservice :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?sp: Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_session_name: string ->
      ?csrf_cookie_type: Eliom_common.cookie_type ->
      ?csrf_secure_session: bool ->
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
      ?error_handler:(Eliom_sessions.server_params ->
                        (string * exn) list -> page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Get]) a_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return)
        service
(** Same as [new_coservice] followed by [register] *)

    val register_new_coservice' :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?sp: Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_session_name: string ->
      ?csrf_cookie_type: Eliom_common.cookie_type ->
      ?csrf_secure_session: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      get_params:
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
      ?error_handler:(Eliom_sessions.server_params ->
                        (string * exn) list -> page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Nonattached of [> `Get] na_s ],
       'tipo, 'gn, unit, [> `Registrable ], return)
        service
(** Same as [new_coservice'] followed by [register] *)

    val register_new_coservice_for_session :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?session_name:string ->
      ?cookie_type:Eliom_common.cookie_type ->
      ?secure:bool ->
      sp:Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      fallback:(unit, unit,
                [ `Attached of ([ `Internal of [ `Service ] ], [`Get]) a_s ],
                [ `WithoutSuffix ] as 'tipo,
                unit, unit, [< registrable ], return)
        service ->
      get_params:
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
      ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                        page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Get]) a_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return)
        service
(** Same as [new_coservice] followed by [register_for_session] *)

    val register_new_coservice_for_session' :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?session_name:string ->
      ?cookie_type:Eliom_common.cookie_type ->
      ?secure:bool ->
      sp:Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      get_params:
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
      ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                        page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
      ('get, unit, [> `Nonattached of [> `Get] na_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return)
        service
(** Same as [new_coservice'] followed by [register_for_session] *)

    val register_new_post_service :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?sp: Eliom_sessions.server_params ->
      ?https:bool ->
      fallback:('get, unit,
                [ `Attached of
                    ([ `Internal of
                         ([ `Service | `Coservice ] as 'kind) ], [`Get]) a_s ],
                [< suff ] as 'tipo, 'gn,
                unit, [< `Registrable ], return)
        service ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                        page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
      ('get, 'post, [> `Attached of
                       ([> `Internal of 'kind ], [> `Post]) a_s ],
       'tipo, 'gn, 'pn, [> `Registrable ], return)
        service
(** Same as [new_post_service] followed by [register] *)

    val register_new_post_coservice :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?sp: Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_session_name: string ->
      ?csrf_cookie_type: Eliom_common.cookie_type ->
      ?csrf_secure_session: bool ->
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
      ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                        page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
      ('get, 'post,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Post]) a_s ],
       'tipo, 'gn, 'pn, [> `Registrable ], return)
        service
(** Same as [new_post_coservice] followed by [register] *)

    val register_new_post_coservice' :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?sp: Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_session_name: string ->
      ?csrf_cookie_type: Eliom_common.cookie_type ->
      ?csrf_secure_session: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?keep_get_na_params:bool ->
      ?https:bool ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                        page Lwt.t) ->
      (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
      (unit, 'post, [> `Nonattached of [> `Post] na_s ],
       [ `WithoutSuffix ], unit, 'pn,
       [> `Registrable ], return)
        service
(** Same as [new_post_coservice'] followed by [register] *)


    val register_new_post_coservice_for_session :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?session_name:string ->
      ?cookie_type:Eliom_common.cookie_type ->
      ?secure:bool ->
      sp:Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      fallback:('get, unit,
                [ `Attached of ([ `Internal of
                                   [< `Service | `Coservice ] ], [`Get]) a_s ],
                [< suff ] as 'tipo,
                'gn, unit, [ `Registrable ], return)
        service ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:(Eliom_sessions.server_params ->
                        (string * exn) list -> page Lwt.t) ->
      (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
      ('get, 'post,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Post]) a_s ],
       'tipo, 'gn, 'pn, [> `Registrable ], return)
        service
(** Same as [new_post_coservice] followed by [register_for_session] *)

    val register_new_post_coservice_for_session' :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?session_name:string ->
      ?cookie_type:Eliom_common.cookie_type ->
      ?secure:bool ->
      sp:Eliom_sessions.server_params ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?keep_get_na_params:bool ->
      ?https:bool ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:(Eliom_sessions.server_params ->
                        (string * exn) list -> page Lwt.t) ->
      (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
      (unit, 'post, [> `Nonattached of [> `Post] na_s ],
       [ `WithoutSuffix ], unit, 'pn,
       [> `Registrable ], return)
        service
(** Same as [new_post_coservice'] followed by [register_for_session] *)


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
          ?sp (* if registering during session *)
          ~service
          ?(error_handler = fun sp l ->
            raise (Eliom_common.Eliom_Typing_Error l))
          page_generator =
        let sp = Ocsigen_lib.apply_option Eliom_sessions.esp_of_sp sp in
        Eliom_services.set_do_appl_xhr service (Pages.do_appl_xhr);
        begin
          match get_kind_ service with
            | `Attached attser ->
              let key_kind = get_or_post_ attser in
              let attserget = get_get_name_ attser in
              let attserpost = get_post_name_ attser in
              let suffix_with_redirect = get_redirect_suffix_ attser in
              let sgpt = get_get_params_type_ service in
              let sppt = get_post_params_type_ service in
              let f table ((attserget, attserpost) as attsernames) = 
                Eliommod_services.add_service
                  table
                  ?sp
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
                      let sp2 = Eliom_sessions.sp_of_esp sp in
                      let ri = get_ri ~sp:sp2
                      and ci = get_config_info ~sp:sp2
                      and suff = get_suffix ~sp:sp2 in
                      (catch (fun () ->
                        ri.ri_post_params ci >>= fun post_params ->
                        ri.ri_files ci >>= fun files ->
                        let g = reconstruct_params
                          ~sp
                          sgpt
                          (force ri.ri_get_params)
                          []
                          nosuffixversion
                          suff
                        in
                        let p = reconstruct_params
                          ~sp
                          sppt
                          post_params
                          files
                          false
                          None
                        in
                        if nosuffixversion && suffix_with_redirect &&
                          files=[] && post_params = []
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
                                  ~sp:sp2
                                  g))
                        else
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
                            match sp.Eliom_common.sp_appl_name with
                              (* the appl name as sent by browser *)
                              | None -> false (* the browser did not ask
                                                 application eliom data,
                                                 we don not send a redirection 
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
                            (Pages.pre_service ?options ~sp:sp2 >>= fun () ->
                             page_generator sp2 g p))
                         (function
                           | Eliom_common.Eliom_Typing_Error l ->
                             error_handler sp2 l
                           | e -> fail e)
                        >>= fun content ->
                       Pages.send
                         ?options
                         ?charset
                         ?code
                         ?content_type
                         ?headers
                         ~sp:sp2 content))))
              in
              (match (key_kind, attserget, attserpost) with
                | (Ocsigen_http_frame.Http_header.POST, _,
                   Eliom_common.SAtt_csrf_safe (id, session_name,
                                                cookie_type, secure)) ->
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> session_name || secure <> sec
                          || cookie_type <> ct
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_sessions.get_session_service_table
                            ?secure ?session_name ?cookie_type ~sp ()),
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
                          let sp = Eliom_sessions.sp_of_esp sp in
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_sessions.get_session_service_table
                              ?secure ?session_name ?cookie_type ~sp ())
                      in
                      f table (attserget, attserpost);
                      n)
                | (Ocsigen_http_frame.Http_header.GET,
                   Eliom_common.SAtt_csrf_safe (id, session_name,
                                                cookie_type, secure),
                   _) ->
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> session_name || secure <> sec
                          || ct <> cookie_type
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_sessions.get_session_service_table
                            ?secure ?session_name ?cookie_type ~sp ()), true
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
                          let sp = Eliom_sessions.sp_of_esp sp in
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_sessions.get_session_service_table
                              ?secure ?session_name ?cookie_type ~sp ())
                      in
                      f table (attserget, attserpost);
                      n)
                | _ -> 
                  let tablereg =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl
                      | Ocsigen_lib.Right (sp, session_name, 
                                           cookie_type, secure) ->
                        !(Eliom_sessions.get_session_service_table
                            ?secure ?session_name ?cookie_type ~sp ())
                  in
                  f tablereg (attserget, attserpost))
            | `Nonattached naser ->
              let na_name = get_na_name_ naser in
              let f table na_name = 
                Eliommod_naservices.add_naservice
                  table
                  ?sp
                  na_name
                  ((match get_max_use_ service with
                    | None -> None
                    | Some i -> Some (ref i)),
                   (match get_timeout_ service with
                     | None -> None
                     | Some t -> Some (t, ref (t +. Unix.time ()))),
                   (fun sp ->
                     let sp2 = Eliom_sessions.sp_of_esp sp in
                     let ri = get_ri sp2
                     and ci = get_config_info sp2 in
                     (catch
                        (fun () ->
                          get_post_params sp2 >>= fun post_params ->
                          ri.ri_files ci >>= fun files ->
                          Pages.pre_service ?options ~sp:sp2 >>= fun () ->
                          page_generator sp2
                            (reconstruct_params
                               ~sp
                               (get_get_params_type_ service)
                               (force ri.ri_get_params)
                               []
                               false
                               None)
                            (reconstruct_params
                               ~sp
                               (get_post_params_type_ service)
                               post_params
                               files
                               false
                               None)))
                       (function
                         | Eliom_common.Eliom_Typing_Error l ->
                           error_handler sp2 l
                         | e -> fail e) >>= fun content ->
                     Pages.send
                       ?options
                       ?charset
                       ?code
                       ?content_type
                       ?headers
                       ~sp:sp2 content)
                  )
              in
              match na_name with
                | Eliom_common.SNa_get_csrf_safe (id, session_name, 
                                                  cookie_type, secure) ->
                  (* CSRF safe coservice: we'll do the registration later *)
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> session_name || secure <> sec
                          || ct <> cookie_type
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_sessions.get_session_service_table
                            ?secure ?session_name ?cookie_type ~sp ()), true
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
                          let sp = Eliom_sessions.sp_of_esp sp in
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_sessions.get_session_service_table
                              ?secure ?session_name ?cookie_type ~sp ())
                      in
                      f table na_name;
                      n)
                | Eliom_common.SNa_post_csrf_safe (id, session_name, 
                                                   cookie_type, secure) ->
                  (* CSRF safe coservice: we'll do the registration later *)
                  let tablereg, forsession =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl, false
                      | Ocsigen_lib.Right (sp, sn, ct, sec) ->
                        if sn <> session_name || secure <> sec
                          || ct <> cookie_type
                        then raise
                          Wrong_session_table_for_CSRF_safe_coservice;
                        !(Eliom_sessions.get_session_service_table
                            ?secure ?session_name ?cookie_type ~sp ()), true
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
                          let sp = Eliom_sessions.sp_of_esp sp in
                          (* we do not register in global table,
                             but in the table specified while creating
                             the csrf safe service *)
                          !(Eliom_sessions.get_session_service_table
                              ?secure ?session_name ?cookie_type ~sp ())
                      in
                      f table na_name;
                      n)
                | _ -> 
                  let tablereg =
                    match table with
                      | Ocsigen_lib.Left globtbl -> globtbl
                      | Ocsigen_lib.Right (sp, session_name, 
                                           cookie_type, secure) ->
                        !(Eliom_sessions.get_session_service_table
                            ?secure ?session_name ?cookie_type ~sp ())
                  in
                  f tablereg na_name
        end

      let register 
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp 
          ~service
          ?error_handler
          page_gen =
        match sp with
          | None ->
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
                (Eliom_common.Eliom_function_forbidden_outside_site_loading
                   "register"))
          | Some sp ->
            register_aux
              ?options
              ?charset
              ?code
              ?content_type
              ?headers
              ?error_handler
              (Ocsigen_lib.Left (get_global_table ~sp ()))
              ~sp
              ~service
              page_gen


      (* WARNING: if we create a new service without registering it,
         we can have a link towards a page that does not exist!!! :-(
         That's why I impose to register all service during init.
         The only other way I see to avoid this is to impose a syntax extension
         like "let rec" for service...
      *)



      let register_for_session
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure
          ~sp
          ~service
          ?error_handler
          page =
        register_aux
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?error_handler
          (Ocsigen_lib.Right (sp, session_name, cookie_type, secure))
          ~sp
          ~service page



      let register_new_service
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp
          ?https
          ~path
          ~get_params
          ?error_handler
          page =
        let u = new_service ?sp ?https ~path ~get_params () in
        register ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp ~service:u ?error_handler page;
        u

      let register_new_coservice
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp
          ?name
          ?csrf_safe
          ?csrf_session_name
          ?csrf_cookie_type
          ?csrf_secure_session
          ?max_use
          ?timeout
          ?https
          ~fallback
          ~get_params
          ?error_handler
          page =
        let u = 
          new_coservice ?name
            ?csrf_safe
            ?csrf_session_name
            ?csrf_cookie_type
            ?csrf_secure_session
            ?max_use ?timeout ?https
            ~fallback ~get_params () 
        in
        register ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp ~service:u ?error_handler page;
        u

      let register_new_coservice'
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp
          ?name
          ?csrf_safe
          ?csrf_session_name
          ?csrf_cookie_type
          ?csrf_secure_session
          ?max_use
          ?timeout
          ?https
          ~get_params
          ?error_handler
          page =
        let u = 
          new_coservice' 
            ?name
            ?csrf_safe
            ?csrf_session_name
            ?csrf_cookie_type
            ?csrf_secure_session
            ?max_use ?timeout ?https ~get_params () 
        in
        register ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp ~service:u ?error_handler page;
        u

      let register_new_coservice_for_session
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure
          ~sp
          ?name
          ?csrf_safe
          (*            ?csrf_session_name = ~session_name
                        ?csrf_cookie_type
                        ?csrf_secure_session = ~secure *)
          ?max_use
          ?timeout
          ?https
          ~fallback
          ~get_params
          ?error_handler
          page =
        let u = 
          new_coservice ?name 
            ?csrf_safe
            ?csrf_session_name:session_name
            ?csrf_cookie_type:cookie_type
            ?csrf_secure_session:secure
            ?max_use ?timeout ?https
            ~fallback ~get_params () 
        in
        register_for_session
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure ~sp ~service:u ?error_handler page;
        u

      let register_new_coservice_for_session'
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure
          ~sp
          ?name
          ?csrf_safe
          (*            ?csrf_session_name
                        ?csrf_cookie_type
                        ?csrf_secure_session *)
          ?max_use
          ?timeout
          ?https
          ~get_params
          ?error_handler
          page =
        let u = new_coservice' 
          ?name 
          ?csrf_safe
          ?csrf_session_name:session_name
          ?csrf_cookie_type:cookie_type
          ?csrf_secure_session:secure
          ?max_use ?https ~get_params () 
        in
        register_for_session
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure ~sp ~service:u ?error_handler page;
        u


      let register_new_post_service
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp
          ?https
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        let u = new_post_service ?sp ?https
          ~fallback:fallback ~post_params:post_params () in
        register ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp ~service:u ?error_handler page_gen;
        u

      let register_new_post_coservice
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp
          ?name
          ?csrf_safe
          ?csrf_session_name
          ?csrf_cookie_type
          ?csrf_secure_session
          ?max_use
          ?timeout
          ?https
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        let u =
          new_post_coservice ?name 
            ?csrf_safe
            ?csrf_session_name
            ?csrf_cookie_type
            ?csrf_secure_session
            ?max_use ?timeout ?https
            ~fallback ~post_params () in
        register ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp ~service:u ?error_handler page_gen;
        u

      let register_new_post_coservice'
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp
          ?name
          ?csrf_safe
          ?csrf_session_name
          ?csrf_cookie_type
          ?csrf_secure_session
          ?max_use
          ?timeout
          ?keep_get_na_params
          ?https
          ~post_params
          ?error_handler
          page_gen =
        let u =
          new_post_coservice'
            ?name
            ?csrf_safe
            ?csrf_session_name
            ?csrf_cookie_type
            ?csrf_secure_session
            ?keep_get_na_params
            ?max_use
            ?timeout
            ?https
            ~post_params ()
        in
        register ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?sp ~service:u ?error_handler page_gen;
        u


      let register_new_post_coservice_for_session
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure
          ~sp
          ?name
          ?csrf_safe
          (*            ?csrf_session_name
                        ?csrf_cookie_type
                        ?csrf_secure_session *)
          ?max_use
          ?timeout
          ?https
          ~fallback
          ~post_params
          ?error_handler
          page_gen =
        let u = new_post_coservice ?name 
          ?csrf_safe
          ?csrf_session_name:session_name
          ?csrf_cookie_type:cookie_type
          ?csrf_secure_session:secure
          ?max_use ?timeout ?https ~fallback ~post_params () in
        register_for_session
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure ~sp ~service:u ?error_handler page_gen;
        u

      let register_new_post_coservice_for_session'
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure
          ~sp
          ?name
          ?csrf_safe
          (*            ?csrf_session_name
                        ?csrf_cookie_type
                        ?csrf_secure_session *)
          ?max_use
          ?timeout
          ?keep_get_na_params
          ?https
          ~post_params
          ?error_handler
          page_gen =
        let u =
          new_post_coservice'
            ?name
            ?csrf_safe
            ?csrf_session_name:session_name
            ?csrf_cookie_type:cookie_type
            ?csrf_secure_session:secure
            ?keep_get_na_params
            ?max_use
            ?timeout
            ?https
            ~post_params ()
        in
        register_for_session
          ?options
          ?charset
          ?code
          ?content_type
          ?headers
          ?session_name
          ?cookie_type
          ?secure ~sp ~service:u ?error_handler page_gen;
        u




     end : ELIOMREGSIG with
                         type page = Pages.page
                       and type options = Pages.options
                       and type return = Pages.return)

