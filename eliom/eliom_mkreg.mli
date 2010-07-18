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


(** This module defines the functor to use to creates modules
   generating functions to register services for your own types of pages.
   It is used for example in {!Eliom_predefmod}.
 *)


open Ocsigen_extensions
open Eliom_sessions
open Eliom_services
open Eliom_parameters



(** {2 Creating modules to register services for one type of pages} *)

module type REGCREATE =
  sig

    type page

    type options

    type return

    val send :
      ?options:options ->
      ?charset:string ->
      ?code:int ->
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

(*
    val register_new_get_post_coservice' :
        ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
      ?https:bool ->
        fallback:('get, unit ,
                  [ `Nonattached of [`Get] na_s ],
                   [< suff ] as 'tipo,
                   'gn, unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
              page Lwt.t) ->
                (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Nonattached of [> `Post] na_s ],
                   [> 'tipo], 'gn, 'pn, [> `Registrable ])
                    service
(* * Same as [new_get_post_coservice'] followed by [register] *)
*)

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

(*
    val register_new_get_post_coservice_for_session' :
        ?options:options ->
        ?session_name:string ->
      ?cookie_type:Eliom_common.cookie_type ->
  ?secure:bool ->
        sp:Eliom_sessions.server_params ->
  ?name: string ->
      ?csrf_safe: bool ->
        ?max_use:int ->
        ?timeout:float ->
      ?https:bool ->
          fallback:('get, unit, [ `Nonattached of [`Get] na_s ],
                    [< suff ] as 'tipo,
                    'gn, unit, [< `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(Eliom_sessions.server_params ->
                  (string * exn) list -> page Lwt.t) ->
                    (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, [> `NonAttached of [> `Post] na_s ],
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(* * Same as [new_get_post_coservice] followed by [register_for_session] *)
*)




  end






module MakeRegister : functor (Pages: REGCREATE) -> 
  ELIOMREGSIG with
                type page = Pages.page
              and type options = Pages.options
              and type return = Pages.return
