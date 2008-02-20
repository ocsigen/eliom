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


open Extensions
open Eliom_sessions
open Eliom_services
open Eliom_parameters



(** {2 Creating modules to register services for one type of pages} *)

module type REGCREATE = 
  sig
    type page

    val send : 
        ?cookies:Eliom_services.cookie list -> 
          ?charset:string ->
            ?code:int ->
              sp:Eliom_sessions.server_params -> 
                page -> result_to_send Lwt.t

  end


module type ELIOMREGSIG1 =
  sig



    type page

    val send : 
        ?cookies:Eliom_services.cookie list -> 
          ?charset:string ->
            ?code: int ->
              sp:Eliom_sessions.server_params -> 
                page -> result_to_send Lwt.t

    val register :
        ?sp: Eliom_sessions.server_params ->
        service:('get, 'post,
                 [< internal_service_kind ],
                 [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
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
    exception {!Eliommod.Eliom_function_forbidden_outside_site_loading}.}   

   Registering services and coservices is always done in memory as there is
   no means of marshalling closures.

    Registering after initialization is not encouraged for coservices
    without timeout, as such services will be available only until the end
    of the server process!
    If you use that for main services, you will dynamically create new URLs!
    This may be dangerous as they will disappear if you stop the server.
    Be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks may be broken!

 *)


    val register_for_session :
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
          service:('get, 'post, [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ]) service ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) -> unit
(** registers a handler for a service in the session table.
   If the same client does a request to this service, this function will be
   used instead of the one from the public table.

   Warning:
   - All main services created during initialization must be
   registered in the public table during initialisation,
   but never after,
   - You can't register a service in a session table 
   when no session is active (i.e. outside a service handler)
 *)


    val register_new_service :
        ?sp: Eliom_sessions.server_params ->
        path:url_path ->
            get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
                ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                  page Lwt.t) ->
                    (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                      ('get, unit, 
                       [> `Attached of 
                         [> `Internal of [> `Service ] * [> `Get] ] a_s ],
                       'tipo, 'gn, unit, 
                       [> `Registrable ]) service
(** Same as [new_service] followed by [register] *)
                      
    val register_new_service' :
        ?sp: Eliom_sessions.server_params ->
          name:string ->
            get_params:('get, [ `WithoutSuffix ], 'gn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, 
                     [> `Nonattached of [> `Get ] na_s ],
                     [ `WithoutSuffix ], 'gn, unit, 
                     [> `Registrable ]) service
(** Same as [new_service'] followed by [register] *)
                      
    val register_new_coservice :
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:(unit, unit, 
                  [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                   [ `WithoutSuffix ] as 'tipo, 
                   unit, unit, [< registrable ])
        service ->
          get_params: 
            ('get, [`WithoutSuffix], 'gn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> 
                (string * exn) list -> page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, 
                     [> `Attached of 
                       [> `Internal of [> `Coservice ] * [> `Get]] a_s ], 
                     'tipo, 'gn, unit, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice] followed by [register] *)

    val register_new_coservice' :
      ?sp: Eliom_sessions.server_params ->
      ?max_use:int ->
      ?timeout:float ->
        get_params: 
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
          ?error_handler:(Eliom_sessions.server_params -> 
            (string * exn) list -> page Lwt.t) ->
              (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                ('get, unit, 
                 [> `Nonattached of [> `Get] na_s ],
                 'tipo, 'gn, unit, [> `Registrable ])
                  service
(** Same as [new_coservice'] followed by [register] *)

    val register_new_coservice_for_session :
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:(unit, unit, 
                    [ `Attached of [ `Internal of [ `Service ] * [`Get]] a_s ],
                    [ `WithoutSuffix ] as 'tipo, 
                    unit, unit, [< registrable ])
            service ->
              get_params: 
                ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
                  ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                    page Lwt.t) ->
                      (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                        ('get, unit, 
                         [> `Attached of 
                           [> `Internal of [> `Coservice ] * [> `Get] ] a_s ], 
                         'tipo, 'gn, unit, 
                         [> `Registrable ])
                          service
(** Same as [new_coservice] followed by [register_for_session] *)

    val register_new_coservice_for_session' :
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          get_params: 
            ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> 'get -> unit -> page Lwt.t) ->
                    ('get, unit, [> `Nonattached of [> `Get] na_s ], 
                     'tipo, 'gn, unit, 
                     [> `Registrable ])
                      service
(** Same as [new_coservice'] followed by [register_for_session] *)

    val register_new_post_service :
        ?sp: Eliom_sessions.server_params ->
        fallback:('get, unit, 
                  [ `Attached of [ `Internal of 
                    ([ `Service | `Coservice ] as 'kind) * [`Get] ] a_s ],
                  [< suff ] as 'tipo, 'gn,
                  unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, [> `Attached of
                    [> `Internal of 'kind * [> `Post] ] a_s ], 
                   'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_service] followed by [register] *)

    val register_new_post_service' :
        ?sp: Eliom_sessions.server_params ->
          name: string ->
            post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
              ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
                page Lwt.t) ->
                  (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
                    (unit, 'post, [> `Nonattached of [> `Post ] na_s ], 
                     [ `WithoutSuffix ], unit, 'pn, [> `Registrable ])
                      service
(** Same as [new_post_service'] followed by [register] *)

    val register_new_post_coservice :
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
        fallback:('get, unit , 
                  [ `Attached of 
                    [ `Internal of [< `Service | `Coservice ] * [`Get] ] a_s ],
                   [< suff ] as 'tipo, 
                   'gn, unit, [< `Registrable ])
        service ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
              page Lwt.t) ->
                (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                  ('get, 'post, 
                   [> `Attached of 
                     [> `Internal of [> `Coservice ] * [> `Post] ] a_s ], 
                     'tipo, 'gn, 'pn, [> `Registrable ])
                    service
(** Same as [new_post_coservice] followed by [register] *)

    val register_new_post_coservice' :
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
        post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
          ?error_handler:(Eliom_sessions.server_params -> (string * exn) list -> 
            page Lwt.t) ->
              (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
                (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                 [ `WithoutSuffix ], unit, 'pn,
                 [> `Registrable ])
                  service
(** Same as [new_post_coservice'] followed by [register] *)

(*
    val register_new_get_post_coservice' :
        ?sp: Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
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
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          fallback:('get, unit, 
                    [ `Attached of [ `Internal of
                      [< `Service | `Coservice ] * [`Get] ] a_s ],
                    [< suff ] as 'tipo, 
                    'gn, unit, [ `Registrable ])
            service ->
              post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
                ?error_handler:(Eliom_sessions.server_params -> 
                  (string * exn) list -> page Lwt.t) ->
                    (Eliom_sessions.server_params -> 'get -> 'post -> page Lwt.t) ->
                      ('get, 'post, 
                       [> `Attached of 
                         [> `Internal of [> `Coservice ] * [> `Post]] a_s ], 
                       'tipo, 'gn, 'pn, [> `Registrable ])
                        service
(** Same as [new_post_coservice] followed by [register_for_session] *)

    val register_new_post_coservice_for_session' :
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
          post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
            ?error_handler:(Eliom_sessions.server_params -> 
              (string * exn) list -> page Lwt.t) ->
                (Eliom_sessions.server_params -> unit -> 'post -> page Lwt.t) ->
                  (unit, 'post, [> `Nonattached of [> `Post] na_s ], 
                   [ `WithoutSuffix ], unit, 'pn, 
                   [> `Registrable ])
                    service
(** Same as [new_post_coservice'] followed by [register_for_session] *)

(*
    val register_new_get_post_coservice_for_session' :
        ?session_name:string ->
        sp:Eliom_sessions.server_params ->
        ?max_use:int ->
        ?timeout:float ->
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


  
  
module type ELIOMREGSIG =
  sig
    include ELIOMREGSIG1
    module Cookies : ELIOMREGSIG1 
    with type page = page * Eliom_services.cookie list
  end



module MakeRegister : functor (Pages: REGCREATE) -> ELIOMREGSIG with 
type page = Pages.page
