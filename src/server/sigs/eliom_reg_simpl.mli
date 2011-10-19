open Eliom_pervasives
open Eliom_services
open Eliom_parameters

(** {2 Service registration } *)

(** The function [register service handler] will associate the
    [service] to the function [handler]. The [handler] function take
    two parameters, the GET and POST parameters of the current HTTP
    request, and should returns the corresponding page.

    The optional parameter [~scope] is {!Eliom_common.global} by
    default, see the Eliom manual for detailled description {%
    <<a_manual chapter="service" fragment="scope"|of different
    scope>>%}.

    The optional parameter [~options] is specific to each output
    module, see the type description for more information.

    The optional parameters [?charset], [?code], [?content_type] and
    [?headers] can be used to modify the HTTP answer sent by
    Eliom. Use this with care.

    The optionnal parameter [~secure_session] has no effect for scope
    {!Eliom_common.global}. With other scopes, the parameter is used
    to force the session service table in which the [handler] will be
    registered. By default, the service is registred in the unsecure
    session if the current request's protocol is [http], or in the
    secure session if the protocol is [https]. If set to [false]
    (resp. [true]) the [handler] will be stored in the unsecure
    (resp. secure) session. See the Eliom manual for an introduction
    to {% <<a_manual chapter="state"|secure state>>%}.

    The optional parameter [~error_handler] is used to specialize the
    error page when actual parameters aren't compatible with the
    expected type.
*)
val register :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  service:('get, 'post,
           [< internal_service_kind ],
           [< suff ], 'gn, 'pn, [ `Registrable ], return) service ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> 'post -> page Lwt.t) ->
  unit
(* FIXME: secure_session is called "secure" in Eliom_state and Eliom_Service.unregister. *)

(** Same as {!val:Eliom_services.service} followed by {!register}. *)
val register_service :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?priority:int ->
  path:Url.path ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> unit -> page Lwt.t) ->
  ('get, unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   'tipo, 'gn, unit,
   [> `Registrable ], return) service

(** Same as {!Eliom_services.coservice} followed by {!register}. *)
val register_coservice :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  ?name: string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [<Eliom_common.user_scope] ->
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


(** Same as {!Eliom_services.coservice'} followed by {!register}. *)
val register_coservice' :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  ?name: string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [<Eliom_common.user_scope] ->
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


(** Same as {!Eliom_services.post_service} followed by {!register}. *)
val register_post_service :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?priority:int ->
  fallback:('get, unit,
            [ `Attached of
                ([ `Internal of
                    ([ `Service | `Coservice ] as 'kind) ], [`Get]) a_s ],
            [< suff ] as 'tipo, 'gn,
            unit, [< `Registrable ], 'return2) (* 'return2 <> return *)
    service ->
  post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> 'post -> page Lwt.t) ->
  ('get, 'post, [> `Attached of
      ([> `Internal of 'kind ], [> `Post]) a_s ],
   'tipo, 'gn, 'pn, [> `Registrable ], return)
    service

(** Same as {!Eliom_services.post_coservice} followed by {!register}. *)
val register_post_coservice :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  ?name: string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [<Eliom_common.user_scope] ->
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

(** Same as {!Eliom_services.post_coservice'} followed by {!register}. *)
val register_post_coservice' :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  ?name: string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [<Eliom_common.user_scope] ->
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

(** {2 Low-level function } *)

(** The function [send page] build the HTTP frame corresponding to
    [page]. This may be used for example in an service handler
    registered with {!Eliom_output.Any.register} or when building a
    custom output module.
*)
val send :
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  page ->
  result Lwt.t

