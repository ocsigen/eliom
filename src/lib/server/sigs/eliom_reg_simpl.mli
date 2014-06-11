open Eliom_lib
open Eliom_parameter
open Eliom_service

(** {2 Service registration } *)

(** The function [register service handler] will associate the
    [service] to the function [handler]. The [handler] function take
    two parameters, the GET and POST parameters of the current HTTP
    request, and should returns the corresponding page.

    The optional parameter [~scope] is {!Eliom_common.global_scope} by
    default, see the Eliom manual for detailled description {%
    <<a_manual chapter="server-services" fragment="scope"|of different
    scope>>%}.

    The optional parameter [~options] is specific to each output
    module, see the type description for more information.

    The optional parameters [?charset], [?code], [?content_type] and
    [?headers] can be used to modify the HTTP answer sent by
    Eliom. Use this with care.

    The optional parameter [~secure_session] has no effect for scope
    {!Eliom_common.global_scope}. With other scopes, the parameter is used
    to force the session service table in which the [handler] will be
    registered. By default, the service is registred in the unsecure
    session if the current request's protocol is [http], or in the
    secure session if the protocol is [https]. If set to [false]
    (resp. [true]) the [handler] will be stored in the unsecure
    (resp. secure) session. See the Eliom manual for an introduction
    to {% <<a_manual chapter="server-state"|secure state>>%}.

    The optional parameter [~error_handler] is used to specialize the
    error page when actual parameters aren't compatible with the
    expected type. The default error handler is
    [ fun l -> raise (]{!Eliom_common.Eliom_Typing_Error}[ l) ].
*)
val register :
  ?scope:[<Eliom_common.scope] ->
  ?options:options ->
  ?charset:string ->
  ?code: int ->
  ?content_type:string ->
  ?headers: Http_headers.t ->
  ?secure_session:bool ->
  service:('get, 'post, [< service_method], [< attached],
           [< internal_service_kind ],
           [< suff ], 'gn, 'pn, [ `Registrable ], returnT) service ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> 'post -> page Lwt.t) ->
  unit
(* FIXME: secure_session is called "secure" in Eliom_state and Eliom_Service.unregister. *)

(** Same as {!Eliom_service.Http.service} followed by {!register}. *)
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
  ('get, unit, [> `Get], [> Eliom_service.attached_kind], [> `Service ],
   'tipo, 'gn, unit,
   [> `Registrable ], returnB) service

(** Same as {!Eliom_service.Http.coservice} followed by {!register}. *)
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
  fallback:(unit, unit, [`Get], [Eliom_service.attached_kind], [`Service ],
            [ `WithoutSuffix ] as 'tipo,
            unit, unit, [< registrable ], returnT)
    service ->
  get_params:
    ('get, [`WithoutSuffix], 'gn) params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> unit -> page Lwt.t) ->
  ('get, unit,[> `Get], [> Eliom_service.attached_kind], [> `AttachedCoservice ],
   'tipo, 'gn, unit,
   [> `Registrable ], returnB)
    service


(** Same as {!Eliom_service.Http.coservice'} followed by {!register}. *)
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
  ('get, unit,[> `Get], [> Eliom_service.non_attached_kind], [> `NonattachedCoservice],
   'tipo, 'gn, unit, [> `Registrable ], returnB)
    service


(** Same as {!Eliom_service.Http.post_service} followed by {!register}. *)
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
  fallback:('get, unit, [`Get], [Eliom_service.attached_kind], [`AttachedCoservice | `Service] as 'kind,
            [< suff ] as 'tipo, 'gn,
            unit, [< `Registrable ], returnT)
    service ->
  post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> 'post -> page Lwt.t) ->
  ('get, 'post, [> `Post], [> Eliom_service.attached_kind], 'kind,
   'tipo, 'gn, 'pn, [> `Registrable ], returnB)
    service

(** Same as {!Eliom_service.Http.post_coservice} followed by {!register}. *)
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
  fallback:('get, unit , [<`Get],[<Eliom_service.attached_kind],[< `AttachedCoservice | `Service ],
            [< suff ] as 'tipo,
            'gn, unit, [< `Registrable ], returnT)
    service ->
  post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> 'post -> page Lwt.t) ->
  ('get, 'post,[>`Post],[> Eliom_service.attached_kind],[> `AttachedCoservice ],
   'tipo, 'gn, 'pn, [> `Registrable ], returnB)
    service

(** Same as {!Eliom_service.Http.post_coservice'} followed by {!register}. *)
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
  (unit, 'post, [>`Post],[> Eliom_service.non_attached_kind],[> `NonattachedCoservice],
   [ `WithoutSuffix ], unit, 'pn,
   [> `Registrable ], returnB)
    service

(** Same as {!Eliom_service.Http.put_service} followed by {!register}. *)
val register_put_service :
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
  ('get -> raw_post_data -> page Lwt.t) ->
  ('get, raw_post_data, [> `Put], [> Eliom_service.attached_kind],[> `Service ],
   'tipo, 'gn, no_param_name,
   [> `Registrable ], returnB) service

(** Same as {!Eliom_service.Http.put_coservice} followed by {!register}. *)
val register_put_coservice :
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
  fallback:(unit, raw_post_data,[`Put],[ Eliom_service.attached_kind],[ `Service ],
            [ `WithoutSuffix ] as 'tipo,
            unit, no_param_name, [< registrable ], returnT)
    service ->
  get_params:
    ('get, [`WithoutSuffix], 'gn) params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> raw_post_data -> page Lwt.t) ->
  ('get, raw_post_data, [> `Put], [> Eliom_service.attached_kind],[> `AttachedCoservice ],
   'tipo, 'gn, no_param_name,
   [> `Registrable ], returnB)
    service

(** Same as {!Eliom_service.Http.put_coservice'} followed by {!register}. *)
val register_put_coservice' :
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
  ('get -> raw_post_data -> page Lwt.t) ->
  ('get, raw_post_data, [> `Put],[> Eliom_service.non_attached_kind],[> `NonattachedCoservice],
   'tipo, 'gn, no_param_name, [> `Registrable ], returnB)
    service

(** Same as {!Eliom_service.Http.delete_service} followed by {!register}. *)
val register_delete_service :
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
  ('get -> raw_post_data -> page Lwt.t) ->
  ('get, raw_post_data, [> `Delete], [> Eliom_service.attached_kind],[> `Service ],
   'tipo, 'gn, no_param_name,
   [> `Registrable ], returnB) service

(** Same as {!Eliom_service.Http.delete_coservice} followed by {!register}. *)
val register_delete_coservice :
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
  fallback:(unit, raw_post_data,[`Delete],[ Eliom_service.attached_kind],[ `Service ],
            [ `WithoutSuffix ] as 'tipo,
            unit, no_param_name, [< registrable ], returnT)
    service ->
  get_params:
    ('get, [`WithoutSuffix], 'gn) params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> raw_post_data -> page Lwt.t) ->
  ('get, raw_post_data, [> `Delete], [> Eliom_service.attached_kind], [> `AttachedCoservice ],
   'tipo, 'gn, no_param_name,
   [> `Registrable ], returnB)
    service

(** Same as {!Eliom_service.Http.delete_coservice'} followed by {!register}. *)
val register_delete_coservice' :
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
  ('get -> raw_post_data -> page Lwt.t) ->
  ('get, raw_post_data, [> `Delete], [> Eliom_service.non_attached_kind ], [> `NonattachedCoservice],
   'tipo, 'gn, no_param_name, [> `Registrable ], returnB)
    service

(** {2 Low-level function } *)

(** The function [send page] build the HTTP frame corresponding to
    [page]. This may be used for example in an service handler
    registered with {!Eliom_registration.Any.register} or when building a
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
