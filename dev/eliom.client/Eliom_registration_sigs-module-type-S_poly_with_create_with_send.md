# Module type `Eliom_registration_sigs.S_poly_with_create_with_send`

```ocaml
type _ page
```
```ocaml
type options
```
```ocaml
type _ return
```
```ocaml
val register : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Eliom_service.Cohttp.Header.t ->
  ?secure_session:bool ->
  service:
    ('get,
      'post,
      _,
      _,
      _,
      Eliom_service.non_ext,
      Eliom_service.reg,
      _,
      _,
      _,
      'a return)
      Eliom_service.t ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('get -> 'post -> 'a page Lwt.t) ->
  unit
```
See [`S.register`](./Eliom_registration_sigs-module-type-S.md#val-register).

```ocaml
val create : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Eliom_service.Cohttp.Header.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Eliom_common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp_) Eliom_service.meth ->
  path:('att, 'co, 'gp_) Eliom_service.path_option ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('gp -> 'pp -> 'a page Lwt.t) ->
  ('gp,
    'pp,
    'm,
    'att,
    'co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    'tipo,
    'gn,
    'pn,
    'a return)
    Eliom_service.t
```
See [`S_with_create.create`](./Eliom_registration_sigs-module-type-S_with_create.md#val-create).

```ocaml
val create_attached_get : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Eliom_service.Cohttp.Header.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Eliom_common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  fallback:
    (unit,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      _,
      [ `WithoutSuffix ],
      unit,
      unit,
      'a return)
      Eliom_service.t ->
  get_params:('gp, [ `WithoutSuffix ], 'gn) Eliom_parameter.params_type ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('gp -> unit -> 'a page Lwt.t) ->
  ('gp,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    'gn,
    unit,
    'a return)
    Eliom_service.t
```
See [`S_with_create.create_attached_get`](./Eliom_registration_sigs-module-type-S_with_create.md#val-create_attached_get).

```ocaml
val create_attached_post : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Eliom_service.Cohttp.Header.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Eliom_common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  fallback:
    ('gp,
      unit,
      Eliom_service.get,
      Eliom_service.att,
      Eliom_service.non_co,
      Eliom_service.non_ext,
      _,
      [ `WithoutSuffix ],
      'gn,
      unit,
      'a return)
      Eliom_service.t ->
  post_params:('pp, [ `WithoutSuffix ], 'pn) Eliom_parameter.params_type ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('gp -> 'pp -> 'a page Lwt.t) ->
  ('gp,
    'pp,
    Eliom_service.post,
    Eliom_service.att,
    Eliom_service.co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    'gn,
    'pn,
    'a return)
    Eliom_service.t
```
See [`S_with_create.create_attached_post`](./Eliom_registration_sigs-module-type-S_with_create.md#val-create_attached_post).

```ocaml
type 'a result
```
```ocaml
val send : 
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Eliom_service.Cohttp.Header.t ->
  'a page ->
  'a result Lwt.t
```
