
# Module `Mkreg.Make_poly`


### Creating modules to register services for one type of parametrised pages


## Parameters

```ocaml
module Pages : Registration_sigs.PARAM_POLY with type frame := Ocsigen.Response.t
```

## Signature

```ocaml
type 'a page = 'a Pages.page
```
```ocaml
type options = Pages.options
```
```ocaml
type 'a return = 'a Pages.return
```
```ocaml
val register : 
  ?app:string ->
  ?scope:[< Common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  ?secure_session:bool ->
  service:
    ('get, 'post, _, _, _, Service.non_ext, Service.reg, _, _, _, 'a return)
      Service.t ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('get -> 'post -> 'a page Lwt.t) ->
  unit
```
See `S.register`.

```ocaml
val create : 
  ?app:string ->
  ?scope:[< Common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp_) Service.meth ->
  path:('att, 'co, 'gp_) Service.path_option ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('gp -> 'pp -> 'a page Lwt.t) ->
  ('gp,
    'pp,
    'm,
    'att,
    'co,
    Service.non_ext,
    Service.reg,
    'tipo,
    'gn,
    'pn,
    'a return)
    Service.t
```
See `S_with_create.create`.

```ocaml
val create_attached_get : 
  ?app:string ->
  ?scope:[< Common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  fallback:
    (unit,
      unit,
      Service.get,
      Service.att,
      Service.non_co,
      Service.non_ext,
      _,
      [ `WithoutSuffix ],
      unit,
      unit,
      'a return)
      Service.t ->
  get_params:('gp, [ `WithoutSuffix ], 'gn) Parameter.params_type ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('gp -> unit -> 'a page Lwt.t) ->
  ('gp,
    unit,
    Service.get,
    Service.att,
    Service.co,
    Service.non_ext,
    Service.reg,
    [ `WithoutSuffix ],
    'gn,
    unit,
    'a return)
    Service.t
```
See `S_with_create.create_attached_get`.

```ocaml
val create_attached_post : 
  ?app:string ->
  ?scope:[< Common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  ?secure_session:bool ->
  ?https:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  fallback:
    ('gp,
      unit,
      Service.get,
      Service.att,
      Service.non_co,
      Service.non_ext,
      _,
      [ `WithoutSuffix ],
      'gn,
      unit,
      'a return)
      Service.t ->
  post_params:('pp, [ `WithoutSuffix ], 'pn) Parameter.params_type ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('gp -> 'pp -> 'a page Lwt.t) ->
  ('gp,
    'pp,
    Service.post,
    Service.att,
    Service.co,
    Service.non_ext,
    Service.reg,
    [ `WithoutSuffix ],
    'gn,
    'pn,
    'a return)
    Service.t
```
See `S_with_create.create_attached_post`.
