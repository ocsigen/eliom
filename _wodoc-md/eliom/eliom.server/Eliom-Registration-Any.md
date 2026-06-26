
# Module `Registration.Any`

Eliom service registration for services that choose dynamically what they want to send. The content is created using for example [`Html.send`](./Eliom-Registration-Html.md#val-send) or [`String.send`](./Eliom-Registration-String.md#val-send) functions. See the Eliom manual for more information about [services that choose dynamically what they want to send](./../server-outputs.md#any)

```ocaml
type 'a page = 'a kind
```
```ocaml
type options = unit
```
```ocaml
type 'a return = Service.non_ocaml
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

```ocaml
type 'a result = 'a kind
```
```ocaml
val send : 
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  'a page ->
  'a result Lwt.t
```