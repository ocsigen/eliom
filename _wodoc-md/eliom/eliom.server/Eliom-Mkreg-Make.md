
# Module `Mkreg.Make`


### Creates modules to register services for one type of pages


## Parameters

```ocaml
module Pages : Registration_sigs.PARAM with type frame := Ocsigen.Response.t
```

## Signature

```ocaml
type page = Pages.page
```
```ocaml
type options = Pages.options
```
```ocaml
type return = Service.non_ocaml
```
```ocaml
type result = Pages.result
```

### Service registration

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
    ('get, 'post, _, _, _, Service.non_ext, Service.reg, _, _, _, return)
      Service.t ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> 'post -> page Lwt.t) ->
  unit
```
The function `register ~service handler` associates the `service` to the function `handler`. The `handler` function takes two parameters, the GET and POST parameters of the current HTTP request, and should return the corresponding page.

The optional parameter `~scope` is [`Common.global_scope`](./Eliom-Common.md#type-global_scope) by default. See the Eliom manual for detailed description [of different scopes](./../server-services.md#service_scope).

The optional parameter `~options` is specific to each output module. See the type description for more information.

The optional parameters `?charset`, `?code`, `?content_type` and `?headers` can be used to modify the HTTP answer sent by Eliom. Use this with care.

The optional parameter `~secure_session` has no effect for scope [`Common.global_scope`](./Eliom-Common.md#type-global_scope). With other scopes, the parameter is used to force the session service table in which the `handler` will be registered. By default, the service is registered in the non-secure session if the current request's protocol is `http`, or in the secure session if the protocol is `https`. If set to `false` (resp. `true`) the `handler` will be stored in the non-secure (resp. secure) session. See the Eliom manual for an introduction to [secure state](./../server-state.md).

The optional parameter `~error_handler` is used to specialize the error page when actual parameters aren't compatible with the expected type. The default error handler is ` fun l -> raise (`[`Common.Eliom_Typing_Error`](./Eliom-Common.md#exception-Eliom_Typing_Error)` l) `.

```ocaml
val send : 
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  page ->
  result Lwt.t
```
The function `send page` builds the HTTP frame corresponding to `page`. This may be used for example in a service handler registered with [`Registration.Any.register`](./Eliom-Registration-Any.md#val-register), or when building a custom output module.

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
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('gp -> 'pp -> page Lwt.t) ->
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
    return)
    Service.t
```
Create a service and register it at the same time. It calls [`Service.create`](./Eliom-Service.md#val-create) and then performs [`Registration_sigs.S.register`](./Eliom-Registration_sigs-module-type-S.md#val-register). Returns the service.

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
      return)
      Service.t ->
  get_params:('gp, [ `WithoutSuffix ], 'gn) Parameter.params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('gp -> unit -> page Lwt.t) ->
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
    return)
    Service.t
```
Create an attached service and register it at the same time. It calls [`Service.create_attached_get`](./Eliom-Service.md#val-create_attached_get) and then performs [`Registration_sigs.S.register`](./Eliom-Registration_sigs-module-type-S.md#val-register). Returns the new service.

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
      return)
      Service.t ->
  post_params:('pp, [ `WithoutSuffix ], 'pn) Parameter.params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('gp -> 'pp -> page Lwt.t) ->
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
    return)
    Service.t
```
Create an attached POST service and register it at the same time. It calls [`Service.create_attached_post`](./Eliom-Service.md#val-create_attached_post) and then performs [`Registration_sigs.S.register`](./Eliom-Registration_sigs-module-type-S.md#val-register). Returns the new service.
