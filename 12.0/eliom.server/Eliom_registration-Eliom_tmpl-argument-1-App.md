
# Parameter `Eliom_tmpl.App`

```ocaml
val application_script : 
  ?defer:bool ->
  ?async:bool ->
  unit ->
  [> `Script ] Eliom_content.Html.elt
```
The function `application_name ()` returns a `<script>` node that represents the javascript part of the application. If you do not include this script in the `<head>` node of your page, it will be automatically added at the end of the `<head>` node.

```ocaml
val wasm_detection_script : 
  ?defer:bool ->
  ?async:bool ->
  ?js_name:string ->
  ?wasm_name:string ->
  unit ->
  [> `Script ] Eliom_content.Html.elt
```
The function `wasm_detection_script ()` returns an inline `<script>` node that automatically detects WebAssembly support in the browser and dynamically loads the appropriate version of the application.

When the browser supports WebAssembly with GC (`window?.WebAssembly?.JSTag` is defined, which is required by wasm\_of\_ocaml), it loads the WASM version (`.wasm.js`). Otherwise, it falls back to the JavaScript version (`.js`).

This function is automatically used by Eliom when WebAssembly support is enabled (see [`Eliom_config.set_enable_wasm`](./Eliom_config.md#val-set_enable_wasm)). The generated script creates a `<script>` element dynamically with the appropriate `src` attribute and appends it to the document head.

parameter defer If true, sets the defer attribute on the dynamically created script tag. Default comes from the site's application\_script configuration.
parameter async If true, sets the async attribute on the dynamically created script tag. Default comes from the site's application\_script configuration.
parameter js\_name Optional filename for the JavaScript fallback version. Useful for specifying cache-busting hashed filenames. Default is application\_name ^ ".js".
parameter wasm\_name Optional filename for the WebAssembly version. Useful for specifying cache-busting hashed filenames. Default is application\_name ^ ".wasm.js".
*Warning: Like [`application_script`](./#val-application_script), do not manually include this script in the `<head>` node of your page when using Eliom's automatic script injection.*

```ocaml
val application_name : string
```
Unique identifier for this application. Currently, it is just the application name as defined by `Appl_params.application_name`.

*Warning: do not mix up with the "application instance id", that is unique for each instance of the application on a client.*

```ocaml
val is_initial_request : unit -> bool
```
Checks during a request whether it is the initial request of the client process in this Eliom application.

```ocaml
type app_id
```
The type `appl` is an abstract type for identifying an application. It usually used a phantom parameter for [`application_content`](./Eliom_registration.md#type-application_content).

```ocaml
type page = Html_types.html Eliom_content.Html.elt
```
```ocaml
type options = appl_service_options
```
```ocaml
type return = Eliom_service.non_ocaml
```
```ocaml
type result = app_id application_content kind
```

### Service registration

```ocaml
val register : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
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
      return)
      Eliom_service.t ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('get -> 'post -> page Lwt.t) ->
  unit
```
The function `register ~service handler` associates the `service` to the function `handler`. The `handler` function takes two parameters, the GET and POST parameters of the current HTTP request, and should return the corresponding page.

The optional parameter `~scope` is [`Eliom_common.global_scope`](./Eliom_common.md#type-global_scope) by default. See the Eliom manual for detailed description `of different scopes`.

The optional parameter `~options` is specific to each output module. See the type description for more information.

The optional parameters `?charset`, `?code`, `?content_type` and `?headers` can be used to modify the HTTP answer sent by Eliom. Use this with care.

The optional parameter `~secure_session` has no effect for scope [`Eliom_common.global_scope`](./Eliom_common.md#type-global_scope). With other scopes, the parameter is used to force the session service table in which the `handler` will be registered. By default, the service is registered in the non-secure session if the current request's protocol is `http`, or in the secure session if the protocol is `https`. If set to `false` (resp. `true`) the `handler` will be stored in the non-secure (resp. secure) session. See the Eliom manual for an introduction to [secure state](./../server-state.md).

The optional parameter `~error_handler` is used to specialize the error page when actual parameters aren't compatible with the expected type. The default error handler is ` fun l -> raise (`[`Eliom_common.Eliom_Typing_Error`](./Eliom_common.md#exception-Eliom_Typing_Error)` l) `.

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
The function `send page` builds the HTTP frame corresponding to `page`. This may be used for example in a service handler registered with [`Eliom_registration.Any.register`](./Eliom_registration-Any.md#val-register), or when building a custom output module.

```ocaml
val create : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
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
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('gp -> 'pp -> page Lwt.t) ->
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
    return)
    Eliom_service.t
```
Create a service and register it at the same time. It calls [`Eliom_service.create`](./Eliom_service.md#val-create) and then performs [`Eliom_registration_sigs.S.register`](./Eliom_registration_sigs-module-type-S.md#val-register). Returns the service.

```ocaml
val create_attached_get : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
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
      return)
      Eliom_service.t ->
  get_params:('gp, [ `WithoutSuffix ], 'gn) Eliom_parameter.params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('gp -> unit -> page Lwt.t) ->
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
    return)
    Eliom_service.t
```
Create an attached service and register it at the same time. It calls [`Eliom_service.create_attached_get`](./Eliom_service.md#val-create_attached_get) and then performs [`Eliom_registration_sigs.S.register`](./Eliom_registration_sigs-module-type-S.md#val-register). Returns the new service.

```ocaml
val create_attached_post : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
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
      return)
      Eliom_service.t ->
  post_params:('pp, [ `WithoutSuffix ], 'pn) Eliom_parameter.params_type ->
  ?error_handler:((string * exn) list -> page Lwt.t) ->
  ('gp -> 'pp -> page Lwt.t) ->
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
    return)
    Eliom_service.t
```
Create an attached POST service and register it at the same time. It calls [`Eliom_service.create_attached_post`](./Eliom_service.md#val-create_attached_post) and then performs [`Eliom_registration_sigs.S.register`](./Eliom_registration_sigs-module-type-S.md#val-register). Returns the new service.
