
# Module `Registration.Html`

```ocaml
type page = Html_types.html Content.Html.elt
```
```ocaml
type options = unit
```
```ocaml
type return = Service.non_ocaml
```
```ocaml
type result = browser_content kind
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
  ?headers:Service.Cohttp.Header.t ->
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
  ?headers:Service.Cohttp.Header.t ->
  page ->
  result Lwt.t
```
The function `send page` builds the HTTP frame corresponding to `page`. This may be used for example in a service handler registered with [`Registration.Any.register`](./Eliom-Registration-Any.md#val-register), or when building a custom output module.
