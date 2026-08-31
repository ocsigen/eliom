# Module `Eliom_client`

```ocaml
val is_client_app : unit -> bool
```
Returns whether the application is sent by a server or started on client side. If called on server side, always returns `false`. Otherwise, it tests the presence of JS variables added automatically by Eliom when the page is sent by a server. Example:

```ocaml
    if not (Eliom_client.is_client_app ())
then Eliom_client.init_client_app ... 
```

### RPC / Server functions

See the [manual](./../clientserver-communication.md#rpc).

```ocaml
type ('a, 'b) server_function
```
A value of type `('a, 'b) server_function` is created on the server from a function `'a -> 'b Lwt.t` and provides a given function on the client side. See also [the concrete client side representation](./#type-server_function).

```ocaml
val server_function : 
  ?scope:[< Eliom_common.scope ] ->
  ?options:unit ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Ocsigen_header.t ->
  ?secure_session:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Eliom_common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?error_handler:((string * exn) list -> 'b Lwt.t) ->
  'a Deriving_Json.t ->
  ('a -> 'b Lwt.t) ->
  ('a, 'b) server_function
```
`server_function argument_type f` creates a value of type [`Eliom_client.server_function`](./#type-server_function). This allows to call `f` from the client. The first argument `argument_type` is an instance of `Deriving_Json` for the type of the argument. It is used to safely encode and decode the argument sent to the server.

The optional parameters correspond directly to the optional parameters of `Eliom_registration.Ocaml.register_coservice'`.

See also the [manual](./../clientserver-communication.md#rpc).
