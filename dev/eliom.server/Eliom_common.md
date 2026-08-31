# Module `Eliom_common`

Low level functions for Eliom, exceptions and types.

```ocaml
module Ocsipersist : module type of Ocsipersist
```
Persistent key-value store interface for OCaml. This is a virtual library defining a unified frontend for a number of key-value storage implementations. Implementations of the following backends currently exist: SQLite, DBM, PostgreSQL. You can choose the backend you prefer by installing packages `ocsipersist-sqlite`, `ocsipersist-dbm` or `ocsipersist-pgsql`.

```ocaml
type scope_hierarchy = Eliom_common_base.scope_hierarchy
```
Scopes

```ocaml
type cookie_scope = [ 
  | `Session of scope_hierarchy
  | `Client_process of scope_hierarchy
 ]
```
```ocaml
type user_scope = [ 
  | `Session_group of scope_hierarchy
  | cookie_scope
 ]
```
```ocaml
type scope = [ 
  | `Site
  | user_scope
 ]
```
```ocaml
type all_scope = [ 
  | scope
  | `Global
  | `Request
 ]
```
```ocaml
type cookie_level = [ 
  | `Session
  | `Client_process
 ]
```
```ocaml
type user_level = [ 
  | `Session_group
  | cookie_level
 ]
```
```ocaml
val cookie_scope_of_user_scope : [< user_scope ] -> [> cookie_scope ]
```
```ocaml
val cookie_level_of_user_scope : [< user_scope ] -> [> cookie_level ]
```
```ocaml
val level_of_user_scope : [< user_scope ] -> [> user_level ]
```
```ocaml
val scope_hierarchy_of_user_scope : [< user_scope ] -> scope_hierarchy
```
Eliom is using regular (browser) cookies but can also use its own browser tab cookies (only if you are using a client side Eliom application).

It is possible to define Eliom references or services for one (browser) session, for one tab, or for one group of sessions.

Using ``Global` scope means you want the data or service to be available to any client. ``Site` is limited to current sub-site (if you have several sites on the same server).

If you want to restrict the visibility of an Eliom reference or a service: \* to a browser session, use `~scope:Eliom_common.default_session_scope`, \* to a group of sessions, use `~scope:Eliom_common.default_group_scope`, \* to a client process, use `~scope:Eliom_common.default_process_scope`. If you have a client side Eliom program running, and you want to restrict the visibility of the service to this instance of the program, use `~scope:Eliom_common.default_process_scope`.

You can create new scope hierarchies with [`Eliom_common.create_scope_hierarchy`](./#val-create_scope_hierarchy). Thus it is possible to have for example several sessions that can be opened or closed independently. They use different cookies.

Secure scopes are associated to secure cookies (that is, cookies sent by browsers only if the protocol is https).

```ocaml
type global_scope = [ 
  | `Global
 ]
```
```ocaml
type site_scope = [ 
  | `Site
 ]
```
```ocaml
type session_group_scope = [ 
  | `Session_group of scope_hierarchy
 ]
```
```ocaml
type session_scope = [ 
  | `Session of scope_hierarchy
 ]
```
```ocaml
type client_process_scope = [ 
  | `Client_process of scope_hierarchy
 ]
```
```ocaml
type request_scope = [ 
  | `Request
 ]
```
```ocaml
val global_scope : [> global_scope ]
```
```ocaml
val site_scope : [> site_scope ]
```
```ocaml
val default_group_scope : [> session_group_scope ]
```
```ocaml
val default_session_scope : [> session_scope ]
```
```ocaml
val default_process_scope : [> client_process_scope ]
```
```ocaml
val comet_client_process_scope : [> client_process_scope ]
```
```ocaml
val request_scope : [> request_scope ]
```
```ocaml
val create_scope_hierarchy : string -> scope_hierarchy
```
```ocaml
val list_scope_hierarchies : unit -> scope_hierarchy list
```

### Exception and error handling

```ocaml
exception Eliom_404
```
Page not found

```ocaml
exception Eliom_Wrong_parameter
```
Service called with wrong parameter names

```ocaml
exception Eliom_Session_expired
```
```ocaml
exception Eliom_Typing_Error of (string * exn) list
```
The service (GET or POST) parameters do not match expected type

```ocaml
exception Eliom_site_information_not_available of string
```
That function cannot be used when the site information is not available, that is, outside a request or the initialisation phase of your Eliom module (while reading the configuration file).

In particular, you cannot use the function before the configuration file is read for example when you are using *static linking*. In that case you must delay the function call using [`Eliom_service.register_eliom_module`](./Eliom_service.md#val-register_eliom_module).

```ocaml
exception Cannot_call_this_function_before_app_is_linked_to_a_site
```
Statically linked app: You cannot call this function before `Eliom_run`.

```ocaml
type full_state_name = {
  user_scope : user_scope;
  secure : bool;
  site_dir_str : string;
}
```
```ocaml
module Full_state_name_table : Map.S with type key = full_state_name
```
```ocaml
val eliom_link_too_old : bool Polytables.key
```
If present and true in request data, it means that the previous coservice does not exist any more

```ocaml
val eliom_service_session_expired : 
  (full_state_name list * full_state_name list) Polytables.key
```
If present in request data, means that the service session cookies does not exist any more. The string lists are the list of names of expired sessions
