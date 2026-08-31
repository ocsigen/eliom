# Module `Eliom_state.Ext`

```ocaml
exception Wrong_scope
```
Exception raised when you try to access a reference belonging to a scope different to the state's scope

```ocaml
type timeout = 
  | TGlobal (* see global setting *)
  | TNone (* explicitly set no timeout *)
  | TSome of float (* timeout duration in seconds *)
```
Type used to describe session timeouts

These types are used to get or set information about browser or process cookies (like timeouts).

```ocaml
type service_cookie_info
```
```ocaml
type data_cookie_info
```
```ocaml
type persistent_cookie_info
```
```ocaml
type (+'a, +'b) state
```
The type of states. The first parameter corresponds to the scope level and the second one to the kind of state (volatile or persistent data, or service state)

```ocaml
val volatile_data_group_state : 
  ?scope:Eliom_common.session_group_scope ->
  string ->
  ([> `Session_group ], [> `Data ]) state
```
`volatile_data_group_state ~scope n` returns the state corresponding to the group named `n` in scope `scope`.

```ocaml
val persistent_data_group_state : 
  ?scope:Eliom_common.session_group_scope ->
  string ->
  ([> `Session_group ], [> `Pers ]) state
```
Same for persistent data

```ocaml
val service_group_state : 
  ?scope:Eliom_common.session_group_scope ->
  string ->
  ([> `Session_group ], [> `Service ]) state
```
Same for services

```ocaml
val current_volatile_data_state : 
  ?secure:bool ->
  ?scope:Eliom_common.user_scope ->
  unit ->
  ([< Eliom_common.user_level ], [< `Data ]) state
```
`current_volatile_data_state ~scope` returns the state corresponding to scope `scope`. Raises `Not_found` if not connected or if no session group is set, or `Eliom_common.Eliom_Session_expired` if a cookie was present but expired.

```ocaml
val current_persistent_data_state : 
  ?secure:bool ->
  ?scope:Eliom_common.user_scope ->
  unit ->
  ([< Eliom_common.user_level ], [< `Pers ]) state Lwt.t
```
Same for persistent data

```ocaml
val current_service_state : 
  ?secure:bool ->
  ?scope:Eliom_common.user_scope ->
  unit ->
  ([< Eliom_common.user_level ], [< `Service ]) state
```
Same for services

```ocaml
val discard_state : 
  ?sitedata:Eliom_common.sitedata ->
  state:('a, 'b) state ->
  unit ->
  unit Lwt.t
```
Discard external states. See [`fold_volatile_sub_states`](./#val-fold_volatile_sub_states) for explanation about the `?sitedata` parameter.

```ocaml
val fold_volatile_sub_states : 
  ?sitedata:Eliom_common.sitedata ->
  state:([< `Session_group | `Session ], [< `Data | `Service ] as 'k) state ->
  ('a -> ([< `Session | `Client_process ], 'k) state -> 'a) ->
  'a ->
  'a
```
Fold all sessions in a groups, or all client processes in a session. If you do not call the function during a request or during the initialisation phase of the Eliom module, you must provide the extra parameter `?sitedata`, that you can get by calling [`Eliom_request_info.get_sitedata`](./Eliom_request_info.md#val-get_sitedata) during the initialisation phase of the Eliom module.

```ocaml
val iter_volatile_sub_states : 
  ?sitedata:Eliom_common.sitedata ->
  state:([< `Session_group | `Session ], [< `Data | `Service ] as 'k) state ->
  (([< `Session | `Client_process ], 'k) state -> unit) ->
  unit
```
Iter on all sessions in a groups, or all client processes in a session. See [`fold_volatile_sub_states`](./#val-fold_volatile_sub_states) for explanation about the `?sitedata` parameter.

```ocaml
val fold_sub_states : 
  ?sitedata:Eliom_common.sitedata ->
  state:
    ([< `Session_group | `Session ], [< `Data | `Pers | `Service ] as 'k) state ->
  ('a -> ([< `Session | `Client_process ], 'k) state -> 'a Lwt.t) ->
  'a ->
  'a Lwt.t
```
Fold all sessions in a groups, or all client processes in a session (volatile and persistent). See [`fold_volatile_sub_states`](./#val-fold_volatile_sub_states) for explanation about the `?sitedata` parameter.

```ocaml
val iter_sub_states : 
  ?sitedata:Eliom_common.sitedata ->
  state:([< `Session_group | `Session ], 'k) state ->
  (([< `Session | `Client_process ], 'k) state -> unit Lwt.t) ->
  unit Lwt.t
```
Iter on all sessions in a groups, or all client processes in a session (volatile and persistent). See [`fold_volatile_sub_states`](./#val-fold_volatile_sub_states) for explanation about the `?sitedata` parameter.

```ocaml
module Low_level : sig ... end
```
Functions to access table data. Prefer using Eliom references.

```ocaml
val get_service_cookie_info : 
  ?sitedata:Eliom_common.sitedata ->
  ([< Eliom_common.cookie_level ], [ `Service ]) state ->
  service_cookie_info
```
Get the infomration about cookies (timeouts, etc.). See [`fold_volatile_sub_states`](./#val-fold_volatile_sub_states) for explanation about the `?sitedata` parameter.

```ocaml
val get_volatile_data_cookie_info : 
  ?sitedata:Eliom_common.sitedata ->
  ([< Eliom_common.cookie_level ], [ `Data ]) state ->
  data_cookie_info
```
See [`fold_volatile_sub_states`](./#val-fold_volatile_sub_states) for explanation about the `?sitedata` parameter.

```ocaml
val get_persistent_cookie_info : 
  ([< Eliom_common.cookie_level ], [ `Pers ]) state ->
  persistent_cookie_info Lwt.t
```
```ocaml
val get_service_cookie_scope : 
  cookie:service_cookie_info ->
  Eliom_common.user_scope
```
```ocaml
val get_volatile_data_cookie_scope : 
  cookie:data_cookie_info ->
  Eliom_common.user_scope
```
```ocaml
val get_persistent_data_cookie_scope : 
  cookie:persistent_cookie_info ->
  Eliom_common.user_scope
```
```ocaml
val set_service_cookie_timeout : 
  cookie:service_cookie_info ->
  float option ->
  unit
```
```ocaml
val set_volatile_data_cookie_timeout : 
  cookie:data_cookie_info ->
  float option ->
  unit
```
```ocaml
val set_persistent_data_cookie_timeout : 
  cookie:persistent_cookie_info ->
  float option ->
  unit Lwt.t
```
```ocaml
val get_service_cookie_timeout : cookie:service_cookie_info -> timeout
```
```ocaml
val get_volatile_data_cookie_timeout : cookie:data_cookie_info -> timeout
```
```ocaml
val get_persistent_data_cookie_timeout : 
  cookie:persistent_cookie_info ->
  timeout
```
```ocaml
val unset_service_cookie_timeout : cookie:service_cookie_info -> unit
```
```ocaml
val unset_volatile_data_cookie_timeout : cookie:data_cookie_info -> unit
```
```ocaml
val unset_persistent_data_cookie_timeout : 
  cookie:persistent_cookie_info ->
  unit Lwt.t
```
```ocaml
val get_session_group_list : unit -> string list
```
Returns a list containing the names of all session group that are available for this site.

```ocaml
val iter_service_cookies : (service_cookie_info -> unit Lwt.t) -> unit Lwt.t
```
Iterator on all active service cookies. `Lwt.pause` is called automatically after each iteration.

```ocaml
val iter_volatile_data_cookies : (data_cookie_info -> unit Lwt.t) -> unit Lwt.t
```
Iterator on data cookies. `Lwt.pause` is called automatically after each iteration.

```ocaml
val iter_persistent_data_cookies : 
  (persistent_cookie_info -> unit Lwt.t) ->
  unit Lwt.t
```
Iterator on persistent cookies. `Lwt.pause` is called automatically after each iteration.

```ocaml
val fold_service_cookies : 
  (service_cookie_info -> 'b -> 'b Lwt.t) ->
  'b ->
  'b Lwt.t
```
Iterator on service cookies. `Lwt.pause` is called automatically after each iteration.

```ocaml
val fold_volatile_data_cookies : 
  (data_cookie_info -> 'b -> 'b Lwt.t) ->
  'b ->
  'b Lwt.t
```
Iterator on data cookies. `Lwt.pause` is called automatically after each iteration.

```ocaml
val fold_persistent_data_cookies : 
  (persistent_cookie_info -> 'b -> 'b Lwt.t) ->
  'b ->
  'b Lwt.t
```
Iterator on persistent cookies. `Lwt.pause` is called automatically after each iteration.
