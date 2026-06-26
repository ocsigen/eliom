
# Module `Reference.Ext`

This module allows access to references for other groups, sessions, or client processes. Use it in conjunction with functions like [`State.Ext.iter_sub_states`](./Eliom-State-Ext.md#val-iter_sub_states) to get the sessions from a group (or the processes from a session).

```ocaml
val get : 
  ([< `Session_group | `Session | `Client_process ], [< `Data | `Pers ])
    State.Ext.state ->
  'a eref ->
  'a Lwt.t
```
get the value of a reference from outside the state. If the value has not been set yet for this state, it will raise exception `Eref_not_initialized`.

```ocaml
val set : 
  ([< `Session_group | `Session | `Client_process ], [< `Data | `Pers ])
    State.Ext.state ->
  'a eref ->
  'a ->
  unit Lwt.t
```
```ocaml
val modify : 
  ([< `Session_group | `Session | `Client_process ], [< `Data | `Pers ])
    State.Ext.state ->
  'a eref ->
  ('a -> 'a) ->
  unit Lwt.t
```
Warning: the function will be executed with the current context

```ocaml
val unset : 
  ([< `Session_group | `Session | `Client_process ], [< `Data | `Pers ])
    State.Ext.state ->
  'a eref ->
  unit Lwt.t
```