
# Module `Ext.Low_level`

Functions to access table data. Prefer using Eliom references.

```ocaml
val get_volatile_data : 
  state:([< `Session_group | `Session | `Client_process ], [< `Data ]) state ->
  table:'a volatile_table ->
  'a
```
Raises `Not_found` if no data in the table for the cookie.

```ocaml
val get_persistent_data : 
  state:([< `Session_group | `Session | `Client_process ], [< `Pers ]) state ->
  table:'a persistent_table ->
  'a Lwt.t
```
Fails with lwt exception `Not_found` if no data in the table for the cookie.

```ocaml
val set_volatile_data : 
  state:([< `Session_group | `Session | `Client_process ], [< `Data ]) state ->
  table:'a volatile_table ->
  'a ->
  unit
```
```ocaml
val set_persistent_data : 
  state:([< `Session_group | `Session | `Client_process ], [< `Pers ]) state ->
  table:'a persistent_table ->
  'a ->
  unit Lwt.t
```
Fails with lwt exception `Not_found` if no data in the table for the cookie.

```ocaml
val remove_volatile_data : 
  state:([< `Session_group | `Session | `Client_process ], [< `Data ]) state ->
  table:'a volatile_table ->
  unit
```
```ocaml
val remove_persistent_data : 
  state:([< `Session_group | `Session | `Client_process ], [< `Pers ]) state ->
  table:'a persistent_table ->
  unit Lwt.t
```