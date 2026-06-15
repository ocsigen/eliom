
# Module `Eliom_syntax`

```ocaml
val get_global_data : unit -> Eliom_runtime.global_data
```
```ocaml
val get_request_data : unit -> Eliom_runtime.request_data
```
```ocaml
val client_value : 
  ?pos:Eliom_lib.pos ->
  string ->
  'args ->
  'a Eliom_client_value.t
```
Registers a client value datum for the next server section when executed in a global\_data (cf. [`Eliom_syntax.set_global`](./#val-set_global)) or in the request\_data when executed in a request.

```ocaml
val set_global : bool -> unit
```
All client values created between `set_global true` and `set_global false` are considered global client values (cf. `the manual`).

```ocaml
val global_context : unit -> bool
```
Returns whether client values created in the current context should be considered global

```ocaml
val close_server_section : string -> unit
```
Called at the end of each server or shared section. The argument identifies the compilation unit.

Adds the list of recently registered [`Eliom_runtime.client_value_datum`](./Eliom_runtime.md#type-client_value_datum)s into the queue of server section data of the compilation unit (`Eliom_lib_base.compilation_unit_global_data`).

Called in parallel with `Eliom_client.Syntax_helpers.close_server_section`.

```ocaml
val close_client_section : 
  string ->
  (int * Ocsigen_lib.poly * Eliom_lib.pos * string option) list ->
  unit
```
Called at the end of every client or shared section. The first argument identifies the compilation unit. The second is the list of novel injections in that section.

Adds a list of `Eliom_lib_base.injection_datum`s into the queue of client section data of the compilation unit (`Eliom_lib_base.compilation_unit_global_data`).

Called in parallel with `Eliom_client.Syntax_helpers.open_client_section`.

```ocaml
val escaped_value : 'a -> Eliom_runtime.escaped_value
```
Convert any value to a [`Eliom_runtime.escaped_value`](./Eliom_runtime.md#type-escaped_value) for usage in the `args` argument to [`Eliom_syntax.client_value`](./#val-client_value).
