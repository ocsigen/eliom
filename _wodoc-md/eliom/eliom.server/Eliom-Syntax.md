
# Module `Eliom.Syntax`

```ocaml
val get_global_data : unit -> Runtime.global_data
```
```ocaml
val get_request_data : unit -> Runtime.request_data
```
```ocaml
val to_poly : 'a -> Ocsigen_lib_base.poly
```
Used by the PPX to serialize injection values.

```ocaml
val client_value : ?pos:Lib.pos -> string -> 'args -> 'a Client_value.t
```
Registers a client value datum for the next server section when executed in a global\_data (cf. [`Syntax.set_global`](./#val-set_global)) or in the request\_data when executed in a request.

```ocaml
val set_global : bool -> unit
```
All client values created between `set_global true` and `set_global false` are considered global client values (cf. [the manual](./../eliom-language.md)).

```ocaml
val global_context : unit -> bool
```
Returns whether client values created in the current context should be considered global

```ocaml
val close_server_section : string -> unit
```
Called at the end of each server or shared section. The argument identifies the compilation unit.

Adds the list of recently registered [`Runtime.client_value_datum`](./Eliom-Runtime.md#type-client_value_datum)s into the queue of server section data of the compilation unit (`Lib_base.compilation_unit_global_data`).

Called in parallel with `Client.Syntax_helpers.close_server_section`.

```ocaml
val close_client_section : 
  string ->
  (int * Ocsigen_base.Lib.poly * Lib.pos * string option) list ->
  unit
```
Called at the end of every client or shared section. The first argument identifies the compilation unit. The second is the list of novel injections in that section.

Adds a list of `Lib_base.injection_datum`s into the queue of client section data of the compilation unit (`Lib_base.compilation_unit_global_data`).

Called in parallel with `Client.Syntax_helpers.open_client_section`.

```ocaml
val escaped_value : 'a -> Runtime.escaped_value
```
Convert any value to a [`Runtime.escaped_value`](./Eliom-Runtime.md#type-escaped_value) for usage in the `args` argument to [`Syntax.client_value`](./#val-client_value).
