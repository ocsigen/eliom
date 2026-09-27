# Module `Eliom.Config`

```ocaml
val get_default_hostname : unit -> string
```
```ocaml
val get_default_port : unit -> int
```
```ocaml
val get_default_sslport : unit -> int
```
```ocaml
val default_protocol_is_https : unit -> bool
```
```ocaml
val get_default_links_xhr : unit -> bool
```
```ocaml
val debug_timings : bool ref
```
```ocaml
val debug_time : string -> unit
```
`debug_time name` starts the browser timer `name` if `debug_timings` is set.

```ocaml
val debug_time_end : string -> unit
```
`debug_time_end name` stops the browser timer `name` if `debug_timings` is set.

```ocaml
val set_tracing : bool -> unit
```
Not tracing by default. Can be dynamically set by adding `"#__trace"` to the URL.

```ocaml
val get_tracing : unit -> bool
```
```ocaml
val get_debugmode : unit -> bool
```
Same as `Ocsigen.Config.get_debugmode`. On client side, returns `false` for now.
