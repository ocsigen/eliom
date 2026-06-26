
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
