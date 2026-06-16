
# Module `Eliommod_gc`

```ocaml
val servicesessiongcfrequency : float option ref
```
```ocaml
val datasessiongcfrequency : float option ref
```
```ocaml
val persistentsessiongcfrequency : float option ref
```
```ocaml
val set_servicesessiongcfrequency : float option -> unit
```
```ocaml
val set_datasessiongcfrequency : float option -> unit
```
```ocaml
val get_servicesessiongcfrequency : unit -> float option
```
```ocaml
val get_datasessiongcfrequency : unit -> float option
```
```ocaml
val set_persistentsessiongcfrequency : float option -> unit
```
```ocaml
val get_persistentsessiongcfrequency : unit -> float option
```
```ocaml
val service_session_gc : Eliom_common.sitedata -> unit
```
```ocaml
val data_session_gc : Eliom_common.sitedata -> unit
```
```ocaml
val persistent_session_gc : Eliom_common.sitedata -> unit
```
```ocaml
val section : Logs.src
```