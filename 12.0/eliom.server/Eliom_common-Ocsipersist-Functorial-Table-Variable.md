
# Module `Table.Variable`

```ocaml
type t
```
```ocaml
val make : name:key -> default:value -> t
```
```ocaml
val make_lazy : name:key -> default:(unit -> value) -> t
```
```ocaml
val make_lazy_lwt : name:key -> default:(unit -> value Lwt.t) -> t
```
```ocaml
val get : t -> value Lwt.t
```
```ocaml
val set : t -> value -> unit Lwt.t
```