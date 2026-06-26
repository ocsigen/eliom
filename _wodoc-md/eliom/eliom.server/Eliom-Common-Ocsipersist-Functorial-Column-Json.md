
# Module `Column.Json`


## Parameters

```ocaml
module C : sig ... end
```

## Signature

```ocaml
type t = C.t
```
```ocaml
val column_type : string
```
```ocaml
val encode : t -> internal
```
```ocaml
val decode : internal -> t
```