
# Module `Eliom.Lib_base`

```ocaml
exception Eliom_Internal_Error of string
```
```ocaml
module type Map_S = sig ... end
```
```ocaml
module Int64_map : Map_S with type key = int64
```
```ocaml
module Int_map : Map_S with type key = int
```
```ocaml
module String_map : Map_S with type key = string
```