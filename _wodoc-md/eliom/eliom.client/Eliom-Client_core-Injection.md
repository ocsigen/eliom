
# Module `Client_core.Injection`

```ocaml
val get : ?ident:string -> ?pos:Lib.pos -> name:string -> _
```
```ocaml
val initialize : 
  compilation_unit_id:string ->
  Client_value.injection_datum ->
  unit
```