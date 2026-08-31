# Module `PARAM.Table`

```ocaml
type t
```
```ocaml
val empty : unit -> t
```
```ocaml
val add : 
  Eliom_common.page_table_key ->
  (Node.t option * (params, result) Eliom_common.service list) ->
  t ->
  t
```
```ocaml
val find : 
  Eliom_common.page_table_key ->
  t ->
  Node.t option * (params, result) Eliom_common.service list
```
```ocaml
val remove : Eliom_common.page_table_key -> t -> t
```
