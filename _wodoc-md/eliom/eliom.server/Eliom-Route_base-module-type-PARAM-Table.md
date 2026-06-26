
# Module `PARAM.Table`

```ocaml
type t
```
```ocaml
val empty : unit -> t
```
```ocaml
val add : 
  Common.page_table_key ->
  (Node.t option * (params, result) Common.service list) ->
  t ->
  t
```
```ocaml
val find : 
  Common.page_table_key ->
  t ->
  Node.t option * (params, result) Common.service list
```
```ocaml
val remove : Common.page_table_key -> t -> t
```