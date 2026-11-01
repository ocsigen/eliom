# Module `PARAM.Container`

```ocaml
type t
```
```ocaml
val set_contains_timeout : t -> bool -> unit
```
```ocaml
val dlist_add : 
  ?sp:Common.server_params ->
  t ->
  (Table.t ref * Common.page_table_key, Common.na_key_serv) Either.t ->
  Node.t
```
```ocaml
val get : t -> Table.t Common.service_table list
```
```ocaml
val set : t -> Table.t Common.service_table list -> unit
```
