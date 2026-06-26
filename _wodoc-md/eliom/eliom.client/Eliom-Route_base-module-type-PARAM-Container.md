
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
  (Table.t ref * Common.page_table_key, Common.na_key_serv) Lib.leftright ->
  Node.t
```
```ocaml
val get : t -> (int * int * Table.t Common.dircontent ref) list
```
```ocaml
val set : t -> (int * int * Table.t Common.dircontent ref) list -> unit
```