# Module `P.Container`

```ocaml
type t
```
```ocaml
val set_contains_timeout : t -> bool -> unit
```
```ocaml
val dlist_add : 
  ?sp:Eliom_common.server_params ->
  t ->
  (Table.t ref * Eliom_common.page_table_key, Eliom_common.na_key_serv)
    Eliom_lib.leftright ->
  Node.t
```
```ocaml
val get : t -> (int * int * Table.t Eliom_common.dircontent ref) list
```
```ocaml
val set : t -> (int * int * Table.t Eliom_common.dircontent ref) list -> unit
```
