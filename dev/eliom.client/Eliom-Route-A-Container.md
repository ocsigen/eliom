# Module `A.Container`

```ocaml
type t = {
  mutable t_services : Table.t Common.service_table list;
  mutable t_contains_timeout : bool;
  mutable t_na_services : (Common.na_key_serv, bool -> params -> result Lwt.t)
                          Hashtbl.t;
}
```
```ocaml
val get : t -> Table.t Common.service_table list
```
```ocaml
val set_contains_timeout : t -> bool -> unit
```
```ocaml
val set : t -> Table.t Common.service_table list -> unit
```
```ocaml
val dlist_add : ?sp:'a -> 'b -> 'c -> unit
```
