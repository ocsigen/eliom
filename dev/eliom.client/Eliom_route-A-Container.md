
# Module `A.Container`

```ocaml
type t = {
  mutable t_services : (int * int * Table.t Eliom_common.dircontent ref) list;
  mutable t_contains_timeout : bool;
  mutable t_na_services : (Eliom_common.na_key_serv,
                          bool ->
                          params ->
                          result Lwt.t)
                          Hashtbl.t;
}
```
```ocaml
val get : t -> (int * int * Table.t Eliom_common.dircontent ref) list
```
```ocaml
val set_contains_timeout : t -> bool -> unit
```
```ocaml
val set : t -> (int * int * Table.t Eliom_common.dircontent ref) list -> unit
```
```ocaml
val dlist_add : ?sp:'a -> 'b -> 'c -> unit
```