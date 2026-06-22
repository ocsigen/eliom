
# Module `Eliom_route.A`

```ocaml
type site_data = unit
```
```ocaml
type info' = info
```
```ocaml
type info = info'
```
```ocaml
type params = string list option
```
```ocaml
type result = Eliom_service.result
```
```ocaml
val site_data : 'a -> unit
```
```ocaml
val sess_info_of_info : info -> Eliom_common.sess_info
```
```ocaml
val subpath_of_info : info -> string list
```
```ocaml
val meth_of_info : info -> Eliom_common.meth
```
```ocaml
val make_params : 'a -> 'b -> 'c -> 'd -> 'c
```
```ocaml
val get_number_of_reloads : unit -> int
```
```ocaml
module Raw_table : sig ... end
```
```ocaml
type table_content = [ 
  | `Ptc of unit option * (params, result) Eliom_common.service list
 ]
```
```ocaml
type service =
  (table ref * Eliom_common.page_table_key, Eliom_common.na_key_serv)
    Eliom_lib.leftright
```
```ocaml
and node = service list
```
```ocaml
and table = table_content Raw_table.t
```
```ocaml
module Table : sig ... end
```
```ocaml
module Node : sig ... end
```
```ocaml
module Container : sig ... end
```
```ocaml
val handle_directory : 'a -> Eliom_service.result Lwt.t
```