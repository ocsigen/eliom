
# Module `Route.A`

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
type result = Service.result
```
```ocaml
val site_data : 'a -> unit
```
```ocaml
val sess_info_of_info : info -> Common.sess_info
```
```ocaml
val subpath_of_info : info -> string list
```
```ocaml
val meth_of_info : info -> Common.meth
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
  | `Ptc of unit option * (params, result) Common.service list
 ]
```
```ocaml
type service =
  (table ref * Common.page_table_key, Common.na_key_serv) Lib.leftright
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
val handle_directory : 'a -> Service.result Lwt.t
```