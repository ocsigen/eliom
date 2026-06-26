
# Module type `Route_base.PARAM`

```ocaml
type site_data
```
```ocaml
type info
```
```ocaml
type params
```
```ocaml
type result
```
```ocaml
val sess_info_of_info : info -> Common.sess_info
```
```ocaml
val meth_of_info : info -> Common.meth
```
```ocaml
val subpath_of_info : info -> string list
```
```ocaml
val make_params : 
  site_data ->
  info ->
  string list option ->
  Common.full_state_name option ->
  params
```
```ocaml
val handle_directory : info -> result Lwt.t
```
```ocaml
val get_number_of_reloads : unit -> int
```
```ocaml
module Node : sig ... end
```
```ocaml
module Table : sig ... end
```
```ocaml
module Container : sig ... end
```