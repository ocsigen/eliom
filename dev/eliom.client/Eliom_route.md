# Module `Eliom_route`

```ocaml
type info = {
  i_sess_info : Eliom_common.sess_info;
  i_subpath : string list;
  i_meth : Eliom_common.meth;
  i_get_params : (string * string) list;
  i_post_params : (string * string) list;
}
```
```ocaml
module A : sig ... end
```
```ocaml
val find_page_table : 
  bool ->
  float ->
  A.Table.t ref ->
  Eliom_common.full_state_name option ->
  A.site_data ->
  A.info ->
  string list option ->
  Eliom_common.page_table_key ->
  A.result Lwt.t
```
```ocaml
val remove_id : 
  ('a, 'b) Eliom_common.service list ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) ->
  ('a, 'b) Eliom_common.service list
```
```ocaml
val find_and_remove_id : 
  ('a, 'b) Eliom_common.service list ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) ->
  ('a, 'b) Eliom_common.service * ('a, 'b) Eliom_common.service list
```
```ocaml
val add_page_table : 
  A.Container.t ->
  string list ->
  A.Table.t ref ->
  Eliom_common.page_table_key ->
  (A.params, A.result) Eliom_common.service ->
  unit
```
```ocaml
val remove_page_table : 
  'a ->
  'b ->
  A.Table.t ref ->
  Eliom_common.page_table_key ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) ->
  unit
```
```ocaml
val add_dircontent : 
  A.Table.t Eliom_common.dircontent ->
  (Eliom_lib.String.Table.key * A.Table.t Eliom_common.direlt ref) ->
  A.Table.t Eliom_common.dircontent
```
```ocaml
val find_dircontent : 
  'a Eliom_common.dircontent ->
  Eliom_lib.String.Table.key ->
  'a Eliom_common.direlt ref
```
```ocaml
val add_or_remove_service : 
  ('a -> Eliom_lib.String.Table.key list -> A.Table.t ref -> 'b -> 'c -> 'd) ->
  'a ->
  A.Table.t Eliom_common.dircontent ref ->
  Eliom_lib.String.Table.key list ->
  'b ->
  'c ->
  'd
```
```ocaml
val add_service : 
  int ->
  A.Container.t ->
  Eliom_lib.String.Table.key list ->
  Eliom_common.page_table_key ->
  (A.params, A.result) Eliom_common.service ->
  unit
```
```ocaml
val remove_service : 
  A.Container.t ->
  Eliom_lib.String.Table.key list ->
  Eliom_common.page_table_key ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) ->
  unit
```
```ocaml
exception Exn1
```
```ocaml
val find_service : 
  float ->
  A.Container.t ->
  Eliom_common.full_state_name option ->
  A.site_data ->
  A.info ->
  A.result Lwt.t
```
```ocaml
val global_tables : A.Container.t
```
```ocaml
val add_naservice : 
  Eliom_common.na_key_serv ->
  (bool -> A.params -> A.result Lwt.t) ->
  A.Container.t ->
  unit
```
```ocaml
val call_naservice : 
  Eliom_common.na_key_serv ->
  A.Container.t ->
  A.result Lwt.t
```
```ocaml
val na_key_of_params : 
  get:bool ->
  (string * string) list ->
  Eliom_common.na_key_serv option
```
```ocaml
val remove_site_dir : 'a list -> 'a list -> 'a list option
```
```ocaml
val call_service : info -> A.result Lwt.t
```
