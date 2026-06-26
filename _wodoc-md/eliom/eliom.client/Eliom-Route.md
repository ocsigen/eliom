
# Module `Eliom.Route`

```ocaml
type info = {
  i_sess_info : Common.sess_info;
  i_subpath : string list;
  i_meth : Common.meth;
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
  Common.full_state_name option ->
  A.site_data ->
  A.info ->
  string list option ->
  Common.page_table_key ->
  A.result Lwt.t
```
```ocaml
val remove_id : 
  ('a, 'b) Common.service list ->
  (Common.anon_params_type * Common.anon_params_type) ->
  ('a, 'b) Common.service list
```
```ocaml
val find_and_remove_id : 
  ('a, 'b) Common.service list ->
  (Common.anon_params_type * Common.anon_params_type) ->
  ('a, 'b) Common.service * ('a, 'b) Common.service list
```
```ocaml
val add_page_table : 
  A.Container.t ->
  string list ->
  A.Table.t ref ->
  Common.page_table_key ->
  (A.params, A.result) Common.service ->
  unit
```
```ocaml
val remove_page_table : 
  'a ->
  'b ->
  A.Table.t ref ->
  Common.page_table_key ->
  (Common.anon_params_type * Common.anon_params_type) ->
  unit
```
```ocaml
val add_dircontent : 
  A.Table.t Common.dircontent ->
  (Lib.String.Table.key * A.Table.t Common.direlt ref) ->
  A.Table.t Common.dircontent
```
```ocaml
val find_dircontent : 
  'a Common.dircontent ->
  Lib.String.Table.key ->
  'a Common.direlt ref
```
```ocaml
val add_or_remove_service : 
  ('a -> Lib.String.Table.key list -> A.Table.t ref -> 'b -> 'c -> 'd) ->
  'a ->
  A.Table.t Common.dircontent ref ->
  Lib.String.Table.key list ->
  'b ->
  'c ->
  'd
```
```ocaml
val add_service : 
  int ->
  A.Container.t ->
  Lib.String.Table.key list ->
  Common.page_table_key ->
  (A.params, A.result) Common.service ->
  unit
```
```ocaml
val remove_service : 
  A.Container.t ->
  Lib.String.Table.key list ->
  Common.page_table_key ->
  (Common.anon_params_type * Common.anon_params_type) ->
  unit
```
```ocaml
exception Exn1
```
```ocaml
val find_service : 
  float ->
  A.Container.t ->
  Common.full_state_name option ->
  A.site_data ->
  A.info ->
  A.result Lwt.t
```
```ocaml
val global_tables : A.Container.t
```
```ocaml
val add_naservice : 
  Common.na_key_serv ->
  (bool -> A.params -> A.result Lwt.t) ->
  A.Container.t ->
  unit
```
```ocaml
val call_naservice : Common.na_key_serv -> A.Container.t -> A.result Lwt.t
```
```ocaml
val na_key_of_params : 
  get:bool ->
  (string * string) list ->
  Common.na_key_serv option
```
```ocaml
val remove_site_dir : 'a list -> 'a list -> 'a list option
```
```ocaml
val call_service : info -> A.result Lwt.t
```