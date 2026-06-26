
# Module `Route_base.Make`


## Parameters

```ocaml
module P : PARAM
```

## Signature

```ocaml
val find_page_table : 
  bool ->
  float ->
  P.Table.t ref ->
  Common.full_state_name option ->
  P.site_data ->
  P.info ->
  string list option ->
  Common.page_table_key ->
  P.result Lwt.t
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
  P.Container.t ->
  string list ->
  P.Table.t ref ->
  Common.page_table_key ->
  (P.params, P.result) Common.service ->
  unit
```
```ocaml
val remove_page_table : 
  'a ->
  'b ->
  P.Table.t ref ->
  Common.page_table_key ->
  (Common.anon_params_type * Common.anon_params_type) ->
  unit
```
```ocaml
val add_dircontent : 
  P.Table.t Common.dircontent ->
  (Lib.String.Table.key * P.Table.t Common.direlt ref) ->
  P.Table.t Common.dircontent
```
```ocaml
val find_dircontent : 
  'a Common.dircontent ->
  Lib.String.Table.key ->
  'a Common.direlt ref
```
```ocaml
val add_or_remove_service : 
  ('a -> Lib.String.Table.key list -> P.Table.t ref -> 'b -> 'c -> 'd) ->
  'a ->
  P.Table.t Common.dircontent ref ->
  Lib.String.Table.key list ->
  'b ->
  'c ->
  'd
```
```ocaml
val add_service : 
  int ->
  P.Container.t ->
  Lib.String.Table.key list ->
  Common.page_table_key ->
  (P.params, P.result) Common.service ->
  unit
```
```ocaml
val remove_service : 
  P.Container.t ->
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
  P.Container.t ->
  Common.full_state_name option ->
  P.site_data ->
  P.info ->
  P.result Lwt.t
```