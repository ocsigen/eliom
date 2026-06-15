
# Module `Eliom_route_base.Make`


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
  Eliom_common.full_state_name option ->
  P.site_data ->
  P.info ->
  string list option ->
  Eliom_common.page_table_key ->
  P.result Lwt.t
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
  P.Container.t ->
  string list ->
  P.Table.t ref ->
  Eliom_common.page_table_key ->
  (P.params, P.result) Eliom_common.service ->
  unit
```
```ocaml
val remove_page_table : 
  'a ->
  'b ->
  P.Table.t ref ->
  Eliom_common.page_table_key ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) ->
  unit
```
```ocaml
val add_dircontent : 
  P.Table.t Eliom_common.dircontent ->
  (Eliom_lib.String.Table.key * P.Table.t Eliom_common.direlt ref) ->
  P.Table.t Eliom_common.dircontent
```
```ocaml
val find_dircontent : 
  'a Eliom_common.dircontent ->
  Eliom_lib.String.Table.key ->
  'a Eliom_common.direlt ref
```
```ocaml
val add_or_remove_service : 
  ('a -> Eliom_lib.String.Table.key list -> P.Table.t ref -> 'b -> 'c -> 'd) ->
  'a ->
  P.Table.t Eliom_common.dircontent ref ->
  Eliom_lib.String.Table.key list ->
  'b ->
  'c ->
  'd
```
```ocaml
val add_service : 
  int ->
  P.Container.t ->
  Eliom_lib.String.Table.key list ->
  Eliom_common.page_table_key ->
  (P.params, P.result) Eliom_common.service ->
  unit
```
```ocaml
val remove_service : 
  P.Container.t ->
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
  P.Container.t ->
  Eliom_common.full_state_name option ->
  P.site_data ->
  P.info ->
  P.result Lwt.t
```