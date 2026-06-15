
# Module `Eliom_route`

```ocaml
val get_page : 
  float ->
  Eliom_common.info ->
  Eliom_common.sitedata ->
  Ocsigen_response.t Lwt.t
```
```ocaml
val add_service : 
  int ->
  Eliom_common.tables ->
  Eliom_lib.String.Table.key list ->
  Eliom_common.page_table_key ->
  (Eliom_common.server_params, Ocsigen_response.t) Eliom_common.service ->
  unit
```
```ocaml
val remove_service : 
  Eliom_common.tables ->
  Eliom_lib.String.Table.key list ->
  Eliom_common.page_table_key ->
  (Eliom_common.anon_params_type * Eliom_common.anon_params_type) ->
  unit
```
```ocaml
val add_naservice : 
  Eliom_common.tables ->
  Eliom_common.na_key_serv ->
  (int ref option
   * (float * float ref) option
   * (Eliom_common.server_params ->
   Ocsigen_response.t Lwt.t)) ->
  unit
```
```ocaml
val remove_naservice : Eliom_common.tables -> Eliom_common.na_key_serv -> unit
```
```ocaml
val make_naservice : 
  float ->
  Eliom_common.info ->
  Eliom_common.sitedata ->
  Ocsigen_response.t Lwt.t
```