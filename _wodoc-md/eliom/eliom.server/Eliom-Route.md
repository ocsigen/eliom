
# Module `Eliom.Route`

```ocaml
val get_page : 
  float ->
  Common.info ->
  Common.sitedata ->
  Ocsigen.Response.t Lwt.t
```
```ocaml
val add_service : 
  int ->
  Common.tables ->
  Lib.String.Table.key list ->
  Common.page_table_key ->
  (Common.server_params, Ocsigen.Response.t) Common.service ->
  unit
```
```ocaml
val remove_service : 
  Common.tables ->
  Lib.String.Table.key list ->
  Common.page_table_key ->
  (Common.anon_params_type * Common.anon_params_type) ->
  unit
```
```ocaml
val add_naservice : 
  Common.tables ->
  Common.na_key_serv ->
  (int ref option
   * (float * float ref) option
   * (Common.server_params ->
   Ocsigen.Response.t Lwt.t)) ->
  unit
```
```ocaml
val remove_naservice : Common.tables -> Common.na_key_serv -> unit
```
```ocaml
val make_naservice : 
  float ->
  Common.info ->
  Common.sitedata ->
  Ocsigen.Response.t Lwt.t
```