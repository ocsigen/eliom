
# Module `Eliom.Mod_sessadmin`

```ocaml
val close_all_service_states : 
  scope:Common.user_scope ->
  secure:bool ->
  Common.sitedata ->
  unit Lwt.t
```
```ocaml
val close_all_data_states : 
  scope:Common.user_scope ->
  secure:bool ->
  Common.sitedata ->
  unit Lwt.t
```
```ocaml
val close_all_persistent_states : 
  scope:Common.user_scope ->
  secure:bool ->
  Common.sitedata ->
  unit Lwt.t
```
```ocaml
val update_serv_exp : 
  Common.full_state_name ->
  Common.sitedata ->
  float option ->
  float option ->
  unit Lwt.t
```
```ocaml
val update_data_exp : 
  Common.full_state_name ->
  Common.sitedata ->
  float option ->
  float option ->
  unit Lwt.t
```
```ocaml
val update_pers_exp : 
  Common.full_state_name ->
  Common.sitedata ->
  float option ->
  float option ->
  unit Lwt.t
```
```ocaml
val section : Logs.src
```