
# Module `Eliommod_sessadmin`

```ocaml
val close_all_service_states : 
  scope:Eliom_common.user_scope ->
  secure:bool ->
  Eliom_common.sitedata ->
  unit Lwt.t
```
```ocaml
val close_all_data_states : 
  scope:Eliom_common.user_scope ->
  secure:bool ->
  Eliom_common.sitedata ->
  unit Lwt.t
```
```ocaml
val close_all_persistent_states : 
  scope:Eliom_common.user_scope ->
  secure:bool ->
  Eliom_common.sitedata ->
  unit Lwt.t
```
```ocaml
val update_serv_exp : 
  Eliom_common.full_state_name ->
  Eliom_common.sitedata ->
  float option ->
  float option ->
  unit Lwt.t
```
```ocaml
val update_data_exp : 
  Eliom_common.full_state_name ->
  Eliom_common.sitedata ->
  float option ->
  float option ->
  unit Lwt.t
```
```ocaml
val update_pers_exp : 
  Eliom_common.full_state_name ->
  Eliom_common.sitedata ->
  float option ->
  float option ->
  unit Lwt.t
```
```ocaml
val section : Logs.src
```