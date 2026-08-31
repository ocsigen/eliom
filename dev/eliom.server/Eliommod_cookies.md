# Module `Eliommod_cookies`

```ocaml
val cookieset_to_json : Ocsigen_cookie_map.t -> string
```
```ocaml
val make_new_session_id : unit -> string
```
```ocaml
val get_cookie_info : 
  float ->
  Eliom_common.sitedata ->
  string Eliom_common.Full_state_name_table.t ->
  string Eliom_common.Full_state_name_table.t ->
  string Eliom_common.Full_state_name_table.t ->
  (string Eliom_common.Full_state_name_table.t
   * string Eliom_common.Full_state_name_table.t
   * string Eliom_common.Full_state_name_table.t) ->
  Eliom_common.tables Eliom_common.cookie_info
  * Eliom_common.Full_state_name_table.key list
```
```ocaml
val new_service_cookie_table : 
  unit ->
  Eliom_common.tables Eliom_common.Service_cookie.t
    Eliom_common.SessionCookies.t
```
```ocaml
val new_data_cookie_table : 
  unit ->
  Eliom_common.Data_cookie.t Eliom_common.SessionCookies.t
```
```ocaml
val compute_session_cookies_to_send : 
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookie_map.t ->
  Ocsigen_cookie_map.t Lwt.t
```
```ocaml
val compute_cookies_to_send : 
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookie_map.t ->
  Ocsigen_cookie_map.t Lwt.t
```
```ocaml
val compute_new_ri_cookies : 
  float ->
  string list ->
  string Ocsigen_cookie_map.Map_inner.t ->
  Eliom_common.tables Eliom_common.cookie_info ->
  Ocsigen_cookie_map.t ->
  string Ocsigen_cookie_map.Map_inner.t Lwt.t
```
```ocaml
type date = float
```
```ocaml
type cookie = {
  full_state_name : Eliom_common.full_state_name;
  expiry : date option;
  timeout : Eliom_common.timeout;
  session_group : Eliom_common.perssessgrp option;
}
```
```ocaml
module Persistent_cookies : sig ... end
```
