
# Module `Eliom.Mod_cookies`

```ocaml
val cookieset_to_json : Ocsigen_cookie_map.t -> string
```
```ocaml
val make_new_session_id : unit -> string
```
```ocaml
val get_cookie_info : 
  float ->
  Common.sitedata ->
  string Common.Full_state_name_table.t ->
  string Common.Full_state_name_table.t ->
  string Common.Full_state_name_table.t ->
  (string Common.Full_state_name_table.t
   * string Common.Full_state_name_table.t
   * string Common.Full_state_name_table.t) ->
  Common.tables Common.cookie_info * Common.Full_state_name_table.key list
```
```ocaml
val new_service_cookie_table : 
  unit ->
  Common.tables Common.Service_cookie.t Common.SessionCookies.t
```
```ocaml
val new_data_cookie_table : 
  unit ->
  Common.Data_cookie.t Common.SessionCookies.t
```
```ocaml
val compute_session_cookies_to_send : 
  Common.sitedata ->
  Common.tables Common.cookie_info ->
  Ocsigen_cookie_map.t ->
  Ocsigen_cookie_map.t Lwt.t
```
```ocaml
val compute_cookies_to_send : 
  Common.sitedata ->
  Common.tables Common.cookie_info ->
  Ocsigen_cookie_map.t ->
  Ocsigen_cookie_map.t Lwt.t
```
```ocaml
val compute_new_ri_cookies : 
  float ->
  string list ->
  string Ocsigen_cookie_map.Map_inner.t ->
  Common.tables Common.cookie_info ->
  Ocsigen_cookie_map.t ->
  string Ocsigen_cookie_map.Map_inner.t Lwt.t
```
```ocaml
type date = float
```
```ocaml
type cookie = {
  full_state_name : Common.full_state_name;
  expiry : date option;
  timeout : Common.timeout;
  session_group : Common.perssessgrp option;
}
```
```ocaml
module Persistent_cookies : sig ... end
```