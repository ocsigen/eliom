# Module `Eliommod_sersess`

```ocaml
val close_service_state : 
  scope:[< Eliom_common.user_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  unit
```
```ocaml
val find_or_create_service_cookie : 
  ?set_session_group:string ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  Eliom_common.tables Eliom_common.one_service_cookie_info
```
```ocaml
val find_service_cookie_only : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  Eliom_common.tables Eliom_common.one_service_cookie_info
```
