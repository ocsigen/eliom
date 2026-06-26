
# Module `Eliom.Mod_sersess`

```ocaml
val close_service_state : 
  scope:[< Common.user_scope ] ->
  secure_o:bool option ->
  ?sp:Common.server_params ->
  unit ->
  unit
```
```ocaml
val find_or_create_service_cookie : 
  ?set_session_group:string ->
  cookie_scope:[< Common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Common.server_params ->
  unit ->
  Common.tables Common.one_service_cookie_info
```
```ocaml
val find_service_cookie_only : 
  cookie_scope:[< Common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Common.server_params ->
  unit ->
  Common.tables Common.one_service_cookie_info
```