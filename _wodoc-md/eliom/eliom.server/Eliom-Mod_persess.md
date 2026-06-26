
# Module `Eliom.Mod_persess`

```ocaml
val close_persistent_state2 : 
  scope:Common.user_scope ->
  Common.sitedata ->
  Common.perssessgrp option ->
  string ->
  unit Lwt.t
```
```ocaml
val close_persistent_state : 
  scope:[< Common.user_scope ] ->
  secure_o:bool option ->
  ?sp:Common.server_params ->
  unit ->
  unit Lwt.t
```
```ocaml
val find_or_create_persistent_cookie : 
  ?set_session_group:string ->
  cookie_scope:[< Common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Common.server_params ->
  unit ->
  Common.one_persistent_cookie_info Lwt.t
```
```ocaml
val find_persistent_cookie_only : 
  cookie_scope:[< Common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Common.server_params ->
  unit ->
  Common.one_persistent_cookie_info Lwt.t
```