# Module `Eliommod_persess`

```ocaml
val close_persistent_state2 : 
  scope:Eliom_common.user_scope ->
  Eliom_common.sitedata ->
  Eliom_common.perssessgrp option ->
  string ->
  unit Lwt.t
```
```ocaml
val close_persistent_state : 
  scope:[< Eliom_common.user_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  unit Lwt.t
```
```ocaml
val find_or_create_persistent_cookie : 
  ?set_session_group:string ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  Eliom_common.one_persistent_cookie_info Lwt.t
```
```ocaml
val find_persistent_cookie_only : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  Eliom_common.one_persistent_cookie_info Lwt.t
```
