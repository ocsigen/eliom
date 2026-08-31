# Module `Eliommod_datasess`

```ocaml
val close_data_state : 
  scope:[< Eliom_common.user_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  unit
```
```ocaml
val find_or_create_data_cookie : 
  ?set_session_group:string ->
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  Eliom_common.one_data_cookie_info
```
```ocaml
val find_data_cookie_only : 
  cookie_scope:[< Eliom_common.cookie_scope ] ->
  secure_o:bool option ->
  ?sp:Eliom_common.server_params ->
  unit ->
  Eliom_common.one_data_cookie_info
```
```ocaml
val counttableelements : (unit -> int) list ref
```
```ocaml
val create_volatile_table : 
  scope:([< Eliom_common.user_scope ] as 'b) ->
  secure:bool ->
  'b * bool * 'a Eliom_common.SessionCookies.t
```
```ocaml
val create_volatile_table_during_session : 
  scope:([< Eliom_common.user_scope ] as 'b) ->
  secure:bool ->
  Eliom_common.sitedata ->
  'b * bool * 'a Eliom_common.SessionCookies.t
```
