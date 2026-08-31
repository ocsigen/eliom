# Module `Eliommod_pagegen`

```ocaml
val def_handler : exn -> 'b Lwt.t
```
```ocaml
val execute : 
  float ->
  (float ->
    Eliom_common.info ->
    Eliom_common.sitedata ->
    Ocsigen_response.t Lwt.t) ->
  Eliom_common.info ->
  Eliom_common.sitedata ->
  Ocsigen_response.t Lwt.t
```
```ocaml
val gen : 
  Eliom_extension.eliom_extension_sig option ->
  Eliom_common.sitedata ->
  Ocsigen_extensions.request_state ->
  Ocsigen_extensions.answer Lwt.t
```
```ocaml
val update_cookie_table : 
  ?now:float ->
  Eliom_common.sitedata ->
  Eliom_common.tables Eliom_common.cookie_info ->
  unit Lwt.t
```
