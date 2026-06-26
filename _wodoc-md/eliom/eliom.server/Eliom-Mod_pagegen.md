
# Module `Eliom.Mod_pagegen`

```ocaml
val def_handler : exn -> 'b Lwt.t
```
```ocaml
val execute : 
  float ->
  (float -> Common.info -> Common.sitedata -> Ocsigen.Response.t Lwt.t) ->
  Common.info ->
  Common.sitedata ->
  Ocsigen.Response.t Lwt.t
```
```ocaml
val gen : 
  Extension.eliom_extension_sig option ->
  Common.sitedata ->
  Ocsigen.Extensions.request_state ->
  Ocsigen.Extensions.answer Lwt.t
```
```ocaml
val update_cookie_table : 
  ?now:float ->
  Common.sitedata ->
  Common.tables Common.cookie_info ->
  unit Lwt.t
```