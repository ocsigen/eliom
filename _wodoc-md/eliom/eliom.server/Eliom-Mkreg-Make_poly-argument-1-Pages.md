
# Parameter `Make_poly.Pages`

```ocaml
type _ page
```
```ocaml
type options
```
```ocaml
type _ return
```
```ocaml
val send : 
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  _ page ->
  Ocsigen.Response.t Lwt.t
```
```ocaml
val send_appl_content : Service.send_appl_content
```
See [`Registration_sigs.PARAM.send_appl_content`](./Eliom-Registration_sigs-module-type-PARAM.md#val-send_appl_content).
