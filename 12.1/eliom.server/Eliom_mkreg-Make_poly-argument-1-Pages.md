
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
  Ocsigen_response.t Lwt.t
```
```ocaml
val send_appl_content : Eliom_service.send_appl_content
```
See `Eliom_reg_sigs.PARAM.send_appl_content`.
