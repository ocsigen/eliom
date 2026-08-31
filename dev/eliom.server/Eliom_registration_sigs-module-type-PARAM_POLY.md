# Module type `Eliom_registration_sigs.PARAM_POLY`

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
type frame
```
```ocaml
val send : 
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  _ page ->
  frame Lwt.t
```
```ocaml
val send_appl_content : Eliom_service.send_appl_content
```
See `Eliom_reg_sigs.PARAM.send_appl_content`.
