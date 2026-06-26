
# Module type `Registration_sigs.PARAM_POLY`

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
val send_appl_content : Service.send_appl_content
```
See [`Registration_sigs.PARAM.send_appl_content`](./Eliom-Registration_sigs-module-type-PARAM.md#val-send_appl_content).
