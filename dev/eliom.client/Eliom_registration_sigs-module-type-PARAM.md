# Module type `Eliom_registration_sigs.PARAM`

```ocaml
type page
```
```ocaml
type options
```
```ocaml
type result
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
  ?headers:Eliom_service.Cohttp.Header.t ->
  page ->
  frame Lwt.t
```
```ocaml
val send_appl_content : Eliom_service.send_appl_content
```
Whether the service is capable of sending application content when required. This field is usually `Eliom_service.XNever`. This value is recorded inside each service just after registration.

```ocaml
val result_of_http_result : frame -> result
```
