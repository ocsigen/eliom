
# Module type `Registration_sigs.PARAM`

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
  ?headers:Cohttp.Header.t ->
  page ->
  frame Lwt.t
```
```ocaml
val send_appl_content : Service.send_appl_content
```
Whether the service is capable of sending application content when required. This field is usually `Service.XNever`. This value is recorded inside each service just after registration.

```ocaml
val result_of_http_result : frame -> result
```