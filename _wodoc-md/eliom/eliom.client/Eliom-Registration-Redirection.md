
# Module `Registration.Redirection`

```ocaml
type 'a page = Service.non_ocaml redirection
```
```ocaml
type options = [ 
  | `MovedPermanently
  | `Found
  | `SeeOther
  | `NotNodifed
  | `UseProxy
  | `TemporaryRedirect
 ]
```
```ocaml
type 'a return = Service.non_ocaml
```
```ocaml
val register : 
  ?app:string ->
  ?scope:[< Common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Service.Cohttp.Header.t ->
  ?secure_session:bool ->
  service:
    ('get, 'post, _, _, _, Service.non_ext, Service.reg, _, _, _, 'a return)
      Service.t ->
  ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
  ('get -> 'post -> 'a page Lwt.t) ->
  unit
```
See `S.register`.

```ocaml
type 'a result = browser_content kind
```
```ocaml
val send : 
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Service.Cohttp.Header.t ->
  'a page ->
  'a result Lwt.t
```