
# Module `Eliom_registration.Redirection`

```ocaml
type 'a page = Eliom_service.non_ocaml redirection
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
type 'a return = Eliom_service.non_ocaml
```
```ocaml
val register : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Eliom_service.Cohttp.Header.t ->
  ?secure_session:bool ->
  service:
    ('get,
      'post,
      _,
      _,
      _,
      Eliom_service.non_ext,
      Eliom_service.reg,
      _,
      _,
      _,
      'a return)
      Eliom_service.t ->
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
  ?headers:Eliom_service.Cohttp.Header.t ->
  'a page ->
  'a result Lwt.t
```