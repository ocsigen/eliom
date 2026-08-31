# Module `Eliom_registration.Any`

```ocaml
type 'a page = 'a kind
```
```ocaml
type options = unit
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
type 'a result = 'a kind
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
