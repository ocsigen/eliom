# Module type `Eliom_registration_sigs.S_poly_with_send`

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
val register : 
  ?app:string ->
  ?scope:[< Eliom_common.scope ] ->
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
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
See [`S.register`](./Eliom_registration_sigs-module-type-S.md#val-register).

```ocaml
type 'a result
```
```ocaml
val send : 
  ?options:options ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Cohttp.Header.t ->
  'a page ->
  'a result Lwt.t
```
