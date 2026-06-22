
# Module `Eliom_registration`

Client-side service registration.

The interface is meant to be compatible with server-side [`Eliom_registration`](#).

See [the manual chapter on client-side services](./../clientserver-services.md) for details.

```ocaml
type 'a kind
```
```ocaml
type browser_content = [ 
  | `Browser
 ]
```
```ocaml
type 'a application_content = [ 
  | `Appl of 'a
 ]
```
```ocaml
module Html : 
  Eliom_registration_sigs.S
    with type page = Html_types.html Eliom_content.Html.elt
     and type options = unit
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
```ocaml
module Action : 
  Eliom_registration_sigs.S
    with type page = unit
     and type options = [ `Reload | `NoReload ]
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
```ocaml
module Unit : 
  Eliom_registration_sigs.S
    with type page = unit
     and type options = unit
     and type return = Eliom_service.non_ocaml
     and type result = browser_content kind
```
```ocaml
type appl_service_options = {
  do_not_launch : bool;
}
```
Has no effect on client; for compatibility with server

```ocaml
module App (_ : Eliom_registration_sigs.APP_PARAM) : sig ... end
```
```ocaml
type _ redirection = 
  | Redirection : (unit,
                  unit,
                  Eliom_service.get,
                  _,
                  _,
                  _,
                  _,
                  [ `WithoutSuffix ],
                  unit,
                  unit,
                  'a)
                  Eliom_service.t -> 'a redirection
```
```ocaml
module Redirection : 
  Eliom_registration_sigs.S_poly_with_send
    with type 'a page = Eliom_service.non_ocaml redirection
     and type options =
           [ `MovedPermanently
           | `Found
           | `SeeOther
           | `NotNodifed
           | `UseProxy
           | `TemporaryRedirect ]
     and type 'a return = Eliom_service.non_ocaml
     and type 'a result = browser_content kind
```
```ocaml
module Any : 
  Eliom_registration_sigs.S_poly_with_send
    with type 'a page = 'a kind
     and type options = unit
     and type 'a return = Eliom_service.non_ocaml
     and type 'a result = 'a kind
```
```ocaml
val appl_self_redirect : 
  ('page -> [< 'a application_content | browser_content ] kind Lwt.t) ->
  'page ->
  'appl application_content kind Lwt.t
```
For compatibility with server-side `appl_self_redirect`
