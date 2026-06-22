
# Module `Eliom_mkreg`

This module defines the functor to use to creates modules generating functions to register services for your own types of pages. It is used for example in [`Eliom_registration`](./Eliom_registration.md).

```ocaml
module Make
  (Pages : Eliom_registration_sigs.PARAM with type frame := Ocsigen_response.t) : 
  Eliom_registration_sigs.S_with_create
    with type page = Pages.page
     and type options = Pages.options
     and type result = Pages.result
```
```ocaml
module Make_poly
  (Pages : 
    Eliom_registration_sigs.PARAM_POLY with type frame := Ocsigen_response.t) : 
  Eliom_registration_sigs.S_poly_with_create
    with type 'a page = 'a Pages.page
     and type options = Pages.options
     and type 'a return = 'a Pages.return
```