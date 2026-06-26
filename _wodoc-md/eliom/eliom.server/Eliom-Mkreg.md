
# Module `Eliom.Mkreg`

This module defines the functor to use to creates modules generating functions to register services for your own types of pages. It is used for example in [`Registration`](./Eliom-Registration.md).

```ocaml
module Make
  (Pages : Registration_sigs.PARAM with type frame := Ocsigen.Response.t) : 
  Registration_sigs.S_with_create
    with type page = Pages.page
     and type options = Pages.options
     and type result = Pages.result
```
```ocaml
module Make_poly
  (Pages : Registration_sigs.PARAM_POLY with type frame := Ocsigen.Response.t) : 
  Registration_sigs.S_poly_with_create
    with type 'a page = 'a Pages.page
     and type options = Pages.options
     and type 'a return = 'a Pages.return
```