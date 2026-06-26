
# Module `Content_functor.Html`

```ocaml
module Ev' (A : sig ... end) : sig ... end
```
```ocaml
module F : sig ... end
```
```ocaml
module R : sig ... end
```
```ocaml
module D : sig ... end
```
```ocaml
module C : sig ... end
```
```ocaml
type +'a elt = 'a F.elt
```
```ocaml
type +'a attrib = 'a F.attrib
```
```ocaml
type uri = F.uri
```
```ocaml
type 'a form_param = 'a Eliom_form.param
```
```ocaml
module type T = sig ... end
```
```ocaml
module Custom_data = Content_core.Html.Custom_data
```
```ocaml
module Of_dom = Content_core.Html.Of_dom
```
```ocaml
module To_dom : sig ... end
```
```ocaml
module Id : sig ... end
```
```ocaml
module Manip : sig ... end
```