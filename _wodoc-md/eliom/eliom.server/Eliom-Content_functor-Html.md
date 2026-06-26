
# Module `Content_functor.Html`

```ocaml
module Ev' (A : sig ... end) : sig ... end
```
```ocaml
module F : sig ... end
```
```ocaml
module D : sig ... end
```
```ocaml
module R = Shared_content.Html.R
```
```ocaml
module Custom_data = Content_core.Html.Custom_data
```
```ocaml
module Id = Content_core.Html.Id
```
```ocaml
module Printer = Content_core.Html.Printer
```
```ocaml
type +'a elt = 'a F.elt
```
```ocaml
type 'a wrap = 'a F.wrap
```
```ocaml
type 'a list_wrap = 'a F.list_wrap
```
```ocaml
type 'a attrib = 'a F.attrib
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