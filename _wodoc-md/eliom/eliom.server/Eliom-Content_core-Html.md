
# Module `Content_core.Html`

See the Eliom manual for more information on [dom semantics vs. functional semantics](./../clientserver-html.md#unique) for HTML5 tree manipulated by client/server application.

```ocaml
type 'a wrap = 'a
```
```ocaml
type 'a list_wrap = 'a list
```
```ocaml
type +'a elt
```
```ocaml
type +'a attrib
```
```ocaml
type uri = Xml.uri
```
```ocaml
module F : sig ... end
```
```ocaml
module D : sig ... end
```
```ocaml
module Make
  (Xml : Xml_sigs.T with type elt = Xml.elt and type attrib = Xml.attrib)
  (_ : Html_sigs.Wrapped_functions with module Xml = Xml)
  (Svg : Svg_sigs.T with module Xml := Xml) : 
  Html_sigs.Make(Xml)(Svg).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib
```
```ocaml
module Id : sig ... end
```
```ocaml
module Custom_data : sig ... end
```
```ocaml
module Printer : 
  Xml_sigs.Typed_pp with type +'a elt := 'a F.elt and type doc := F.doc
```