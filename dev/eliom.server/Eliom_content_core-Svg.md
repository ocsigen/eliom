
# Module `Eliom_content_core.Svg`

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
  (_ : Svg_sigs.Wrapped_functions with module Xml = Xml) : 
  Svg_sigs.Make(Xml).T
    with type +'a elt = 'a elt
     and type +'a attrib = 'a attrib
```
```ocaml
module Id : sig ... end
```
```ocaml
module Printer : 
  Xml_sigs.Typed_pp with type +'a elt := 'a F.elt and type doc := F.doc
```