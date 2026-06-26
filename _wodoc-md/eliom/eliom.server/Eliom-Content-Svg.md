
# Module `Content.Svg`

Building and pretty-printing valid SVG tree. Information about Svg api can be found at [`Svg_sigs.T`](./../../tyxml/tyxml.functor/Svg_sigs-module-type-T.md)

See the Eliom manual for more information on[dom semantics vs. functional semantics](./../clientserver-html.md#unique) for SVG tree manipulated by client/server application.

```ocaml
type +'a elt
```
```ocaml
type +'a attrib
```
```ocaml
type 'a wrap = 'a
```
```ocaml
type 'a list_wrap = 'a list
```
```ocaml
type uri = Xml.uri
```
```ocaml
module F : sig ... end
```
Typed interface for building valid SVG tree (functional semantics). See [`Svg_sigs.T`](./../../tyxml/tyxml.functor/Svg_sigs-module-type-T.md).

```ocaml
module D : sig ... end
```
Typed interface for building valid SVG tree (DOM semantics). See [`Svg_sigs.T`](./../../tyxml/tyxml.functor/Svg_sigs-module-type-T.md).

```ocaml
module R : sig ... end
```
Creation of SVG content from shared reactive signals and data ([`Shared`](./Eliom-Shared.md)). For the operations provided, see [`Svg_sigs.T`](./../../tyxml/tyxml.functor/Svg_sigs-module-type-T.md).

```ocaml
module C : sig ... end
```
Creation of content from client-side values. This makes possible to insert in server side generated pages some nodes that will be computed on client side (for example reactive nodes).

```ocaml
module Id : sig ... end
```
Node identifiers.

```ocaml
module Printer : 
  Xml_sigs.Typed_pp with type +'a elt := 'a elt and type doc := F.doc
```
SVG printer. See [`Xml_sigs.Typed_pp`](./../../tyxml/tyxml.functor/Xml_sigs-module-type-Typed_pp.md).
