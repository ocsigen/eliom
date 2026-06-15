
# Module `Eliom_content.Svg`

Building valid SVG .

See the Eliom manual for more information on [dom semantics vs. functional semantics](./../clientserver-html.md#unique) for HTML5 tree manipulated by client/server application.

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
Creation of *f*unctional content (copy-able but not referable).

```ocaml
module D : sig ... end
```
Creation of content with *D*OM semantics (referable

```ocaml
module R : sig ... end
```
Creation of reactive content

```ocaml
module C : sig ... end
```
Creation of content from client-side values.

```ocaml
module Id : sig ... end
```
Node identifiers

```ocaml
module Manip : sig ... end
```
DOM-like manipulation functions.

```ocaml
module To_dom : sig ... end
```
Conversion from Svg `elt`s to Javascript DOM elements (`<:` [`Js_of_ocaml.Dom_html.element`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-class-type-element.md)). One conversion function per source type (stressed by the `of_` prefix).

```ocaml
module Of_dom : sig ... end
```
Conversion functions from DOM nodes ([`Js_of_ocaml.Dom_html.element`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-class-type-element.md) [`Js_of_ocaml.Js.t`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Js.md#type-t)) to Eliom nodes ([`Eliom_content.Html.elt`](./Eliom_content-Html.md#type-elt)).
