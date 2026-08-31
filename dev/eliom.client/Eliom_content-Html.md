# Module `Eliom_content.Html`

Building valid (X)HTML5.

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
type 'a form_param
```
```ocaml
module F : sig ... end
```
Creation of *f*unctional HTML5 content (copy-able but not referable).

```ocaml
module D : sig ... end
```
Creation of HTML5 content with *D*OM semantics (referable)

```ocaml
module R : sig ... end
```
Creation of HTML5 content from [React](http://erratique.ch/software/react) signals. HTML5's trees are automatically updated whenever corresponding signals change.

```ocaml
module C : sig ... end
```
Creation of HTML5 content from client-side values. This module is available on client side only to make possible to use C-nodes in shared sections.

```ocaml
module Id : sig ... end
```
Node identifiers

```ocaml
module Custom_data : sig ... end
```
```ocaml
module To_dom : Js_of_ocaml_tyxml.Tyxml_cast_sigs.TO with type 'a elt = 'a elt
```
Conversion from HTML5 `elt`s to Javascript DOM elements (`<:` [`Js_of_ocaml.Dom_html.element`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-class-type-element.md)). One conversion function per source type (stressed by the `of_` prefix).

```ocaml
module Manip : sig ... end
```
DOM-like manipulation functions.

```ocaml
module Of_dom : Js_of_ocaml_tyxml.Tyxml_cast_sigs.OF with type 'a elt = 'a elt
```
Conversion functions from DOM nodes ([`Js_of_ocaml.Dom_html.element`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-class-type-element.md) [`Js_of_ocaml.Js.t`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Js.md#type-t)) to Eliom nodes ([`Eliom_content.Html.elt`](./#type-elt)).
