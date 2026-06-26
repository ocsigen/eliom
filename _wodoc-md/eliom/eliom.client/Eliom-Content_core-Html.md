
# Module `Content_core.Html`

Building Html tree.

See the Eliom manual for more information on [dom semantics vs. functional semantics](./../clientserver-html.md#unique) for SVG tree manipulated by client/server application.

```ocaml
type +'a elt
```
```ocaml
type 'a wrap = 'a
```
```ocaml
type 'a list_wrap = 'a list
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
Typed interface for building valid HTML5 tree (functional semantics). See [`Html_sigs.T`](./../../tyxml/tyxml.functor/Html_sigs-module-type-T.md).

```ocaml
module D : sig ... end
```
Typed interface for building valid HTML5 tree (DOM semantics). See [`Html_sigs.T`](./../../tyxml/tyxml.functor/Html_sigs-module-type-T.md).

Typed interface for building valid HTML5 tree from [React](http://erratique.ch/software/react) signals. HTML5's trees are automatically updated whenever corresponding signals change.

[`Html_sigs.T`](./../../tyxml/tyxml.functor/Html_sigs-module-type-T.md).

```ocaml
module R : sig ... end
```
```ocaml
module Id : sig ... end
```
Node identifiers

```ocaml
module Custom_data : sig ... end
```
Type-safe custom data for HTML5. See the [examples in the manual](./../clientserver-html.md#custom_data).

```ocaml
module Of_dom : Js_of_ocaml_tyxml.Tyxml_cast_sigs.OF with type 'a elt = 'a elt
```
Conversion of Javascript DOM elements to HTML5 elts (with DOM semantics of course). One conversion function per source type (stressed by the `of_` prefix).
