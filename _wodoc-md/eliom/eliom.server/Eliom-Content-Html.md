
# Module `Content.Html`

Building and printing valid HTML5 tree. Information about Html api can be found at [`Html_sigs.T`](./../../tyxml/tyxml.functor/Html_sigs-module-type-T.md) .

See [more information on dom semantics vs. functional semantics](./../clientserver-html.md#unique) in Eliom's manual for HTML5 tree manipulated by client/server application.

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
module type T = sig ... end
```
Tyxml HTML signature (with Eliom's links/forms extensions) shared by `F` and `D`, suitable as a functor parameter. Avoids `module type of Content.Html.F`, which captures wrapped module paths and triggers an OCaml strengthening bug (see `ia-reports/2026-04-ocaml-wrapped-mtof-bug.md`).

```ocaml
module F : sig ... end
```
Creation of **F**unctional HTML5 content (copy-able but not referable, see also [`Content`](./Eliom-Content.md)).

```ocaml
module D : sig ... end
```
Creation of HTML content with **D**OM semantics (referable, see also [`Content`](./Eliom-Content.md)).

```ocaml
module C : sig ... end
```
Creation of HTML content from client-side values. This makes possible to insert in server side generated pages some nodes that will be computed on client side (for example reactive nodes).

```ocaml
module Id : sig ... end
```
Node identifiers

```ocaml
module R : sig ... end
```
Creation of HTML content from shared reactive signals and data ([`Shared`](./Eliom-Shared.md)). For the operations provided, see [`Html_sigs.T`](./../../tyxml/tyxml.functor/Html_sigs-module-type-T.md).

```ocaml
module Custom_data : sig ... end
```
Type-safe custom data for HTML. See the [examples in the manual](./../clientserver-html.md#custom_data).

```ocaml
module Printer : 
  Xml_sigs.Typed_pp with type +'a elt := 'a elt and type doc := F.doc
```
["Polyglot"](http://dev.w3.org/html5/html-xhtml-author-guide/) HTML printer. See [`Xml_sigs.Typed_pp`](./../../tyxml/tyxml.functor/Xml_sigs-module-type-Typed_pp.md).
