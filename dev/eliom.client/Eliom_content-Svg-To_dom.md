# Module `Svg.To_dom`

Conversion from Svg `elt`s to Javascript DOM elements (`<:` [`Js_of_ocaml.Dom_html.element`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-class-type-element.md)). One conversion function per source type (stressed by the `of_` prefix).

```ocaml
val of_element : 'a elt -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
```
```ocaml
val of_node : 'a elt -> Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val of_pcdata : [> `Pcdata ] elt -> Js_of_ocaml.Dom.text Js_of_ocaml.Js.t
```
