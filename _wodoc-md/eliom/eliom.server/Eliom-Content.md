
# Module `Eliom.Content`

This module allows creating valid HTML content, or other XML formats.

XML tree manipulation within Eliom is based on the TyXML library but Eliom is using a custom representation for XML values (see [`Xml`](./Eliom-Content-Xml.md)). Then, `Content` redefines the two high level interfaces ([`Svg`](./Eliom-Content-Svg.md), [`Html`](./Eliom-Content-Html.md)) that are provided by TyXML for valid XML tree creation and printing.

- If you want to generate typed HTML, use [`Content.Html`](./Eliom-Content-Html.md),
- If you want to write untyped html, use `Content.Html_text`,
- If you want to generate typed svg, use [`Content.Svg`](./Eliom-Content-Svg.md).
Modules [`Content.Html`](./Eliom-Content-Html.md), [`Content.Svg`](./Eliom-Content-Svg.md) contain two sub-modules: [`Content.Html.F`](./Eliom-Content-Html-F.md), [`Content.Html.D`](./Eliom-Content-Html-D.md) corresponding to tow different semantics. They also contain a module [`Content.Html.C`](./Eliom-Content-Html-C.md) that allows to inject client-side content into server-side content.


###### Functional semantics

The `F` modules provides functions to create elements with *f*unctional semantics: they are standard OCaml values.

Use this module:

- if your application does not have a client-side part (server-side generated Web site)
- or if the client-side is not written with Eliom,
- or if you do not need to use this node from the client-side program (no injection `%n` on this node) and want to avoid the extra attributes added by module `D`.
If you use a `F`\-node `n` in an injection (`%n`), it is considered as any OCaml value, NOT precisely the copy you (possibly) inserted in the page. For example, `To_dom.of_element %n` will not refer to the element in the page, but create a new DOM node.


###### DOM semantics

The `D` module provides functions to create elements with *D*OM semantics: Firstly, they behave like DOM nodes, e.g. they can only be added once to the DOM tree even when appended several times. Secondly, those values have an identifier, which means they can be referred to on client side (by `%variable`) or used with the functions in `Content.Html.To_dom` and `Content.Html.Manip`.

In case of doubt, always use `D`\-nodes when you are writing a client-server Eliom app. You can also mix F-nodes and D-nodes.


###### Client-side value injection

The `C` modules provides functions to inject client-side elements and attributes into server-side content.

**Please read [Eliom's manual](./../clientserver-html.md) to learn how to generate HTML.**

```ocaml
module Xml : sig ... end
```
Low-level XML manipulation.

```ocaml
module Xml_shared : 
  Xml_sigs.T
    with type 'a W.t = 'a Shared.React.S.t
     and type 'a W.tlist = 'a Shared.ReactiveData.RList.t
     and type event_handler =
           (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit) Client_value.t
     and type mouse_event_handler =
           (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
             unit)
             Client_value.t
     and type keyboard_event_handler =
           (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
             unit)
             Client_value.t
     and type touch_event_handler =
           (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
             unit)
             Client_value.t
```
```ocaml
module Svg : sig ... end
```
Building and pretty-printing valid SVG tree. Information about Svg api can be found at [`Svg_sigs.T`](./../../tyxml/tyxml.functor/Svg_sigs-module-type-T.md)

```ocaml
module Html : sig ... end
```
Building and printing valid HTML5 tree. Information about Html api can be found at [`Html_sigs.T`](./../../tyxml/tyxml.functor/Html_sigs-module-type-T.md) .
