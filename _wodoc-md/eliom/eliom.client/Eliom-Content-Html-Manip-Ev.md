
# Module `Manip.Ev`

```ocaml
type ('a, 'b) ev = 'a elt -> ('b Js_of_ocaml.Js.t -> bool) -> unit
```
```ocaml
type ('a, 'b) ev_unit = 'a elt -> ('b Js_of_ocaml.Js.t -> unit) -> unit
```
```ocaml
val onkeyup : ('a, Js_of_ocaml.Dom_html.keyboardEvent) ev
```
```ocaml
val onkeydown : ('a, Js_of_ocaml.Dom_html.keyboardEvent) ev
```
```ocaml
val onmouseup : ('a, Js_of_ocaml.Dom_html.mouseEvent) ev
```
```ocaml
val onmousedown : ('a, Js_of_ocaml.Dom_html.mouseEvent) ev
```
```ocaml
val onmouseout : ('a, Js_of_ocaml.Dom_html.mouseEvent) ev
```
```ocaml
val onmouseover : ('a, Js_of_ocaml.Dom_html.mouseEvent) ev
```
```ocaml
val onclick : ('a, Js_of_ocaml.Dom_html.mouseEvent) ev
```
```ocaml
val ondblclick : ('a, Js_of_ocaml.Dom_html.mouseEvent) ev
```
```ocaml
val onload : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onerror : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onabort : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onfocus : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onblur : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onfocus_textarea : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onblur_textarea : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onscroll : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onreturn : ('a, Js_of_ocaml.Dom_html.keyboardEvent) ev_unit
```
```ocaml
val onchange : ('a, Js_of_ocaml.Dom_html.event) ev
```
```ocaml
val onchange_select : ('a, Js_of_ocaml.Dom_html.event) ev
```