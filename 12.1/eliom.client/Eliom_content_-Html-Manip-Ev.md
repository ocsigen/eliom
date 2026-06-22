
# Module `Manip.Ev`

```ocaml
type ('a, 'b) ev = 'a elt -> ('b Js_of_ocaml.Js.t -> bool) -> unit
```
```ocaml
type ('a, 'b) ev_unit = 'a elt -> ('b Js_of_ocaml.Js.t -> unit) -> unit
```
```ocaml
val bool_cb : 
  ((Js_of_ocaml.Dom_html.event as 'a) Js_of_ocaml.Js.t -> bool) ->
  ('b, 'a Js_of_ocaml.Js.t) Js_of_ocaml.Dom_html.event_listener
```
```ocaml
val onkeyup : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onkeydown : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onmouseup : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onmousedown : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onmouseout : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onmouseover : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onclick : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val ondblclick : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onload : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onerror : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onabort : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool) ->
  unit
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
val onscroll : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onreturn : 
  'a F.elt ->
  (Js_of_ocaml__Dom_html.keyboardEvent Js_of_ocaml.Js.t -> unit) ->
  unit
```
```ocaml
val onchange : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool) ->
  unit
```
```ocaml
val onchange_select : 
  'a F.elt ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> bool) ->
  unit
```