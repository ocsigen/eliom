
# Module `Html.Manip`

DOM-like manipulation functions.

In this module, all the functions apply only to HTML5 element with [Dom semantics](./../clientserver-html.md#unique).

```ocaml
val appendChild : ?before:'a elt -> 'b elt -> 'c elt -> unit
```
`appendChild e1 e2` inserts the element `e2` as last child of `e1`. If the optional parameter `~before:e3` is present and if `e3` is a child of `e1`, then `e2` is inserted before `e3` in the list of `e1` children.

```ocaml
val appendToBody : ?before:'a elt -> 'c elt -> unit
```
Append to the body of the document.

```ocaml
val appendChildren : ?before:'a elt -> 'b elt -> 'c elt list -> unit
```
`appendChildren e1 elts` inserts `elts` as last children of `e1`. If the optional parameter `~before:e3` is present and if `e3` is a child of `e1`, then `elts` are inserted before `e3` in the list of `e1` children.

```ocaml
val insertFirstChild : 'b elt -> 'c elt -> unit
```
`insertFirstChild p c` inserts `c` as first child of `p`

```ocaml
val nth : 'a elt -> int -> 'b elt option
```
`nth e n` returns the nth child of `e` (first is 0\)

```ocaml
val childLength : 'a elt -> int
```
`childLength e` returns the number of children of `e`

```ocaml
val removeChild : 'a elt -> 'b elt -> unit
```
The function `removeChild e1 e2` removes for `e2` from the list of `e1` children.

```ocaml
val replaceChild : 'a elt -> 'b elt -> 'c elt -> unit
```
The function `replace e1 e2 e3` replaces for `e2` by `e3` in the list of `e1` children.

```ocaml
val removeChildren : 'a elt -> unit
```
The function `removeChildren e1` removes `e1` children.

```ocaml
val removeSelf : 'a elt -> unit
```
`removeSelf e` removes element e from the DOM.

```ocaml
val replaceChildren : 'a elt -> 'b elt list -> unit
```
The function `replaceChildren e1 elts` replaces all the children of `e1` by `elt`.

```ocaml
val parentNode : 'a elt -> 'b elt option
```
`parentNode elt` returns the parent of `elt`, if any.

```ocaml
val nextSibling : 'a elt -> 'b elt option
```
`nextSibling elt` returns the next element that has the same parent, if `elt` is not the last.

```ocaml
val previousSibling : 'a elt -> 'b elt option
```
`previousSibling elt` returns the previous element that has the same parent, if `elt` is not the first.

```ocaml
val insertBefore : before:'a elt -> 'b elt -> unit
```
`insertBefore ~before elt` insert `elt` before `before`.

```ocaml
val insertAfter : after:'a elt -> 'b elt -> unit
```
`insertAfter ~after elt` insert `elt` after `after`.

```ocaml
val replaceSelf : 'a elt -> 'b elt -> unit
```
`replaceSelf elt1 elt2` replaces `elt1` by `elt2`.

```ocaml
val children : 'a elt -> 'b elt list
```
`children elt` returns the list of html children of `elt`.

```ocaml
val addEventListener : 
  ?capture:bool ->
  'a elt ->
  (Js_of_ocaml.Dom_html.event as 'b) Js_of_ocaml.Js.t
    Js_of_ocaml.Dom_html.Event.typ ->
  ('a elt -> 'b Js_of_ocaml.Js.t -> bool) ->
  Js_of_ocaml.Dom_html.event_listener_id
```
The function `addEventListener elt evt handler` attach the `handler` for the event `evt` on the element `elt`. See the Js\_of\_ocaml manual, for a list of [available events](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-Event.md).

```ocaml
module Named : sig ... end
```
Dom manipulation by element identifier.

```ocaml
val scrollIntoView : ?bottom:bool -> 'a elt -> unit
```
The function `scrollIntoView elt` scroll the page to a position where `elt` is displayed at the top of the window. If the optional parameter `~bottom:true` is present, the page is scrolled to a position where `elt` is displayed at the bottom of the window.

```ocaml
module Class : sig ... end
```
```ocaml
module Elt : sig ... end
```
```ocaml
module Ev : sig ... end
```
```ocaml
module Attr : sig ... end
```
```ocaml
module Css : sig ... end
```
Read the CSS properties of DOM elements.

```ocaml
module SetCss : sig ... end
```
Modify the CSS properties of DOM elements.
