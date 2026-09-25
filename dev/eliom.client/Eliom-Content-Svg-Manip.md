# Module `Svg.Manip`

DOM-like manipulation functions.

In this module, all the functions apply only to SVG element with [Dom semantics](./../clientserver-html.md#unique).

```ocaml
val appendChild : ?before:'a elt -> 'b elt -> 'c elt -> unit
```
`appendChild e1 e2` inserts the element `e2` as last child of `e1`. If the optional parameter `~before:e3` is present and if `e3` is a child of `e1`, then `e2` is inserted before `e3` in the list of `e1` children.

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
`removeChild e1 e2` removes for `e2` from the list of children of `e1`.

```ocaml
val replaceChild : 'a elt -> 'b elt -> 'c elt -> unit
```
`replace e1 e2 e3` replaces `e3` by `e2` in the list of children of `e1`.

```ocaml
val removeChildren : 'a elt -> unit
```
`removeChildren e1` removes all children of `e1`.

```ocaml
val removeSelf : 'a elt -> unit
```
`removeSelf e` removes element e from the DOM.

```ocaml
val replaceChildren : 'a elt -> 'b elt list -> unit
```
`replaceChildren e1 elts` replaces all the children of `e1` by `elt`.

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
module Named : sig ... end
```
Dom manipulation by element identifier.

```ocaml
module Class : sig ... end
```
