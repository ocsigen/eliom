
# Module `Svg.Manip`

```ocaml
val get_node : 'a F.elt -> Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val get_unique_node : 
  string ->
  'a F.elt ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val get_unique_elt : 
  string ->
  'a F.elt ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
```
```ocaml
val raw_appendChild : 
  ?before:'a F.elt ->
  < appendChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth
    ; insertBefore : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
      'res0 Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'b F.elt ->
  unit
```
```ocaml
val raw_appendChildren : 
  ?before:'a F.elt ->
  < appendChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth
    ; insertBefore : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
      'res0 Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'b F.elt list ->
  unit
```
```ocaml
val raw_removeChild : 
  < removeChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'a F.elt ->
  unit
```
```ocaml
val raw_replaceChild : 
  < replaceChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'a F.elt ->
  'b F.elt ->
  unit
```
```ocaml
val raw_removeChildren : 
  < childNodes : 
    < get : 'a Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t.. >
      Js_of_ocaml.Js.gen_prop
    ; removeChild : 'a Js_of_ocaml.Js.t -> 'res Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  unit
```
```ocaml
val raw_replaceChildren : 
  < appendChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth
    ; childNodes : 
      < get : 'a Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t.. >
        Js_of_ocaml.Js.gen_prop
    ; removeChild : 'a Js_of_ocaml.Js.t -> 'b Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'c F.elt list ->
  unit
```
```ocaml
val nth : 'a F.elt -> int -> 'b F.elt option
```
```ocaml
val childLength : 'a F.elt -> int
```
```ocaml
val appendChild : ?before:'a F.elt -> 'b F.elt -> 'c F.elt -> unit
```
```ocaml
val appendChildren : ?before:'a F.elt -> 'b F.elt -> 'c F.elt list -> unit
```
```ocaml
val removeChild : 'a F.elt -> 'b F.elt -> unit
```
```ocaml
val removeSelf : 'a F.elt -> unit
```
```ocaml
val insertFirstChild : 'a F.elt -> 'b F.elt -> unit
```
```ocaml
val replaceChild : 'a F.elt -> 'b F.elt -> 'c F.elt -> unit
```
```ocaml
val removeChildren : 'a F.elt -> unit
```
```ocaml
val replaceChildren : 'a F.elt -> 'b F.elt list -> unit
```
```ocaml
val childNodes : 'a F.elt -> Js_of_ocaml.Dom.node Js_of_ocaml.Js.t list
```
```ocaml
val filterElements : ('a -> 'b Js_of_ocaml.Js.Opt.t) -> 'a list -> 'b list
```
```ocaml
val childElements : 'a F.elt -> Js_of_ocaml__Dom.element Js_of_ocaml.Js.t list
```
```ocaml
val children : 'a F.elt -> 'b F.elt list
```
```ocaml
val parentNode : 'a F.elt -> 'b F.elt option
```
```ocaml
val nextSibling : 'a F.elt -> 'b F.elt option
```
```ocaml
val previousSibling : 'a F.elt -> 'b F.elt option
```
```ocaml
val insertBefore : before:'a F.elt -> 'b F.elt -> unit
```
```ocaml
val insertAfter : after:'a F.elt -> 'b F.elt -> unit
```
```ocaml
val replaceSelf : 'a F.elt -> 'b F.elt -> unit
```
```ocaml
module RawNamed : sig ... end
```
```ocaml
module Class : sig ... end
```
```ocaml
module Named = RawNamed
```