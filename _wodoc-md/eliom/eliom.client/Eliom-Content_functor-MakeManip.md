
# Module `Content_functor.MakeManip`


## Parameters

```ocaml
module Kind : sig ... end
```
```ocaml
module To_dom : sig ... end
```
```ocaml
module Of_dom : sig ... end
```
```ocaml
module Id : sig ... end
```
```ocaml
module Ns : sig ... end
```

## Signature

```ocaml
val get_node : 'a Kind.elt -> Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val get_unique_node : 
  string ->
  'a Kind.elt ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val get_unique_elt : 
  string ->
  'a Kind.elt ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
```
```ocaml
val raw_appendChild : 
  ?before:'a Kind.elt ->
  < appendChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth
    ; insertBefore : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
      'res0 Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'b Kind.elt ->
  unit
```
```ocaml
val raw_appendChildren : 
  ?before:'a Kind.elt ->
  < appendChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth
    ; insertBefore : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
      'res0 Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'b Kind.elt list ->
  unit
```
```ocaml
val raw_removeChild : 
  < removeChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'a Kind.elt ->
  unit
```
```ocaml
val raw_replaceChild : 
  < replaceChild : 
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
    'res Js_of_ocaml.Js.meth.. >
    Js_of_ocaml.Js.t ->
  'a Kind.elt ->
  'b Kind.elt ->
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
  'c Kind.elt list ->
  unit
```
```ocaml
val nth : 'a Kind.elt -> int -> 'b Kind.elt option
```
```ocaml
val childLength : 'a Kind.elt -> int
```
```ocaml
val appendChild : ?before:'a Kind.elt -> 'b Kind.elt -> 'c Kind.elt -> unit
```
```ocaml
val appendChildren : 
  ?before:'a Kind.elt ->
  'b Kind.elt ->
  'c Kind.elt list ->
  unit
```
```ocaml
val removeChild : 'a Kind.elt -> 'b Kind.elt -> unit
```
```ocaml
val removeSelf : 'a Kind.elt -> unit
```
```ocaml
val insertFirstChild : 'a Kind.elt -> 'b Kind.elt -> unit
```
```ocaml
val replaceChild : 'a Kind.elt -> 'b Kind.elt -> 'c Kind.elt -> unit
```
```ocaml
val removeChildren : 'a Kind.elt -> unit
```
```ocaml
val replaceChildren : 'a Kind.elt -> 'b Kind.elt list -> unit
```
```ocaml
val childNodes : 'a Kind.elt -> Js_of_ocaml.Dom.node Js_of_ocaml.Js.t list
```
```ocaml
val filterElements : ('a -> 'b Js_of_ocaml.Js.Opt.t) -> 'a list -> 'b list
```
```ocaml
val childElements : 
  'a Kind.elt ->
  Js_of_ocaml__Dom.element Js_of_ocaml.Js.t list
```
```ocaml
val children : 'a Kind.elt -> 'b Kind.elt list
```
```ocaml
val parentNode : 'a Kind.elt -> 'b Kind.elt option
```
```ocaml
val nextSibling : 'a Kind.elt -> 'b Kind.elt option
```
```ocaml
val previousSibling : 'a Kind.elt -> 'b Kind.elt option
```
```ocaml
val insertBefore : before:'a Kind.elt -> 'b Kind.elt -> unit
```
```ocaml
val insertAfter : after:'a Kind.elt -> 'b Kind.elt -> unit
```
```ocaml
val replaceSelf : 'a Kind.elt -> 'b Kind.elt -> unit
```
```ocaml
module RawNamed : sig ... end
```
```ocaml
module Class : sig ... end
```