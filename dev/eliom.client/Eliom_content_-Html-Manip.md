
# Module `Html.Manip`

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
val raw_addEventListener : 
  ?capture:bool ->
  (< appendChild : 
     Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
     Js_of_ocaml.Dom.node Js_of_ocaml__Js.t Js_of_ocaml__Js.meth
    ; childNodes : 
      < get : Js_of_ocaml.Dom.node Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t
        ; set : 
          Js_of_ocaml.Dom.node Js_of_ocaml.Dom.nodeList Js_of_ocaml.Js.t ->
          unit.. >
        Js_of_ocaml__Js.gen_prop
    ; cloneNode : 
      bool Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml__Js.t Js_of_ocaml__Js.meth
    ; compareDocumentPosition : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.DocumentPosition.t Js_of_ocaml.Js.meth
    ; contains : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      bool Js_of_ocaml.Js.t Js_of_ocaml.Js.meth
    ; dispatchEvent : 
      Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t ->
      bool Js_of_ocaml.Js.t Js_of_ocaml.Js.meth
    ; firstChild : 
      < get : Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
        ; set : 
          Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
          unit.. >
        Js_of_ocaml__Js.gen_prop
    ; getRootNode : Js_of_ocaml.Dom.node Js_of_ocaml__Js.t Js_of_ocaml__Js.meth
    ; getRootNode_options : 
      Js_of_ocaml.Dom.getRootNodeOptions Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml__Js.t Js_of_ocaml__Js.meth
    ; hasChildNodes : bool Js_of_ocaml.Js.t Js_of_ocaml.Js.meth
    ; insertBefore : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
      Js_of_ocaml.Dom.node Js_of_ocaml__Js.t Js_of_ocaml__Js.meth
    ; isConnected : < get : bool Js_of_ocaml.Js.t.. > Js_of_ocaml__Js.gen_prop
    ; isEqualNode : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      bool Js_of_ocaml.Js.t Js_of_ocaml.Js.meth
    ; isSameNode : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      bool Js_of_ocaml.Js.t Js_of_ocaml.Js.meth
    ; lastChild : 
      < get : Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
        ; set : 
          Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
          unit.. >
        Js_of_ocaml__Js.gen_prop
    ; lookupNamespaceURI : 
      Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
      Js_of_ocaml.Js.js_string Js_of_ocaml__Js.t Js_of_ocaml__Js.opt
        Js_of_ocaml__Js.meth
    ; lookupPrefix : 
      Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ->
      Js_of_ocaml.Js.js_string Js_of_ocaml__Js.t Js_of_ocaml__Js.opt
        Js_of_ocaml__Js.meth
    ; namespaceURI : 
      < get : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
        ; set : 
          Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
          unit.. >
        Js_of_ocaml__Js.gen_prop
    ; nextSibling : 
      < get : Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
        ; set : 
          Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
          unit.. >
        Js_of_ocaml__Js.gen_prop
    ; nodeName : 
      < get : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t.. >
        Js_of_ocaml__Js.gen_prop
    ; nodeType : < get : Js_of_ocaml.Dom.nodeType.. > Js_of_ocaml__Js.gen_prop
    ; nodeValue : 
      < get : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t Js_of_ocaml.Js.opt.. >
        Js_of_ocaml__Js.gen_prop
    ; normalize : unit Js_of_ocaml.Js.meth
    ; onanimationcancel : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.animationEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onanimationend : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.animationEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onanimationiteration : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.animationEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onanimationstart : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.animationEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onbeforetoggle : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.toggleEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onclick : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondblclick : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondrag : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondragend : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondragenter : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondragleave : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondragover : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondragstart : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ondrop : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.dragEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ongotpointercapture : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onkeydown : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onkeypress : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onkeyup : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onlostpointercapture : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onmousedown : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onmousemove : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onmouseout : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onmouseover : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onmouseup : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointercancel : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointerdown : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointerenter : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointerleave : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointermove : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointerout : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointerover : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onpointerup : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.pointerEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onscroll : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ontoggle : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.toggleEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ontransitioncancel : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ontransitionend : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ontransitionrun : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; ontransitionstart : 
      ('a Js_of_ocaml.Js.t,
        Js_of_ocaml.Dom_html.transitionEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; onwheel : 
      ('a Js_of_ocaml.Js.t, Js_of_ocaml.Dom_html.wheelEvent Js_of_ocaml.Js.t)
        Js_of_ocaml.Dom_html.event_listener
        Js_of_ocaml.Js.writeonly_prop
    ; parentElement : 
      < get : Js_of_ocaml.Dom.element Js_of_ocaml.Js.t Js_of_ocaml.Js.opt.. >
        Js_of_ocaml__Js.gen_prop
    ; parentNode : 
      < get : Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
        ; set : 
          Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
          unit.. >
        Js_of_ocaml__Js.gen_prop
    ; previousSibling : 
      < get : Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt
        ; set : 
          Js_of_ocaml.Dom.node Js_of_ocaml.Js.t Js_of_ocaml.Js.opt ->
          unit.. >
        Js_of_ocaml__Js.gen_prop
    ; removeChild : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml__Js.t Js_of_ocaml__Js.meth
    ; replaceChild : 
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml.Js.t ->
      Js_of_ocaml.Dom.node Js_of_ocaml__Js.t Js_of_ocaml__Js.meth.. > as 'a)
    Js_of_ocaml.Js.t ->
  (Js_of_ocaml.Dom_html.event as 'b) Js_of_ocaml.Js.t
    Js_of_ocaml.Dom_html.Event.typ ->
  ('c Eliom_content_core.Html.F.elt -> 'b Js_of_ocaml.Js.t -> bool) ->
  Js_of_ocaml.Dom_html.event_listener_id
```
```ocaml
val addEventListener : 
  ?capture:bool ->
  'a F.elt ->
  (Js_of_ocaml.Dom_html.event as 'b) Js_of_ocaml.Js.t
    Js_of_ocaml.Dom_html.Event.typ ->
  ('c Eliom_content_core.Html.F.elt -> 'b Js_of_ocaml.Js.t -> bool) ->
  Js_of_ocaml.Dom_html.event_listener_id
```
```ocaml
module Named : sig ... end
```
```ocaml
val appendToBody : ?before:'a F.elt -> 'b F.elt -> unit
```
```ocaml
val get_unique_elt_input : 
  string ->
  'a F.elt ->
  Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t
```
```ocaml
val get_unique_elt_select : 
  string ->
  'a F.elt ->
  Js_of_ocaml.Dom_html.selectElement Js_of_ocaml.Js.t
```
```ocaml
val get_unique_elt_textarea : 
  string ->
  'a F.elt ->
  Js_of_ocaml.Dom_html.textAreaElement Js_of_ocaml.Js.t
```
```ocaml
val get_unique_elt_img : 
  string ->
  'a F.elt ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t
```
```ocaml
val scrollIntoView : ?bottom:bool -> 'a F.elt -> unit
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
```ocaml
module SetCss : sig ... end
```