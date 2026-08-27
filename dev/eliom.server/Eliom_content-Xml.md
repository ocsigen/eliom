
# Module `Eliom_content.Xml`

Low-level XML manipulation.


### Base functions

See [`Xml_sigs.Iterable`](./../../tyxml/tyxml.functor/Xml_sigs-module-type-Iterable.md).

```ocaml
module W = Xml_wrap.NoWrap
```
```ocaml
type 'a wrap = 'a
```
```ocaml
type 'a list_wrap = 'a list
```
```ocaml
type uri
```
```ocaml
val string_of_uri : (uri, string) W.ft
```
```ocaml
val uri_of_string : (string, uri) W.ft
```
```ocaml
type aname = string
```
```ocaml
type event_handler =
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit) Eliom_client_value.t
```
```ocaml
type mouse_event_handler =
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit)
    Eliom_client_value.t
```
```ocaml
type keyboard_event_handler =
  (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
    unit)
    Eliom_client_value.t
```
```ocaml
type touch_event_handler =
  (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
    unit)
    Eliom_client_value.t
```
```ocaml
type attrib
```
```ocaml
val float_attrib : aname -> float wrap -> attrib
```
```ocaml
val int_attrib : aname -> int wrap -> attrib
```
```ocaml
val string_attrib : aname -> string wrap -> attrib
```
```ocaml
val space_sep_attrib : aname -> string list wrap -> attrib
```
```ocaml
val comma_sep_attrib : aname -> string list wrap -> attrib
```
```ocaml
val event_handler_attrib : aname -> event_handler -> attrib
```
```ocaml
val mouse_event_handler_attrib : aname -> mouse_event_handler -> attrib
```
```ocaml
val keyboard_event_handler_attrib : aname -> keyboard_event_handler -> attrib
```
```ocaml
val touch_event_handler_attrib : aname -> touch_event_handler -> attrib
```
```ocaml
val uri_attrib : aname -> uri wrap -> attrib
```
```ocaml
val uris_attrib : aname -> uri list wrap -> attrib
```
```ocaml
type elt
```
```ocaml
type ename = string
```
```ocaml
val empty : unit -> elt
```
```ocaml
val comment : string -> elt
```
```ocaml
val pcdata : string wrap -> elt
```
```ocaml
val encodedpcdata : string wrap -> elt
```
```ocaml
val entity : string -> elt
```
```ocaml
val leaf : ?a:attrib list -> ename -> elt
```
```ocaml
val node : ?a:attrib list -> ename -> elt list_wrap -> elt
```
```ocaml
val cdata : string -> elt
```
```ocaml
val cdata_script : string -> elt
```
```ocaml
val cdata_style : string -> elt
```
```ocaml
type separator = 
  | Space
  | Comma
```
```ocaml
val aname : attrib -> aname
```
```ocaml
type acontent = private 
  | AFloat of float
  | AInt of int
  | AStr of string
  | AStrL of separator * string list
```
```ocaml
val acontent : attrib -> acontent
```
```ocaml
type econtent = private 
  | Empty
  | Comment of string
  | EncodedPCDATA of string
  | PCDATA of string
  | Entity of string
  | Leaf of ename * attrib list
  | Node of ename * attrib list * elt list
```
```ocaml
val content : elt -> econtent
```

### Unique nodes

Unique nodes are XML nodes that are manipulated 'by reference' when sent to the client part of an Eliom-application: the created element is allocated only one time in each instance of an application. See [the eliom manual](./../clientserver-html.md#unique) for more details.


### Event handlers

```ocaml
type caml_event_handler
```
Values of type `caml_event_handler` represents event handler build with the `{{ ... }}` syntax (see the Eliom manual for more information on [syntax extension](./../clientserver-html.md#syntax)). Such values are expected by functions like `Eliom_content.Html.a_onclick`.

```ocaml
val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value
```
`Eliom_content.Xml.wrap page v` is like `Eliom_wrap.wrap v` but it makes sure that all `elt`s in `v` which are included in `page` are sent with empty content. This is safe because such elements will be taken from the DOM on the client either ways.
