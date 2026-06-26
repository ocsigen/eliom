
# Module `R.Xml`

Underlying XML data-structure

The type variables in [`elt`](./Eliom-Content-Svg-R.md#type-elt) and [`attrib`](./Eliom-Content-Svg-R.md#type-attrib) are know as *phantom types*. The implementation, defined here, is actually monomorphic.

In particular, tyxml doesn't impose any overhead over the underlying representation. The [`tot`](./Eliom-Content-Svg-R.md#val-tot) and [`toelt`](./Eliom-Content-Svg-R.md#val-toelt) functions allows to convert between the typed and the untyped representation without any cost.

Note that some implementation may not be iterable or printable, such as the Dom representation exposed by js\_of\_ocaml.

```ocaml
module W : 
  Xml_wrap.T
    with type 'a t = 'a Content_core.Xml_wed.W.t
    with type 'a tlist = 'a Content_core.Xml_wed.W.tlist
    with type ('a, 'b) ft = ('a, 'b) Content_core.Xml_wed.W.ft
```
```ocaml
type 'a wrap = 'a W.t
```
```ocaml
type 'a list_wrap = 'a W.tlist
```
```ocaml
type uri = Content_core.Xml_wed.uri
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
type event_handler = Content_core.Xml_wed.event_handler
```
```ocaml
type mouse_event_handler = Content_core.Xml_wed.mouse_event_handler
```
```ocaml
type keyboard_event_handler = Content_core.Xml_wed.keyboard_event_handler
```
```ocaml
type touch_event_handler = Content_core.Xml_wed.touch_event_handler
```
```ocaml
type attrib = Content_core.Xml_wed.attrib
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
type elt = Content_core.Xml_wed.elt
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