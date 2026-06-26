
# Module `Content_core.Xml_wed`

```ocaml
module W : 
  module type of struct include Js_of_ocaml_tyxml.Tyxml_js.Wrap end
    with type 'a t = 'a React.signal
    with type 'a tlist = 'a ReactiveData.RList.t
    with type ('a, 'b) ft = 'a -> 'b
```
```ocaml
type 'a wrap = 'a W.t
```
```ocaml
type 'a list_wrap = 'a W.tlist
```
```ocaml
type uri = Xml.uri
```
```ocaml
val string_of_uri : (uri, string) W.ft
```
```ocaml
val uri_of_string : (string, uri) W.ft
```
```ocaml
type aname = Xml.aname
```
```ocaml
type event_handler
```
```ocaml
type mouse_event_handler
```
```ocaml
type keyboard_event_handler
```
```ocaml
type touch_event_handler
```
```ocaml
type attrib = Xml.attrib
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
type elt = Xml.elt
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
val cdata : string -> elt
```
```ocaml
val cdata_script : string -> elt
```
```ocaml
val cdata_style : string -> elt
```
```ocaml
val float_attrib : aname -> float React.S.t -> attrib
```
```ocaml
val int_attrib : aname -> int React.S.t -> attrib
```
```ocaml
val string_attrib : aname -> string React.S.t -> attrib
```
```ocaml
val space_sep_attrib : aname -> string list React.S.t -> attrib
```
```ocaml
val comma_sep_attrib : aname -> string list React.S.t -> attrib
```
```ocaml
val uri_attrib : aname -> uri React.S.t -> attrib
```
```ocaml
val uris_attrib : aname -> uri list React.S.t -> attrib
```
```ocaml
val node : ?a:attrib list -> string -> elt list_wrap -> elt
```