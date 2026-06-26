
# Module `Content.Xml_shared`

```ocaml
module W : 
  Xml_wrap.T
    with type 'a t = 'a Shared.React.S.t
    with type 'a tlist = 'a Shared.ReactiveData.RList.t
```
```ocaml
type 'a wrap = 'a W.t
```
```ocaml
type 'a list_wrap = 'a W.tlist
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
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit) Client_value.t
```
```ocaml
type mouse_event_handler =
  (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> unit) Client_value.t
```
```ocaml
type keyboard_event_handler =
  (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t -> unit) Client_value.t
```
```ocaml
type touch_event_handler =
  (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t -> unit) Client_value.t
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