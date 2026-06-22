
# Module `Eliom_content_core.Xml`

```ocaml
module W : 
  Xml_wrap.T
    with type 'a t = 'a
     and type 'a tlist = 'a list
     and type (-'a, 'b) ft = 'a -> 'b
```
```ocaml
type uri = string
```
```ocaml
val uri_of_string : uri -> string
```
```ocaml
val string_of_uri : string -> uri
```
```ocaml
val uri_of_fun : (unit -> string) -> uri
```
```ocaml
type aname = string
```
```ocaml
type attrib
```
```ocaml
type caml_event_handler = 
  | CE_registered_closure of string * Eliom_lib.poly
  | CE_client_closure of Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit
  | CE_client_closure_mouse of Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
    unit
  | CE_client_closure_keyboard of Js_of_ocaml.Dom_html.keyboardEvent
                                  Js_of_ocaml.Js.t ->
    unit
  | CE_client_closure_touch of Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
    unit
  | CE_call_service of ([ `A | `Form_get | `Form_post ]
                      * (bool * string list) option
                      * string option
                      * Ocsigen_lib_base.poly)
                       option
                       Eliom_lazy.request
```
```ocaml
type internal_event_handler = 
  | Raw of string
  | Caml of caml_event_handler
```
```ocaml
type event_handler = Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit
```
```ocaml
type mouse_event_handler =
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
  unit
```
```ocaml
type keyboard_event_handler =
  Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
  unit
```
```ocaml
type touch_event_handler =
  Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
  unit
```
```ocaml
type ename = string
```
```ocaml
type elt
```
```ocaml
type 'a wrap = 'a
```
```ocaml
type 'a list_wrap = 'a list
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