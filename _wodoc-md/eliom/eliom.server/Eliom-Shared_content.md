
# Module `Eliom.Shared_content`

```ocaml
module Xml : 
  Xml_sigs.T
    with type 'a W.t = 'a Shared.React.S.t
     and type 'a W.tlist = 'a Shared.ReactiveData.RList.t
     and type event_handler =
           (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit) Client_value.t
     and type mouse_event_handler =
           (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
             unit)
             Client_value.t
     and type keyboard_event_handler =
           (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
             unit)
             Client_value.t
     and type touch_event_handler =
           (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
             unit)
             Client_value.t
```
```ocaml
module Svg : sig ... end
```
```ocaml
module Html : sig ... end
```