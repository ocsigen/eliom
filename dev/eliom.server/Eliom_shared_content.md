# Module `Eliom_shared_content`

```ocaml
module Xml : 
  Xml_sigs.T
    with type 'a W.t = 'a Eliom_shared.React.S.t
     and type 'a W.tlist = 'a Eliom_shared.ReactiveData.RList.t
     and type event_handler =
           (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t ->
             unit)
             Eliom_client_value.t
     and type mouse_event_handler =
           (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t ->
             unit)
             Eliom_client_value.t
     and type keyboard_event_handler =
           (Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t ->
             unit)
             Eliom_client_value.t
     and type touch_event_handler =
           (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
             unit)
             Eliom_client_value.t
```
```ocaml
module Svg : sig ... end
```
```ocaml
module Html : sig ... end
```
