# Module `Eliom_client_core.ReactState`

```ocaml
type t
```
```ocaml
val start_signal : 
  (t -> unit React.signal) ->
  Js_of_ocaml.Dom.node Js_of_ocaml.Js.t
```
```ocaml
val change_dom : t -> Js_of_ocaml.Dom.node Js_of_ocaml.Js.t -> unit
```
