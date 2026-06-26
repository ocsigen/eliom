
# Module `Manip.Named`

```ocaml
val appendChild : ?before:'a F.elt -> 'b Id.id -> 'c F.elt -> unit
```
```ocaml
val appendChildren : ?before:'a F.elt -> 'b Id.id -> 'c F.elt list -> unit
```
```ocaml
val removeChild : 'a Id.id -> 'b F.elt -> unit
```
```ocaml
val replaceChild : 'a Id.id -> 'b F.elt -> 'c F.elt -> unit
```
```ocaml
val removeChildren : 'a Id.id -> unit
```
```ocaml
val replaceChildren : 'a Id.id -> 'b F.elt list -> unit
```
```ocaml
val addEventListener : 
  ?capture:bool ->
  'a Id.id ->
  (Js_of_ocaml.Dom_html.event as 'b) Js_of_ocaml.Js.t
    Js_of_ocaml.Dom_html.Event.typ ->
  ('c Content_core.Html.F.elt -> 'b Js_of_ocaml.Js.t -> bool) ->
  Js_of_ocaml.Dom_html.event_listener_id
```