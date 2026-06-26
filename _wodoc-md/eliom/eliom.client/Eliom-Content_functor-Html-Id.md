
# Module `Html.Id`

```ocaml
type +'a id = 'a Content_core.Html.Id.id
```
The type of global HTML5 element identifier.

```ocaml
val new_elt_id : ?global:bool -> unit -> 'a id
```
The function `new_elt_id ()` creates a new HTML5 element identifier. (see the Eliom manual for more information on [global element](./../clientserver-html.md#global)).

```ocaml
val create_named_elt : 
  id:'a id ->
  'a Content_core.Html.elt ->
  'a Content_core.Html.elt
```
The function `create_named_elt ~id elt` create a copy of the element `elt` that will be accessible through the name `id`.

```ocaml
val create_global_elt : 'a Content_core.Html.elt -> 'a Content_core.Html.elt
```
The function `create_named_elt elt` is equivalent to `create_named_elt ~id:(new_elt_id ()) elt`.

```ocaml
val create_request_elt : 
  ?reset:bool ->
  'a Content_core.Html.elt ->
  'a Content_core.Html.elt
```
See [`Content.Svg.Id.create_request_elt`](./Eliom-Content-Svg-Id.md#val-create_request_elt)

```ocaml
val get_element' : 'a id -> Js_of_ocaml__Dom_html.element Js_of_ocaml.Js.t
```
```ocaml
val get_element : 'a id -> 'b Of_dom.elt option
```