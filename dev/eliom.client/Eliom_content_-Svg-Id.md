
# Module `Svg.Id`

```ocaml
type +'a id = 'a Eliom_content_core.Svg.Id.id
```
The type of global SVG element identifier.

```ocaml
val new_elt_id : ?global:bool -> unit -> 'a id
```
See [`Eliom_content.Html.Id.new_elt_id`](./../eliom.server/Eliom_content-Html-Id.md#val-new_elt_id)

```ocaml
val create_named_elt : 
  id:'a id ->
  'a Eliom_content_core.Svg.elt ->
  'a Eliom_content_core.Svg.elt
```
See [`Eliom_content.Html.Id.create_named_elt`](./../eliom.server/Eliom_content-Html-Id.md#val-create_named_elt)

```ocaml
val create_global_elt : 
  'a Eliom_content_core.Svg.elt ->
  'a Eliom_content_core.Svg.elt
```
See [`Eliom_content.Html.Id.create_global_elt`](./../eliom.server/Eliom_content-Html-Id.md#val-create_global_elt)

```ocaml
val create_request_elt : 
  ?reset:bool ->
  'a Eliom_content_core.Svg.elt ->
  'a Eliom_content_core.Svg.elt
```
See [`Eliom_content.Html.Id.create_request_elt`](./../eliom.server/Eliom_content-Html-Id.md#val-create_request_elt)

```ocaml
val get_element' : 'a id -> Js_of_ocaml__Dom_html.element Js_of_ocaml.Js.t
```
```ocaml
val get_element : 'a id -> 'b Eliom_content_core.Svg.elt option
```