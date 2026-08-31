# Module `Svg.Id`

```ocaml
type +'a id
```
The type of global SVG element identifier.

```ocaml
val new_elt_id : ?global:bool -> unit -> 'a id
```
See [`Eliom_content.Html.Id.new_elt_id`](./Eliom_content-Html-Id.md#val-new_elt_id)

```ocaml
val create_named_elt : id:'a id -> 'a elt -> 'a elt
```
See [`Eliom_content.Html.Id.create_named_elt`](./Eliom_content-Html-Id.md#val-create_named_elt)

```ocaml
val create_global_elt : 'a elt -> 'a elt
```
See [`Eliom_content.Html.Id.create_global_elt`](./Eliom_content-Html-Id.md#val-create_global_elt)

```ocaml
val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
```
See [`Eliom_content.Html.Id.create_request_elt`](./Eliom_content-Html-Id.md#val-create_request_elt)
