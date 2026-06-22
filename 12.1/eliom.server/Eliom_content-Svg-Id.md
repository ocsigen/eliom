
# Module `Svg.Id`

Node identifiers.

```ocaml
type +'a id
```
The type of global SVG element identifier.

```ocaml
val new_elt_id : ?global:bool -> unit -> 'a id
```
The function `new_elt_id ()` creates a new HTML5 element identifier. (see the Eliom manual for more information on [global element](./../clientserver-html.md#global)).

```ocaml
val create_named_elt : id:'a id -> 'a elt -> 'a elt
```
The function `create_named_elt ~id elt` create a copy of the element `elt` that will be accessible through the name `id`.

```ocaml
val create_global_elt : 'a elt -> 'a elt
```
The function `create_named_elt elt` is equivalent to `create_named_elt ~id:(new_elt_id ()) elt`.

```ocaml
val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
```
`create_request_elt ?reset elt` creates a referable copy of `elt`. If `~reset = true` is provided (default: false), a new ID is created even if `elt` has an ID already.
