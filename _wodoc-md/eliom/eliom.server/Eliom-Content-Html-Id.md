
# Module `Html.Id`

Node identifiers

```ocaml
type +'a id
```
The type of global HTML element identifier.

```ocaml
val new_elt_id : ?global:bool -> unit -> 'a id
```
The function `new_elt_id ()` creates a new global HTML element identifier (see the Eliom manual for more information on [global element](./../clientserver-html.md#global)).

```ocaml
val create_named_elt : id:'a id -> 'a elt -> 'a elt
```
The function `create_named_elt ~id elt` create a copy of the element `elt` that will be sent to client with the reference `id`.

```ocaml
val create_global_elt : 'a elt -> 'a elt
```
The function `create_named_elt elt` is equivalent to `create_named_elt ~id:(new_elt_id ()) elt`.

```ocaml
val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
```
`create_request_elt ?reset elt` creates a referable copy of `elt`. If `~reset = true` is provided (default: false), a new ID is created even if `elt` has an ID already.

```ocaml
val have_id : 'a id -> 'b elt -> bool
```