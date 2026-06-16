
# Module `Html.Id`

```ocaml
type +'a id
```
```ocaml
val new_elt_id : ?global:bool -> unit -> 'a id
```
```ocaml
val create_named_elt : id:'a id -> 'a elt -> 'a elt
```
```ocaml
val create_global_elt : 'a elt -> 'a elt
```
```ocaml
val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
```