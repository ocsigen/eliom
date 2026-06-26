
# Module `Eliom.Unwrap`

```ocaml
type unwrap_id
```
Values of type `unwrap_id` are used to identify a specific unwrapper.

```ocaml
val id_of_int : int -> unwrap_id
```
```ocaml
val unwrap_js : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> 'a
```
`unwrap_js v` unwraps the content of the JavaScript variable `v`
