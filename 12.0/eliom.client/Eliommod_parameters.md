
# Module `Eliommod_parameters`

```ocaml
type param = Js_of_ocaml.Form.form_elt
```
```ocaml
type field = [ 
  | `String of Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
  | `File of Js_of_ocaml.File.file Js_of_ocaml.Js.t
 ]
```
```ocaml
val insert_string : string -> field
```
```ocaml
val insert_file : Js_of_ocaml.File.file Js_of_ocaml.Js.t -> field
```
```ocaml
val to_string : field -> string
```
```ocaml
val inject_param_list : ('a * string) list -> ('a * field) list
```
```ocaml
val get_param_list : ('a * field) list -> ('a * string) list
```
```ocaml
val inject_param_table : 
  (string * string) list Eliom_lib.String.Table.t ->
  (string * param) list Eliom_lib.String.Table.t
```
```ocaml
val string_of_param : 
  [< `File of 'a | `String of Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t ] ->
  string
```