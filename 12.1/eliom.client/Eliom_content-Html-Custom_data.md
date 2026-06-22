
# Module `Html.Custom_data`

```ocaml
type 'a t
```
Custom data with values of type `'a`.

```ocaml
val create : 
  name:string ->
  ?default:'a ->
  to_string:('a -> string) ->
  of_string:(string -> 'a) ->
  unit ->
  'a t
```
Create a custom data field by providing string conversion functions. If the `default` is provided, calls to [`Eliom_content.Html.Custom_data.get_dom`](./#val-get_dom) return that instead of throwing an exception `Not_found`.

```ocaml
val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t
```
Create a custom data from a Json-deriving type.

```ocaml
val attrib : 'a t -> 'a -> [> `User_data ] attrib
```
`attrib my_data value ` creates a HTML5 attribute for the custom-data type `my_data` with value `value` for injecting it into an a HTML5 tree ([`Eliom_content.Html.elt`](./Eliom_content-Html.md#type-elt)).

```ocaml
val get_dom : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> 'a t -> 'a
```
```ocaml
val set_dom : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  'a t ->
  'a ->
  unit
```