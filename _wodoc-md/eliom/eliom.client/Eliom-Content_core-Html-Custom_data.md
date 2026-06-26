
# Module `Html.Custom_data`

Type-safe custom data for HTML5. See the [examples in the manual](./../clientserver-html.md#custom_data).

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
Create a custom data field by providing string conversion functions. If the `default` is provided, calls to [`Content.Html.Custom_data.get_dom`](./Eliom-Content-Html-Custom_data.md#val-get_dom) return that instead of throwing an exception `Not_found`.

```ocaml
val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t
```
Create a custom data from a Json-deriving type.

```ocaml
val attrib : 'a t -> 'a -> [> `User_data ] attrib
```
`attrib my_data value ` creates a HTML5 attribute for the custom-data type `my_data` with value `value` for injecting it into an a HTML5 tree ([`Content.Html.elt`](./Eliom-Content-Html.md#type-elt)).

```ocaml
val get_dom : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> 'a t -> 'a
```
`get_dom element custom_data` gets the `custom_data` from a JavaScript `element` ([`Js_of_ocaml.Dom_html.element`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-class-type-element.md)).

returns The value encoded in the respective custom data attribute of element, or the default value, if any.
raises [`Not_found`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Not_found) if the element does not contain the respective custom data attribute and the custom\_data was created without default.
```ocaml
val set_dom : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  'a t ->
  'a ->
  unit
```
`set_dom element custom_data value` sets the custom data attribute for `custom_data` of an JavaScript `element` ([`Js_of_ocaml.Dom_html.element`](./../../js_of_ocaml/js_of_ocaml/Js_of_ocaml-Dom_html-class-type-element.md)) to `value`.
