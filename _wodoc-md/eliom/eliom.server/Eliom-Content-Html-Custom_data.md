
# Module `Html.Custom_data`

Type-safe custom data for HTML. See the [examples in the manual](./../clientserver-html.md#custom_data).

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
Create a custom data field by providing string conversion functions. If the `default` is provided, calls to `Content.Html.Custom_data.get_dom` return that instead of throwing an exception `Not_found`.

```ocaml
val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t
```
Create a custom data from a Json-deriving type.

```ocaml
val attrib : 'a t -> 'a -> [> `User_data ] attrib
```
`attrib my_data value ` creates a HTML attribute for the custom-data type `my_data` with value `value` for injecting it into an a HTML tree ([`Content.Html.elt`](./Eliom-Content-Html.md#type-elt)).
