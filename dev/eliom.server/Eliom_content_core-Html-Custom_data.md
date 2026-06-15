
# Module `Html.Custom_data`

```ocaml
type 'a t
```
```ocaml
val create : 
  name:string ->
  ?default:'a ->
  to_string:('a -> string) ->
  of_string:(string -> 'a) ->
  unit ->
  'a t
```
```ocaml
val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t
```
```ocaml
val attrib : 'a t -> 'a -> [> `User_data ] attrib
```