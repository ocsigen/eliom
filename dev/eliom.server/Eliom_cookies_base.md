# Module `Eliom_cookies_base`

```ocaml
type cookie = Ocsigen_cookie_map.cookie = 
  | OSet of float option * string * bool
  | OUnset
```
```ocaml
val cookie_of_json : Deriving_Json_lexer.lexbuf -> cookie
```
```ocaml
val cookie_to_json : Buffer.t -> cookie -> unit
```
```ocaml
val cookie_json : cookie Deriving_Json.t
```
```ocaml
type cookie_array = (string array * (string * cookie) array) array
```
```ocaml
val cookie_array_of_json : Deriving_Json_lexer.lexbuf -> cookie_array
```
```ocaml
val cookie_array_to_json : Buffer.t -> cookie_array -> unit
```
```ocaml
val cookie_array_json : cookie_array Deriving_Json.t
```
```ocaml
val cookieset_to_json : 
  cookie Ocsigen_cookie_map.Map_inner.t Ocsigen_cookie_map.Map_path.t ->
  string
```
changes to cookieset\_to\_json must be completed by corresponding changes in cookieset\_of\_json

```ocaml
val cookieset_of_json : string -> Ocsigen_cookie_map.t
```
