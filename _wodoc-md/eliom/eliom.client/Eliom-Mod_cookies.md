
# Module `Eliom.Mod_cookies`

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
```ocaml
val cookie_tables : 
  (float option * string * bool) Ocsigen_cookie_map.Map_inner.t
    Ocsigen_cookie_map.Map_path.t
    Js_of_ocaml.Jstable.t
```
```ocaml
module Map (Ord : sig ... end) : sig ... end
```
```ocaml
module Map_path : sig ... end
```
```ocaml
module Map_inner : sig ... end
```
```ocaml
val json_cookies : 
  (Deriving_Json.Json_float.a option
   * Deriving_Json.Json_string.a
   * Deriving_Json.Json_bool.a)
    Map_inner.t
    Map_path.t
    Deriving_Json.t
```
```ocaml
val extern_cookies : 
  'a Ocsigen_cookie_map.Map_inner.t Ocsigen_cookie_map.Map_path.t ->
  'a Map_inner.t Map_path.t
```
```ocaml
val intern_cookies : 
  'a Map_inner.t Map_path.t ->
  'a Ocsigen_cookie_map.Map_inner.t Ocsigen_cookie_map.Map_path.t
```
```ocaml
val get_table : 
  ?in_local_storage:bool ->
  string option ->
  (Deriving_Json.Json_float.a option
   * Deriving_Json.Json_string.a
   * Deriving_Json.Json_bool.a)
    Ocsigen_cookie_map.Map_inner.t
    Ocsigen_cookie_map.Map_path.t
```
`in_local_storage` implements cookie substitutes for iOS WKWebView

```ocaml
val set_table : 
  ?in_local_storage:bool ->
  string option ->
  (Deriving_Json.Json_float.a option
   * Deriving_Json.Json_string.a
   * Deriving_Json.Json_bool.a)
    Ocsigen_cookie_map.Map_inner.t
    Ocsigen_cookie_map.Map_path.t ->
  unit
```
`in_local_storage` implements cookie substitutes for iOS WKWebView

```ocaml
val now : unit -> float
```
```ocaml
val update_cookie_table : 
  ?in_local_storage:bool ->
  string option ->
  cookie Ocsigen_cookie_map.Map_inner.t Ocsigen_cookie_map.Map_path.t ->
  unit
```
`in_local_storage` implements cookie substitutes for iOS WKWebView

```ocaml
val get_cookies_to_send : 
  ?in_local_storage:bool ->
  string option ->
  bool ->
  Lib.Url.path ->
  (string * Deriving_Json.Json_string.a) list
```
`in_local_storage` implements cookie substitutes for iOS WKWebView

```ocaml
val make_new_session_id : unit -> 'a
```