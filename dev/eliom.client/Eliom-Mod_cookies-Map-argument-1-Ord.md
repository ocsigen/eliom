# Parameter `Map.Ord`

```ocaml
type key
```
```ocaml
val key_of_json : Deriving_Json_lexer.lexbuf -> key
```
```ocaml
val key_to_json : Buffer.t -> key -> unit
```
```ocaml
val key_json : key Deriving_Json.t
```
```ocaml
val compare : key -> key -> int
```
