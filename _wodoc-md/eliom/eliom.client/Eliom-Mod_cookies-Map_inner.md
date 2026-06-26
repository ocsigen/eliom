
# Module `Mod_cookies.Map_inner`

```ocaml
type !'a t = 
  | Empty
  | Node of {
    l : 'a t;
    v : string;
    d : 'a;
    r : 'a t;
    h : int;
  }
```
```ocaml
val of_json : 
  (Deriving_Json_lexer.lexbuf -> 'a) ->
  Deriving_Json_lexer.lexbuf ->
  'a t
```
```ocaml
val to_json : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a t -> unit
```
```ocaml
val json : 'a Deriving_Json.t -> 'a t Deriving_Json.t
```
```ocaml
val height : 'a t -> int
```
```ocaml
val create : 'a t -> string -> 'a -> 'a t -> 'a t
```
```ocaml
val bal : 'a t -> string -> 'a -> 'a t -> 'a t
```
```ocaml
val add : string -> 'a -> 'a t -> 'a t
```
```ocaml
val fold : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
```
```ocaml
val empty : 'a t
```