
# Module `Eliommod_cookies.Map`


## Parameters

```ocaml
module Ord : sig ... end
```

## Signature

```ocaml
type 'a t = 
  | Empty
  | Node of {
    l : 'a t;
    v : Ord.key;
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
val create : 'a t -> Ord.key -> 'a -> 'a t -> 'a t
```
```ocaml
val bal : 'a t -> Ord.key -> 'a -> 'a t -> 'a t
```
```ocaml
val add : Ord.key -> 'a -> 'a t -> 'a t
```
```ocaml
val fold : (Ord.key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
```
```ocaml
val empty : 'a t
```