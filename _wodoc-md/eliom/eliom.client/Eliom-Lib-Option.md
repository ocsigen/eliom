
# Module `Lib.Option`

Module Option to compute type `'a option`

```ocaml
type 'a t = 'a option
```
```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```
```ocaml
val get : (unit -> 'a) -> 'a t -> 'a
```
```ocaml
val get' : 'a -> 'a t -> 'a
```
```ocaml
val iter : ('a -> unit) -> 'a t -> unit
```
```ocaml
val return : 'a -> 'a t
```
```ocaml
val bind : 'a t -> ('a -> 'b t) -> 'b t
```
```ocaml
val to_list : 'a t -> 'a list
```