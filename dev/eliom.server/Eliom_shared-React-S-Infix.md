# Module `S.Infix`

Infix operators

```ocaml
val (>|=) : 'a t -> ('a -> 'b) Value.t -> 'b t
```
`s >|= f` is `map f s`.

```ocaml
val (=|<) : ('a -> 'b) Value.t -> 'a t -> 'b t
```
`f =|< s` is `map f s`.
