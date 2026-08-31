# Module `S.Infix`

Infix operators

```ocaml
val (>|=) : 'a t -> ('a -> 'b) sv -> 'b t
```
`s >|= f` is `map f s`.

```ocaml
val (=|<) : ('a -> 'b) sv -> 'a t -> 'b t
```
`f =|< s` is `map f s`.
