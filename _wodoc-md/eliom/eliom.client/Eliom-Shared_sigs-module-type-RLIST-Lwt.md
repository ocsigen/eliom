
# Module `RLIST.Lwt`

Cooperative versions of the ReactiveData operators

```ocaml
val map_p : ('a -> 'b Lwt.t) sv -> 'a t -> 'b t Lwt.t
```