
# Module `RList.Lwt`

Cooperative versions of the ReactiveData operators

```ocaml
val map_p : ('a -> 'b Lwt.t) Value.t -> 'a t -> 'b t Lwt.t
```