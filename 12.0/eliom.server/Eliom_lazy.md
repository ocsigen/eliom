
# Module `Eliom_lazy`

```ocaml
type 'a request
```
```ocaml
val from_fun : (unit -> 'a) -> 'a request
```
```ocaml
val from_val : 'a -> 'a request
```
```ocaml
val force : 'a request -> 'a
```