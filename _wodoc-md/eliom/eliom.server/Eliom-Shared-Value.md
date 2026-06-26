
# Module `Shared.Value`

Accessing shared values

```ocaml
type +'a t
```
```ocaml
val create : 'a -> 'a Client_value.t -> 'a t
```
```ocaml
val client : 'a t -> 'a Client_value.t
```
`client x` is the client-side portion of `x`.

```ocaml
val local : 'a t -> 'a
```
`local x` is the local portion of `x`.
