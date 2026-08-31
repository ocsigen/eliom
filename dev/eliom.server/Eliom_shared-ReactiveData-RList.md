# Module `ReactiveData.RList`

```ocaml
type 'a t
```
The type of (shared) reactive lists

```ocaml
type 'a handle
```
Handles are used to manipulate reactive lists

```ocaml
val create : 
  ?default:
    ('a FakeReactiveData.RList.t * 'a FakeReactiveData.RList.handle) option
      Eliom_client_value.t ->
  ?reset_default:bool ->
  'a list ->
  'a t * 'a handle
```
`create ?default ?reset_default l` produces a pair `l, f`, where `s` is a (shared) reactive list, and `f` is a handle for manipulating the list.

The initial value of the list is `l`, unless `default` is provided. `default`, if provided, is used as the client-side list (and corresponding handle). `reset_default`, if set to true (default: false), resets the value of `default` to `l`.

```ocaml
val concat : 'a t -> 'a t -> 'a t
```
```ocaml
val value : 'a t -> 'a list Value.t
```
```ocaml
val signal : ?eq:('a -> 'a -> bool) Value.t -> 'a t -> 'a list React.S.t
```
```ocaml
val singleton_s : 'a React.S.t -> 'a t
```
```ocaml
val map : ('a -> 'b) Value.t -> 'a t -> 'b t
```
```ocaml
val from_signal : ?eq:('a -> 'a -> bool) Value.t -> 'a list React.S.t -> 'a t
```
```ocaml
val acc_e : 
  ?init:('a t * 'a handle) ->
  'a React.E.t Eliom_client_value.t ->
  'a t
```
```ocaml
module Lwt : sig ... end
```
Cooperative versions of the ReactiveData operators

```ocaml
val synced : 'a t -> bool
```
If `synced l` is true, then the server-side and client-side values of `l` are equal. This means that the client-side code can initially rely on the server-provided value, and defer any updates until the first client-side update of `l`.
