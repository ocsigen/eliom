# Module type `Eliom_shared_sigs.RLIST`

```ocaml
type 'a t
```
The type of (shared) reactive lists

```ocaml
type 'a handle
```
Handles are used to manipulate reactive lists

```ocaml
type 'a signal
```
```ocaml
type 'a sv
```
The type of shared values

```ocaml
type 'a ct
```
Client-side version of 'a t

```ocaml
type 'a chandle
```
Client-side version of 'a handle

```ocaml
val create : 
  ?default:('a ct * 'a chandle) option Eliom_client_value.t ->
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
val value : 'a t -> 'a list sv
```
```ocaml
val signal : ?eq:('a -> 'a -> bool) sv -> 'a t -> 'a list signal
```
```ocaml
val singleton_s : 'a signal -> 'a t
```
```ocaml
val map : ('a -> 'b) sv -> 'a t -> 'b t
```
```ocaml
val from_signal : ?eq:('a -> 'a -> bool) sv -> 'a list signal -> 'a t
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
