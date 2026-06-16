
# Module `React.S`

```ocaml
type 'a t
```
```ocaml
val const : 'a -> 'a t
```
```ocaml
val value : 'a t -> 'a Value.t
```
```ocaml
val map : ?eq:('b -> 'b -> bool) Value.t -> ('a -> 'b) Value.t -> 'a t -> 'b t
```
```ocaml
val fmap : 
  ?eq:('b -> 'b -> bool) Value.t ->
  ('a -> 'b option) Value.t ->
  'b Value.t ->
  'a t ->
  'b t
```
```ocaml
val merge : 
  ?eq:('a -> 'a -> bool) Value.t ->
  ('a -> 'b -> 'a) Value.t ->
  'a ->
  'b t list ->
  'a t
```
```ocaml
val l2 : 
  ?eq:('c -> 'c -> bool) Value.t ->
  ('a -> 'b -> 'c) Value.t ->
  'a t ->
  'b t ->
  'c t
```
```ocaml
val l3 : 
  ?eq:('d -> 'd -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t
```
```ocaml
val l4 : 
  ?eq:('e -> 'e -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t
```
```ocaml
val l5 : 
  ?eq:('f -> 'f -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t
```
```ocaml
val l6 : 
  ?eq:('g -> 'g -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t
```
```ocaml
val switch : ?eq:('a -> 'a -> bool) Value.t -> 'a t t -> 'a t
```
```ocaml
module Infix : sig ... end
```
Infix operators

```ocaml
module Lwt : sig ... end
```
Cooperative versions of the React operators

```ocaml
val create : 
  ?default:
    ('a React.S.t * (?step:React.step -> 'a -> unit)) option
      Eliom_client_value.t ->
  ?reset_default:bool ->
  ?eq:('a -> 'a -> bool) Value.t ->
  'a ->
  'a t * (?step:React.step -> 'a -> unit) Value.t
```
`create ?default ?reset_default x` produces a pair `s, f`, where `s` is a (shared) reactive signal, and `f` is a shared function for updating the signal.

The initial value of the signal is `x`, unless `default` is provided. `default`, if provided, is used as the client-side signal. `reset_default`, if set to true (default: false), resets the value of `default` to `x`.

The behavior of `f` is undefined on the server side. On the client side, `f` behaves just like the standard React-provided update functions.

```ocaml
val synced : 'a t -> bool
```
If `synced s` is true, then the server-side and client-side values of `s` are equal. This means that the client-side code can initially rely on the server-provided value, and defer updates until the first client-side update of `s`.
