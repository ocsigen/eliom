
# Module type `Eliom_shared_sigs.S`

```ocaml
type 'a t
```
```ocaml
type 'a sv
```
```ocaml
val const : 'a -> 'a t
```
```ocaml
val value : 'a t -> 'a sv
```
```ocaml
val map : ?eq:('b -> 'b -> bool) sv -> ('a -> 'b) sv -> 'a t -> 'b t
```
```ocaml
val fmap : 
  ?eq:('b -> 'b -> bool) sv ->
  ('a -> 'b option) sv ->
  'b sv ->
  'a t ->
  'b t
```
```ocaml
val merge : 
  ?eq:('a -> 'a -> bool) sv ->
  ('a -> 'b -> 'a) sv ->
  'a ->
  'b t list ->
  'a t
```
```ocaml
val l2 : 
  ?eq:('c -> 'c -> bool) sv ->
  ('a -> 'b -> 'c) sv ->
  'a t ->
  'b t ->
  'c t
```
```ocaml
val l3 : 
  ?eq:('d -> 'd -> bool) sv ->
  ('a -> 'b -> 'c -> 'd) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t
```
```ocaml
val l4 : 
  ?eq:('e -> 'e -> bool) sv ->
  ('a -> 'b -> 'c -> 'd -> 'e) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t
```
```ocaml
val l5 : 
  ?eq:('f -> 'f -> bool) sv ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t
```
```ocaml
val l6 : 
  ?eq:('g -> 'g -> bool) sv ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t
```
```ocaml
val switch : ?eq:('a -> 'a -> bool) sv -> 'a t t -> 'a t
```
```ocaml
module Infix : sig ... end
```
Infix operators

```ocaml
module Lwt : sig ... end
```
Cooperative versions of the React operators
