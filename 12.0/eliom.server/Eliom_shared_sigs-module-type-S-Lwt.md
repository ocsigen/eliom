
# Module `S.Lwt`

Cooperative versions of the React operators

```ocaml
val map_s : 
  ?eq:('b -> 'b -> bool) sv ->
  ('a -> 'b Lwt.t) sv ->
  'a t ->
  'b t Lwt.t
```
```ocaml
val l2_s : 
  ?eq:('c -> 'c -> bool) sv ->
  ('a -> 'b -> 'c Lwt.t) sv ->
  'a t ->
  'b t ->
  'c t Lwt.t
```
```ocaml
val l3_s : 
  ?eq:('d -> 'd -> bool) sv ->
  ('a -> 'b -> 'c -> 'd Lwt.t) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t Lwt.t
```
```ocaml
val l4_s : 
  ?eq:('e -> 'e -> bool) sv ->
  ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t Lwt.t
```
```ocaml
val l5_s : 
  ?eq:('f -> 'f -> bool) sv ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t Lwt.t
```
```ocaml
val l6_s : 
  ?eq:('g -> 'g -> bool) sv ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) sv ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t Lwt.t
```
```ocaml
val merge_s : 
  ?eq:('a -> 'a -> bool) sv ->
  ('a -> 'b -> 'a Lwt.t) sv ->
  'a ->
  'b t list ->
  'a t Lwt.t
```