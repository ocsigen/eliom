
# Module `S.Lwt`

Cooperative versions of the React operators

```ocaml
val map_s : 
  ?eq:('b -> 'b -> bool) Value.t ->
  ('a -> 'b Lwt.t) Value.t ->
  'a t ->
  'b t Lwt.t
```
```ocaml
val l2_s : 
  ?eq:('c -> 'c -> bool) Value.t ->
  ('a -> 'b -> 'c Lwt.t) Value.t ->
  'a t ->
  'b t ->
  'c t Lwt.t
```
```ocaml
val l3_s : 
  ?eq:('d -> 'd -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd Lwt.t) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t Lwt.t
```
```ocaml
val l4_s : 
  ?eq:('e -> 'e -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t Lwt.t
```
```ocaml
val l5_s : 
  ?eq:('f -> 'f -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) Value.t ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t Lwt.t
```
```ocaml
val l6_s : 
  ?eq:('g -> 'g -> bool) Value.t ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) Value.t ->
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
  ?eq:('a -> 'a -> bool) Value.t ->
  ('a -> 'b -> 'a Lwt.t) Value.t ->
  'a ->
  'b t list ->
  'a t Lwt.t
```