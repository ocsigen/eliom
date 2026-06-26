
# Module `Lib.Clist`

Circular lists

```ocaml
type 'a t = 'a Ocsigen_lib_base.Clist.t
```
```ocaml
type 'a node = 'a Ocsigen_lib_base.Clist.node
```
```ocaml
val make : 'a -> 'a node
```
```ocaml
val create : unit -> 'a t
```
```ocaml
val insert : 'a t -> 'a node -> unit
```
```ocaml
val remove : 'a node -> unit
```
```ocaml
val value : 'a node -> 'a
```
```ocaml
val in_list : 'a node -> bool
```
```ocaml
val is_empty : 'a t -> bool
```
```ocaml
val iter : ('a -> unit) -> 'a t -> unit
```
Infinite iteration on circular lists

```ocaml
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
```
Infinite fold on circular lists (use with care!)
