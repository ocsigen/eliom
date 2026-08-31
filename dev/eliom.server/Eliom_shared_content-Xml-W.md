# Module `Xml.W`

```ocaml
type 'a t = 'a Eliom_shared.React.S.t
```
```ocaml
val return : 'a -> 'a t
```
```ocaml
type (-'a, 'b) ft
```
```ocaml
val fmap : ('a, 'b) ft -> 'a t -> 'b t
```
```ocaml
type 'a tlist = 'a Eliom_shared.ReactiveData.RList.t
```
```ocaml
val nil : unit -> 'a tlist
```
```ocaml
val singleton : 'a t -> 'a tlist
```
```ocaml
val cons : 'a t -> 'a tlist -> 'a tlist
```
```ocaml
val append : 'a tlist -> 'a tlist -> 'a tlist
```
```ocaml
val map : ('a, 'b) ft -> 'a tlist -> 'b tlist
```
