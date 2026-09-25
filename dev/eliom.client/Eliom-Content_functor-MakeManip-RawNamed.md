# Module `MakeManip.RawNamed`

```ocaml
val appendChild : ?before:'a Kind.elt -> 'b Id.id -> 'c Kind.elt -> unit
```
```ocaml
val appendChildren : 
  ?before:'a Kind.elt ->
  'b Id.id ->
  'c Kind.elt list ->
  unit
```
```ocaml
val removeChild : 'a Id.id -> 'b Kind.elt -> unit
```
```ocaml
val replaceChild : 'a Id.id -> 'b Kind.elt -> 'c Kind.elt -> unit
```
```ocaml
val removeChildren : 'a Id.id -> unit
```
```ocaml
val replaceChildren : 'a Id.id -> 'b Kind.elt list -> unit
```
