# Module `Manip.RawNamed`

```ocaml
val appendChild : ?before:'a F.elt -> 'b Id.id -> 'c F.elt -> unit
```
```ocaml
val appendChildren : ?before:'a F.elt -> 'b Id.id -> 'c F.elt list -> unit
```
```ocaml
val removeChild : 'a Id.id -> 'b F.elt -> unit
```
```ocaml
val replaceChild : 'a Id.id -> 'b F.elt -> 'c F.elt -> unit
```
```ocaml
val removeChildren : 'a Id.id -> unit
```
```ocaml
val replaceChildren : 'a Id.id -> 'b F.elt list -> unit
```
