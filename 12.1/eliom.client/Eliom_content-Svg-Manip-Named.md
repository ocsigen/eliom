
# Module `Manip.Named`

Dom manipulation by element identifier.

The module `Named` defines the same functions as `Eliom_dom`. They take as parameter an element identifier instead of an element with Dom semantics. Those functions only works if the element is available in the application (sent in the page or along the page). If the element is not available, those functions raise with `Not_found`.

```ocaml
val appendChild : ?before:'a elt -> 'b Id.id -> 'c elt -> unit
```
see `appendChild`

```ocaml
val appendChildren : ?before:'a elt -> 'b Id.id -> 'c elt list -> unit
```
see `appendChildren`

```ocaml
val removeChild : 'a Id.id -> 'b elt -> unit
```
see `removeChild`

```ocaml
val replaceChild : 'a Id.id -> 'b elt -> 'c elt -> unit
```
see `replaceChild`

```ocaml
val removeChildren : 'a Id.id -> unit
```
see `removeChildren`

```ocaml
val replaceChildren : 'a Id.id -> 'b elt list -> unit
```
see `replaceChildren`
