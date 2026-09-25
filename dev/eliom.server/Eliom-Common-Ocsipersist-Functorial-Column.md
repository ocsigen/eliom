# Module `Functorial.Column`

```ocaml
module String : COLUMN with type t = string
```
```ocaml
module Float : COLUMN with type t = float
```
```ocaml
module Marshal (C : sig ... end) : COLUMN with type t = C.t
```
```ocaml
module Json (C : sig ... end) : COLUMN with type t = C.t
```
