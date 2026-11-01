# Module `Ocsipersist.Functorial`

Functorial frontent. Allows for custom (de)serialisation functions, which keeps data human-readable in the backend.

```ocaml
type internal
```
```ocaml
module type COLUMN = sig ... end
```
```ocaml
module Table
  (_ : sig ... end)
  (Key : COLUMN)
  (Value : COLUMN) : 
  Ocsipersist_lib.Sigs.TABLE with type key = Key.t and type value = Value.t
```
```ocaml
module Column : sig ... end
```
