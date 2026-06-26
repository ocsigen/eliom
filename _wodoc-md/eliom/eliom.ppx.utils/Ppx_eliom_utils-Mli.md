
# Module `Ppx_eliom_utils.Mli`

```ocaml
val is_escaped_ident : string -> bool
```
```ocaml
val get_injected_ident_info : string -> string * int
```
```ocaml
val exists : unit -> bool
```
```ocaml
val find_escaped_ident : 
  string Ppxlib.Location.loc ->
  Ppxlib.Parsetree.core_type
```
```ocaml
val find_injected_ident : 
  string Ppxlib.Location.loc ->
  Ppxlib.Parsetree.core_type
```
```ocaml
val find_fragment : string Ppxlib.Location.loc -> Ppxlib.Parsetree.core_type
```