# Module `Ppx_eliom_utils`

### Various helping functions

```ocaml
val sequence : 
  ?loc:Ppxlib.Location.t ->
  ?attrs:Ppxlib.Parsetree.attribute list ->
  Ppxlib.Parsetree.expression list ->
  Ppxlib.Parsetree.expression
```
```ocaml
val str : 
  ?loc:Ppxlib.Location.t ->
  ?attrs:Ppxlib.Parsetree.attribute list ->
  string ->
  Ppxlib.Parsetree.expression
```
```ocaml
val int : 
  ?loc:Ppxlib.Location.t ->
  ?attrs:Ppxlib.Parsetree.attribute list ->
  int ->
  Ppxlib.Parsetree.expression
```
```ocaml
val id_file_hash : Ppxlib.Location.t -> string Ppxlib.Location.loc
```
Name of the variable which holds the hash of the file.

```ocaml
val eid : string Ppxlib.Location.loc -> Ppxlib.Parsetree.expression
```
```ocaml
val position : Ppxlib.Location.t -> Ppxlib.Parsetree.expression
```
```ocaml
val format_args : 
  Ppxlib.Parsetree.expression list ->
  Ppxlib.Parsetree.expression
```
```ocaml
val pat_args : Ppxlib.Parsetree.pattern list -> Ppxlib.Parsetree.pattern
```
```ocaml
module Context : sig ... end
```
Context convenience module.

```ocaml
module Mli : sig ... end
```
```ocaml
module Cmo : sig ... end
```
```ocaml
module type Pass = sig ... end
```
Signature of specific code of a preprocessor.

```ocaml
val driver_args : (Arg.key * Arg.spec * Arg.doc) list
```
```ocaml
module Make (_ : Pass) : sig ... end
```
