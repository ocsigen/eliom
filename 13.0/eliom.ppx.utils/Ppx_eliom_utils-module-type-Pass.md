# Module type `Ppx_eliom_utils.Pass`

Signature of specific code of a preprocessor.

How to handle "client", "shared" and "server" sections for top level structure items.

For shared and server, the boolean argument indicate if this declaration can lead to evaluation of a fragment.

```ocaml
val shared_str : 
  bool ->
  Ppxlib.Parsetree.structure_item ->
  Ppxlib.Parsetree.structure_item list
```
```ocaml
val server_str : 
  bool ->
  Ppxlib.Parsetree.structure_item ->
  Ppxlib.Parsetree.structure_item list
```
```ocaml
val client_str : 
  Ppxlib.Parsetree.structure_item ->
  Ppxlib.Parsetree.structure_item list
```
How to handle "client", "shared" and "server" sections for top level signature items.

```ocaml
val shared_sig : 
  Ppxlib.Parsetree.signature_item ->
  Ppxlib.Parsetree.signature_item list
```
```ocaml
val client_sig : 
  Ppxlib.Parsetree.signature_item ->
  Ppxlib.Parsetree.signature_item list
```
```ocaml
val server_sig : 
  Ppxlib.Parsetree.signature_item ->
  Ppxlib.Parsetree.signature_item list
```
```ocaml
val fragment : 
  loc:Ppxlib.Location.t ->
  ?typ:Ppxlib.Parsetree.core_type ->
  context:Context.server ->
  num:string ->
  id:string Ppxlib.Location.loc ->
  unsafe:bool ->
  Ppxlib.Parsetree.expression ->
  Ppxlib.Parsetree.expression
```
How to handle "`%client ...`" and "`%shared ...`" expr.

```ocaml
val escape_inject : 
  loc:Ppxlib.Location.t ->
  ?ident:string ->
  context:Context.escape_inject ->
  id:string Ppxlib.Location.loc ->
  unsafe:bool ->
  Ppxlib.Parsetree.expression ->
  Ppxlib.Parsetree.expression
```
How to handle escaped "~%ident" inside a fragment.

```ocaml
val prelude : Ppxlib.Location.t -> Ppxlib.Parsetree.structure
```
```ocaml
val postlude : Ppxlib.Location.t -> Ppxlib.Parsetree.structure
```
