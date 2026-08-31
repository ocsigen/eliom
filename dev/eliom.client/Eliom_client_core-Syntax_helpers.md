# Module `Eliom_client_core.Syntax_helpers`

```ocaml
val register_client_closure : string -> ('a -> 'b) -> unit
```
```ocaml
val open_client_section : Eliom_lib.String_map.key -> unit
```
```ocaml
val close_server_section : Eliom_lib.String_map.key -> unit
```
```ocaml
val get_escaped_value : Eliom_lib.poly -> 'a
```
```ocaml
val get_injection : ?ident:string -> ?pos:Eliom_lib.pos -> string -> 'a
```
