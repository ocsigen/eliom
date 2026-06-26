
# Module `Client_core.Syntax_helpers`

```ocaml
val register_client_closure : string -> ('a -> 'b) -> unit
```
```ocaml
val open_client_section : Lib.String_map.key -> unit
```
```ocaml
val close_server_section : Lib.String_map.key -> unit
```
```ocaml
val get_escaped_value : Lib.poly -> 'a
```
```ocaml
val get_injection : ?ident:string -> ?pos:Lib.pos -> string -> 'a
```