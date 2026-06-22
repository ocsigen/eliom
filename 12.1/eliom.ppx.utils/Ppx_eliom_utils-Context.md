
# Module `Ppx_eliom_utils.Context`

Context convenience module.

```ocaml
type server = [ 
  | `Server
  | `Shared
 ]
```
```ocaml
type client = [ 
  | `Client
  | `Shared
 ]
```
```ocaml
type escape_inject = [ 
  | `Escaped_value of server
  | `Injection of client
 ]
```
```ocaml
type t = [ 
  | `Server
  | `Client
  | `Shared
  | `Fragment of server * bool
  | `Escaped_value of server
  | `Injection of client
 ]
```