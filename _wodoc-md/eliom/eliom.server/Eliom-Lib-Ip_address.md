
# Module `Lib.Ip_address`

```ocaml
exception No_such_host
```
```ocaml
val get_inet_addr : ?v6:bool -> string -> Unix.inet_addr Lwt.t
```