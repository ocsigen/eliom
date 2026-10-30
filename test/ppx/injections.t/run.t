Injections of server values in client code. The server sends the values of each
client section with the module; the client reads them back in the same order.

The server binds each injected value, and sends it with its number and the name
of the injected identifier. In shared code, the server side uses the value
itself:

  $ ppx_eliom_server --impl injections.eliom > server.ml
  $ grep -E '^(let|and) _eliom_injected_ident_|^let from_shared' server.ml
  let _eliom_injected_ident_xxLZGv1 = x
  and _eliom_injected_ident_xxLZGv2 = y
  let _eliom_injected_ident_xxLZGv3 = x * 2
  let _eliom_injected_ident_xxLZGv4 = Stdlib.max_int
  let _eliom_injected_ident_xxLZGv5 = x
  let from_shared = _eliom_injected_ident_xxLZGv5 + 1
  $ grep -Eo '\([0-9]+,$|\), (None|\(Some "[^"]*"\))\)' server.ml |
  >   sed -E 's/^\(([0-9]+),$/\1/; s/^\), (.*)\)$/\1/' | paste -d ' ' - -
  1 (Some "x")
  2 (Some "y")
  3 None
  4 (Some "Stdlib_max_int")
  5 (Some "x")

The type pass stores each injected value in a reference, to infer its type:

  $ ppx_eliom_types --impl injections.eliom |
  >   grep -Eo '_eliom_injected_ident_[^ ]* := \(Some .*\)|^let from_shared .*'
  _eliom_injected_ident_xxLZGv1 := (Some x)
  _eliom_injected_ident_xxLZGv2 := (Some y)
  _eliom_injected_ident_xxLZGv3 := (Some (x * 2))
  _eliom_injected_ident_xxLZGv4 := (Some Stdlib.max_int)
  _eliom_injected_ident_xxLZGv5 := (Some x)
  let from_shared = x + 1

The client reads each injection by its number. The same value injected twice is
read twice but sent once:

  $ ppx_eliom_client --impl injections.eliom |
  >   grep -Eo '\?ident:(None|\(Some "[^"]*"\))|"xxLZGv[0-9]+"' | paste -d ' ' - -
  ?ident:(Some "x") "xxLZGv1"
  ?ident:(Some "y") "xxLZGv2"
  ?ident:(Some "x") "xxLZGv1"
  ?ident:None "xxLZGv3"
  ?ident:(Some "Stdlib_max_int") "xxLZGv4"
  ?ident:(Some "x") "xxLZGv5"
