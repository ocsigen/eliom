The client pass reads the types inferred on the server from the server cmo
given by `-server-cmo` (test/ppx/cmo tests a real one). Errors:

  $ ocamlc -c other.ml
  $ ppx_eliom_client -server-cmo missing.cmo --impl injection.eliom
  File "missing.cmo", line 1:
  Error: Eliom: Error while loading types: missing.cmo: No such file or directory
  [1]

  $ ppx_eliom_client -server-cmo other.ml --impl injection.eliom
  File "other.ml", line 1:
  Error: Eliom: Error while loading types: not an object file
  [1]

  $ ppx_eliom_client -server-cmo other.cmo --impl injection.eliom
  File "other.cmo", line 1:
  Error: Eliom: Error while loading types: no debugging information
  [1]

A cmo of another file, compiled with debugging information, has no type for the
injections and client values:

  $ ocamlc -g -c other.ml
  $ ppx_eliom_client -server-cmo other.cmo --impl injection.eliom |
  >   grep -Eo 'Error: [^"]*'
  Error: Inferred type of injected ident not found. You need to regenerate other.cmo.

  $ ppx_eliom_client -server-cmo other.cmo --impl fragment.eliom
  Fatal error: exception Failure("find_fragment: unexpected type: [%ocaml.error\n                                  \"Error: Inferred type of client value not found. You need to regenerate other.cmo.\"]")
  [2]
