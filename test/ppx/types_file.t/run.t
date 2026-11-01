The client pass reads the types inferred on the server from the file given by
`-type`. eliomc builds this file with `ocamlc -i` on the output of the type
pass, where weak type variables `'_a` become `'eliom_inferred_type__a`.

The client uses these types for the injections, the sent values and the client
values. Each type variable is renamed, and a warning marks the injections and
sent values whose type has one. The client value of `missing` has no type in
the file. The output is put on one line, to check parts of it independently of
the layout:

  $ ppx_eliom_client -type types.type_mli --impl types.eliom | tr -s ' \n' '  ' |
  >   grep -Eo '"N63mRt1" : [a-z]+|get_escaped_value _eliom_escaped_ident_1 : .* list|Error: [^"]*'
  "N63mRt1" : int
  get_escaped_value _eliom_escaped_ident_1 : (('an_eliom_inferred_type__weak1_1) [@ocaml.ppwarning "The type of this injected value contains a type variable that could be wrongly inferred."]) list
  Error: Inferred type of client value not found. You need to regenerate types.type_mli.

The type of a client value must not be a type variable, unless it is unsafe:

  $ ppx_eliom_client -type poly.type_mli --impl poly.eliom
  File "poly.eliom", line 1, characters 21-41:
  1 | let%server poly () = [%client raise Exit]
                           ^^^^^^^^^^^^^^^^^^^^
  Error: The types of client values must be monomorphic from its usage or from its type annotation
  [1]
  $ ppx_eliom_client -type unsafe.type_mli --impl unsafe.eliom |
  >   grep -o "'[a-z_0-9]*eliom_inferred_type[a-z_0-9]*"
  'an_eliom_inferred_type__weak1_1

A missing type file is reported:

  $ ppx_eliom_client -type missing.type_mli --impl types.eliom
  File "missing.type_mli", line 1:
  Error: Eliom: Error while loading types: missing.type_mli: No such file or directory
  [1]
