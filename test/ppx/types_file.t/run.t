The client pass reads the types inferred on the server from the file given by
`-type`. eliomc builds this file with `ocamlc -i` on the output of the type
pass, where weak type variables `'_a` become `'eliom_inferred_type__a`.

The client uses these types for the injections, the sent values and the client
values. Each type variable is renamed, and a warning marks the injections and
sent values whose type has one. The client value of `missing` has no type in
the file:

  $ ppx_eliom_client -type types.type_mli --impl types.eliom
  let __eliom__compilation_unit_id__N63mRt = "N63mRt"
  let () =
    Eliom.Client_core.Syntax_helpers.open_client_section
      __eliom__compilation_unit_id__N63mRt
  let y =
    (Eliom.Client_core.Syntax_helpers.get_injection ?ident:(Some "x")
       ~pos:({
               Lexing.pos_fname = "types.eliom";
               Lexing.pos_lnum = 2;
               Lexing.pos_bol = 17;
               Lexing.pos_cnum = 34
             },
              {
                Lexing.pos_fname = "types.eliom";
                Lexing.pos_lnum = 2;
                Lexing.pos_bol = 17;
                Lexing.pos_cnum = 35
              }) "N63mRt1" : int)
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "N63mRt1"
      (fun _eliom_escaped_ident_1 : int->
         List.length
           (Eliom.Client_core.Syntax_helpers.get_escaped_value
              _eliom_escaped_ident_1 : (('an_eliom_inferred_type__weak1_1)
                                         [@ocaml.ppwarning
                                           "The type of this injected value contains a type variable that could be wrongly inferred."])
                                         list))
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "N63mRt2"
      (fun () :
         [%ocaml.error
           "Error: Inferred type of client value not found. You need to regenerate types.type_mli."]->
         0)

The type of a client value must not be a type variable, unless it is unsafe:

  $ ppx_eliom_client -type poly.type_mli --impl poly.eliom
  File "poly.eliom", line 1, characters 21-41:
  1 | let%server poly () = [%client raise Exit]
                           ^^^^^^^^^^^^^^^^^^^^
  Error: The types of client values must be monomorphic from its usage or from its type annotation
  [1]
  $ ppx_eliom_client -type unsafe.type_mli --impl unsafe.eliom
  let __eliom__compilation_unit_id__'3efCI = "'3efCI"
  ;;Eliom.Client_core.Syntax_helpers.register_client_closure "'3efCI1"
      (fun () : 'an_eliom_inferred_type__weak1_1-> raise Exit)

A missing type file is reported:

  $ ppx_eliom_client -type missing.type_mli --impl types.eliom
  File "missing.type_mli", line 1:
  Error: Eliom: Error while loading types: missing.type_mli: No such file or directory
  [1]
