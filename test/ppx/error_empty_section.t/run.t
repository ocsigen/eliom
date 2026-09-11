  $ ppx_eliom_server --impl test.ml
  let __eliom__compilation_unit_id__nXNiaY = "nXNiaY"
  let () = Eliom_syntax.set_global true
  [%%ocaml.error "Empty [%%shared] extension. Did you mean [%%shared.start] ?"]
  let () = Eliom_syntax.set_global false
