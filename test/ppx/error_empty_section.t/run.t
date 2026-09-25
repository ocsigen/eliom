  $ ppx_eliom_server --impl test.eliom
  let __eliom__compilation_unit_id__e2YuTg = "e2YuTg"
  let () = Eliom.Syntax.set_global true
  [%%ocaml.error "Empty [%%shared] extension. Did you mean [%%shared.start] ?"]
  let () = Eliom.Syntax.set_global false
