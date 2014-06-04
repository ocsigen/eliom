
type ('a, 'b) server_function_service =
  (unit, 'a,
   [ `Nonattached of [ `Post] Eliom_service.na_s ], [ `WithoutSuffix ],
   unit, [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name,
   [ `Registrable ],
   'b Eliom_service.ocaml_service)
  Eliom_service.service
