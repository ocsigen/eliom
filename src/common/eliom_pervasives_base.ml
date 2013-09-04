
type ('a, 'b) server_function_service =
  (unit, 'a,
   [ `Nonattached of [ `Post] Eliom_service.na_s ], [ `WithoutSuffix ],
   unit, [ `One of 'a Eliom_parameter.caml ] Eliom_parameter.param_name,
   [ `Registrable ],
   'b Eliom_service.caml_service)
  Eliom_service.service
