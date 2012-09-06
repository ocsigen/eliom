
type ('a, 'b) server_function_service =
  (unit, string,
   [ `Nonattached of [ `Post] Eliom_service.na_s ], [ `WithoutSuffix ],
   unit, [ `One of string ] Eliom_parameter.param_name,
   [ `Registrable ], string Eliom_parameter.caml)
  Eliom_service.service
