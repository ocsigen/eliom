
type ('a, 'b) server_function_service =
  (unit, 'a, [`Post], Eliom_service.non_attached_kind, [`NonattachedCoservice], [ `WithoutSuffix ],
   unit, [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name,
   [ `Registrable ],
   'b Eliom_service.ocaml_service)
  Eliom_service.service
