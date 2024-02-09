type ('a, 'b) server_function_service =
  ( unit
    , 'a
    , Eliom_service.post
    , Eliom_service.non_att
    , Eliom_service.co
    , Eliom_service.non_ext
    , Eliom_service.reg
    , [`WithoutSuffix]
    , unit
    , [`One of 'a Eliom_parameter.ocaml] Eliom_parameter.param_name
    , 'b Eliom_service.ocaml )
    Eliom_service.t
