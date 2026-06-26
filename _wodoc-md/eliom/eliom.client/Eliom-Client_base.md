
# Module `Eliom.Client_base`

```ocaml
type ('a, 'b) server_function_service =
  (unit,
    'a,
    Service.post,
    Service.non_att,
    Service.co,
    Service.non_ext,
    Service.reg,
    [ `WithoutSuffix ],
    unit,
    [ `One of 'a Parameter.ocaml ] Parameter.param_name,
    'b Service.ocaml)
    Service.t
```