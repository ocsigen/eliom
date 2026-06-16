
# Module type `Eliom_service_sigs.TYPES`


### Auxiliary service-related types

```ocaml
type get = 
  | Get_method
```
```ocaml
type put = 
  | Put_method
```
```ocaml
type post = 
  | Post_method
```
```ocaml
type delete = 
  | Delete_method
```
```ocaml
type co = 
  | Co
```
```ocaml
type non_co = 
  | Non_co
```
```ocaml
type ext = 
  | Ext
```
```ocaml
type non_ext = 
  | Non_ext
```
```ocaml
type http = 
  | Http_ret
```
```ocaml
type 'a ocaml = 
  | Ocaml of 'a
```
```ocaml
type non_ocaml = 
  | Non_ocaml
```
```ocaml
type reg = 
  | Reg
```
```ocaml
type non_reg = 
  | Non_reg
```
```ocaml
type ('get, 'tipo, 'gn) params = ('get, 'tipo, 'gn) Eliom_parameter.params_type constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]
```

### Method specification

```ocaml
type ('m, _, _, _, _, _, _) meth = 
  | Get : ('gp, 'tipo, 'gn) params -> (get, 'gp, 'gn, unit, unit, 'tipo, unit) meth
  | Post : ('gp, 'tipo, 'gn) params
    * ('pp, [ `WithoutSuffix ], 'pn) params -> (post,
                                                 'gp,
                                                 'gn,
                                                 'pp,
                                                 'pn,
                                                 'tipo,
                                                 'gp)
                                                 meth
  | Put : ('gp, 'tipo, 'gn) params -> (put,
                                      'gp,
                                      'gn,
                                      Eliom_parameter.raw_post_data,
                                      Eliom_parameter.no_param_name,
                                      'tipo,
                                      unit)
                                      meth
  | Delete : ('gp, 'tipo, 'gn) params -> (delete,
                                         'gp,
                                         'gn,
                                         Eliom_parameter.raw_post_data,
                                         Eliom_parameter.no_param_name,
                                         'tipo,
                                         unit)
                                         meth
```
**Method specification datatype**

An Eliom service (see [`Eliom_service_sigs.S.t`](./Eliom_service_sigs-module-type-S.md#type-t)) can respond to one of the following HTTP methods:

- GET (`Get g`)
- POST (`Post (g, p)`)
- PUT (`Put g`)
- DELETE (`Delete g`)
In all cases, the service parameters need to be provided (see [`Eliom_parameter_sigs.S`](./Eliom_parameter_sigs-module-type-S.md)). POST (`Post (g, p)`) services accept both GET (`g`) and POST (`p`) parameters. For the other methods, only GET (`g`) parameters apply.

The type parameters are used to impose various type constraints, and are not necessarily of interest to the programmer. Their technical meaning is as follows.

- 0-th param : method
- params 1-4 : GET and POST parameter types and names
- param 5 : suffix parameters permitted or not
- param 6 : non-unit only for the `Post (g, p)` case when `g` is not unit ; used to force unit GET parameters when needed
```ocaml
type 'm which_meth = 
  | Get' : get which_meth
  | Post' : post which_meth
  | Put' : put which_meth
  | Delete' : delete which_meth
```
Like [`meth`](./#type-meth) but without the parameters
