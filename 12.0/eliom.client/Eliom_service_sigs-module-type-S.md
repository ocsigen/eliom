
# Module type `Eliom_service_sigs.S`


### Services

See `Eliom_service.create` (on the server) for how to create services.


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

An Eliom service (see [`Eliom_service_sigs.S.t`](./#type-t)) can respond to one of the following HTTP methods:

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


#### Auxiliary types

```ocaml
type att
```
```ocaml
type non_att
```
```ocaml
type 'a attached_info = 
  | Attached : att -> att attached_info
  | Nonattached : non_att -> non_att attached_info
```
```ocaml
type ('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg, +'tipo, 'gn, 'pn, 'ret) t constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]
```
**Type of services**

For a service `('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg, 'tipo, 'gn, 'pn, 'ret) t`:

- `'get` is the type of GET parameters expected by the service.
- `'post` is the type of POST parameters expected by the service.
- `'meth` the HTTP method
- `'attached` attached or non-attached
- `'co` co-service or regular service
- `'ext` external or internal
- `'reg`: possible to register a handler on this service
- `'tipo` the type parameter of subtype `suff` states the kind of parameters it uses: suffix or not.
- `'gn` is the type of GET parameters names. See [`Eliom_parameter.param_name`](./Eliom_parameter.md#type-param_name) and form generation functions (e. g. `Eliom_content.Html.D.get_form` ).
- `'pn` is the type of POST parameters names. See [`Eliom_parameter.param_name`](./Eliom_parameter.md#type-param_name) and form generation functions (e. g. `Eliom_content.Html.D.post_form` ).
- ` 'ret` is an information on what the service returns. See [`Eliom_registration.kind`](./Eliom_registration.md#type-kind).
```ocaml
and result = 
  | No_contents
  | Dom of Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
  | Redirect : (unit,
               unit,
               get,
               _,
               _,
               _,
               _,
               [ `WithoutSuffix ],
               unit,
               unit,
               non_ocaml)
               t -> result
  | Reload_action of {
    hidden : bool;
    https : bool;
  }
```
```ocaml
type unit_service =
  (unit,
    unit,
    get,
    att,
    non_co,
    non_ext,
    non_reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    non_ocaml)
    t
```
```ocaml
type (_, _, _) path_option = 
  | Path : Eliom_lib.Url.path -> (att, non_co, _) path_option
  | No_path : (non_att, co, unit) path_option
```
**Optional service path**


#### Predefined services


##### Reload actions

```ocaml
val reload_action : 
  (unit,
    unit,
    get,
    non_att,
    co,
    non_ext,
    non_reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    non_ocaml)
    t
```
The service `reload_action` is a predefined non-attached action with special behaviour: it has no parameter at all, even non-attached parameters. Use it if you want to make a link to the current page without non-attached parameters. It is almost equivalent to a POST non-attached service without POST parameters, on which you register an action that does nothing, but you can use it with \<a\> links, not only forms. It does not keep non attached GET parameters.

```ocaml
val reload_action_https : 
  (unit,
    unit,
    get,
    non_att,
    co,
    non_ext,
    non_reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    non_ocaml)
    t
```
Like [`reload_action`](./#val-reload_action), but forces HTTPS

```ocaml
val reload_action_hidden : 
  (unit,
    unit,
    get,
    non_att,
    co,
    non_ext,
    non_reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    non_ocaml)
    t
```
Like [`reload_action`](./#val-reload_action), but keeps non-attached GET parameters.

```ocaml
val reload_action_https_hidden : 
  (unit,
    unit,
    get,
    non_att,
    co,
    non_ext,
    non_reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    non_ocaml)
    t
```
Like [`reload_action_hidden`](./#val-reload_action_hidden), but forces HTTPS


#### Static files

```ocaml
val static_dir : 
  unit ->
  (string list,
    unit,
    get,
    att,
    non_co,
    non_ext,
    non_reg,
    [ `WithSuffix ],
    [ `One of string list ] Eliom_parameter.param_name,
    unit,
    non_ocaml)
    t
```
The predefined service `static_dir` allows one to create links to static files. This service takes the name of a static file as a parameter (a string list, slash separated). The actual directory in filesystem where static pages will be found must be set up in the configuration file with the staticmod extension.

```ocaml
val https_static_dir : 
  unit ->
  (string list,
    unit,
    get,
    att,
    non_co,
    non_ext,
    non_reg,
    [ `WithSuffix ],
    [ `One of string list ] Eliom_parameter.param_name,
    unit,
    non_ocaml)
    t
```
Like [`static_dir`](./#val-static_dir), but forces HTTPS link

```ocaml
val static_dir_with_params : 
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [ `WithoutSuffix ], 'an) Eliom_parameter.params_type ->
  unit ->
  (string list * 'a,
    unit,
    get,
    att,
    non_co,
    non_ext,
    non_reg,
    [ `WithSuffix ],
    [ `One of string list ] Eliom_parameter.param_name * 'an,
    unit,
    non_ocaml)
    t
```
Like [`static_dir`](./#val-static_dir), but allows one to put GET parameters

```ocaml
val https_static_dir_with_params : 
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [ `WithoutSuffix ], 'an) Eliom_parameter.params_type ->
  unit ->
  (string list * 'a,
    unit,
    get,
    att,
    non_co,
    non_ext,
    non_reg,
    [ `WithSuffix ],
    [ `One of string list ] Eliom_parameter.param_name * 'an,
    unit,
    non_ocaml)
    t
```
Like [`static_dir_with_params`](./#val-static_dir_with_params), but forces HTTPS link


#### Miscellaneous

```ocaml
val preapply : 
  service:('a, 'b, 'meth, att, 'co, 'ext, 'reg, _, 'e, 'f, 'return) t ->
  'a ->
  (unit,
    'b,
    'meth,
    att,
    'co,
    'ext,
    non_reg,
    [ `WithoutSuffix ],
    unit,
    'f,
    'return)
    t
```
The function `preapply ~service parameters` creates a new service by preapplying `service` to the GET `parameters`. It is not possible to register a handler on an preapplied service; preapplied services may be used in links or as fallbacks.

```ocaml
val add_non_localized_get_parameters : 
  params:('p, [ `WithoutSuffix ], 'pn) Eliom_parameter.non_localized_params ->
  service:('a, 'b, 'meth, 'attach, 'co, 'ext, 'reg, 'd, 'e, 'f, 'return) t ->
  ('a * 'p, 'b, 'meth, 'attach, 'co, 'ext, 'reg, 'd, 'e * 'pn, 'f, 'return) t
```
The function `add_non_localized_get_parameters ~params ~service` Adds non localized GET parameters `params` to `service`. See the Eliom manual for more information about [non localized parameters](./../server-params.md#nonlocalizedparameters).

```ocaml
val add_non_localized_post_parameters : 
  params:('p, [ `WithoutSuffix ], 'pn) Eliom_parameter.non_localized_params ->
  service:('a, 'b, 'meth, 'attach, 'co, 'ext, 'g, 'd, 'e, 'f, 'return) t ->
  ('a, 'b * 'p, 'meth, 'attach, 'co, 'ext, 'g, 'd, 'e, 'f * 'pn, 'return) t
```
Like [`add_non_localized_get_parameters`](./#val-add_non_localized_get_parameters) but with POST parameters.

```ocaml
val extern : 
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  prefix:string ->
  path:Eliom_lib.Url.path ->
  meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, _) meth ->
  unit ->
  ('gp, 'pp, 'm, att, non_co, ext, non_reg, 'tipo, 'gn, 'pn, non_ocaml) t
```
`extern ~prefix ~path ~meth ()` creates an external service, i.e., a service implemented by a remote server (not necessarily running Ocsigen/Eliom).
