
# Module `Eliom.Service_base`

```ocaml
val __eliom__compilation_unit_id__2xPKAs : string
```
```ocaml
module Types : Service_sigs.TYPES
```

### Auxiliary service-related types

```ocaml
type get = Types.get = 
  | Get_method
```
```ocaml
type put = Types.put = 
  | Put_method
```
```ocaml
type post = Types.post = 
  | Post_method
```
```ocaml
type delete = Types.delete = 
  | Delete_method
```
```ocaml
type co = Types.co = 
  | Co
```
```ocaml
type non_co = Types.non_co = 
  | Non_co
```
```ocaml
type ext = Types.ext = 
  | Ext
```
```ocaml
type non_ext = Types.non_ext = 
  | Non_ext
```
```ocaml
type http = Types.http = 
  | Http_ret
```
```ocaml
type 'a ocaml = 'a Types.ocaml = 
  | Ocaml of 'a
```
```ocaml
type non_ocaml = Types.non_ocaml = 
  | Non_ocaml
```
```ocaml
type reg = Types.reg = 
  | Reg
```
```ocaml
type non_reg = Types.non_reg = 
  | Non_reg
```
```ocaml
type ('get, 'tipo, 'gn) params = ('get, 'tipo, 'gn) Parameter.params_type constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]
```

### Method specification

```ocaml
type ('m, _, _, _, _, _, _) meth = ('m, _, _, _, _, _, _) Types.meth = 
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
                                      Parameter.raw_post_data,
                                      Parameter.no_param_name,
                                      'tipo,
                                      unit)
                                      meth
  | Delete : ('gp, 'tipo, 'gn) params -> (delete,
                                         'gp,
                                         'gn,
                                         Parameter.raw_post_data,
                                         Parameter.no_param_name,
                                         'tipo,
                                         unit)
                                         meth
```
**Method specification datatype**

An Eliom service (see [`Service_sigs.S.t`](./Eliom-Service_sigs-module-type-S.md#type-t)) can respond to one of the following HTTP methods:

- GET (`Get g`)
- POST (`Post (g, p)`)
- PUT (`Put g`)
- DELETE (`Delete g`)
In all cases, the service parameters need to be provided (see [`Parameter_sigs.S`](./Eliom-Parameter_sigs-module-type-S.md)). POST (`Post (g, p)`) services accept both GET (`g`) and POST (`p`) parameters. For the other methods, only GET (`g`) parameters apply.

The type parameters are used to impose various type constraints, and are not necessarily of interest to the programmer. Their technical meaning is as follows.

- 0-th param : method
- params 1-4 : GET and POST parameter types and names
- param 5 : suffix parameters permitted or not
- param 6 : non-unit only for the `Post (g, p)` case when `g` is not unit ; used to force unit GET parameters when needed
```ocaml
type 'm which_meth = 'm Types.which_meth = 
  | Get' : get which_meth
  | Post' : post which_meth
  | Put' : put which_meth
  | Delete' : delete which_meth
```
Like [`meth`](./#val-meth) but without the parameters

```ocaml
module Url = Lib.Url
```
```ocaml
type suff = [ 
  | `WithSuffix
  | `WithoutSuffix
 ]
```
```ocaml
val params_of_meth : 
  'm 'gp 'gn 'pp 'pn 'x. ('m,
                           'gp,
                           'gn,
                           'pp,
                           'pn,
                           [< `WithSuffix | `WithoutSuffix ] as 'a,
                           'x)
                           meth ->
  ('gp, 'a, 'gn) params * ('pp, [ `WithoutSuffix ], 'pn) params
```
```ocaml
val which_meth_internal : 
  'm 'gp 'gn 'pp 'pn 'tipo 'x. ('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'x) meth ->
  'm which_meth
```
```ocaml
val is_post : 
  'm 'gp 'gn 'pp 'pn 'x. ('m,
                           'gp,
                           'gn,
                           'pp,
                           'pn,
                           [< `WithSuffix | `WithoutSuffix ],
                           'x)
                           meth ->
  bool
```
```ocaml
val is_post' : 'm. 'm which_meth -> bool
```
```ocaml
type reload_fun = 
  | Rf_keep
  | Rf_client_fun
```
```ocaml
type att = {
  prefix : string;
  subpath : Url.path;
  fullpath : Url.path option ref;
  get_name : Common.att_key_serv;
  post_name : Common.att_key_serv;
  redirect_suffix : bool;
  priority : int;
}
```
```ocaml
type non_att = {
  na_name : Common.na_key_serv;
  keep_get_na_params : bool;
}
```
```ocaml
type 'a attached_info = 
  | Attached : att -> att attached_info
  | Nonattached : non_att -> non_att attached_info
```
```ocaml
type send_appl_content = 
  | XNever
  | XAlways
  | XSame_appl of string * string option (* Whether the service is capable to send application content or not. (application content has type Service.eliom_appl_answer: content of the application container, or xhr redirection ...). A link towards a service with send_appl_content = XNever will always answer a regular http frame (this will stop the application if used in a regular link or form, but not with XHR). XAlways means "for all applications" (like redirections/actions). XSame_appl means "only for this application". If there is a client side application, and the service has XAlways or XSame_appl when it is the same application, then the link (or form or change_page) will expect application content. *)
```
```ocaml
type service_kind = [ 
  | `Service
  | `AttachedCoservice
  | `NonattachedCoservice
  | `External
 ]
```
```ocaml
type ('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg, +'tipo, 'getnames, 'postnames, 'rt)
  t =
  {
  pre_applied_parameters : (string * Mod_parameters.param) list
                           Lib.String.Table.t
                         * (string * Mod_parameters.param) list;
  get_params_type : ('get, 'tipo, 'getnames) Parameter.params_type;
  post_params_type : ('post, [ `WithoutSuffix ], 'postnames)
                     Parameter.params_type;
  max_use : int option;
  timeout : float option;
  meth : 'meth which_meth;
  kind : service_kind;
  info : 'attached attached_info;
  https : bool;
  keep_nl_params : [ `All | `Persistent | `None ];
  mutable send_appl_content : send_appl_content;
  mutable client_fun : ('get -> 'post -> result Lwt.t) option ref Client_value.t
                       option;
  mutable reload_fun : reload_fun;
  service_mark : (unit,
                 unit,
                 'meth,
                 'attached,
                 'co,
                 'ext,
                 'reg,
                 suff,
                 unit,
                 unit,
                 unit)
                 t
                 Common.wrapper;
} constraint 'tipo = [< suff ]
```
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
val pre_wrap : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ] as 'h, 'i, 'j, 'k) t ->
  ('a, 'b, 'c, 'd, 'l, 'm, 'n, 'h, 'i, 'j, 'o) t
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
val service_mark : unit -> unit
```
```ocaml
val info : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  'd attached_info
```
```ocaml
val pre_applied_parameters : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  (string * Mod_parameters.param) list Lib.String.Table.t
  * (string * Mod_parameters.param) list
```
```ocaml
val get_params_type : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ] as 'h, 'i, 'j, 'k) t ->
  ('a, 'h, 'i) Parameter.params_type
```
```ocaml
val post_params_type : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  ('b, [ `WithoutSuffix ], 'i) Parameter.params_type
```
```ocaml
val prefix : att -> string
```
```ocaml
val sub_path : att -> Url.path
```
```ocaml
val redirect_suffix : att -> bool
```
```ocaml
val full_path : att -> Url.path
```
```ocaml
val get_name : att -> Common.att_key_serv
```
```ocaml
val post_name : att -> Common.att_key_serv
```
```ocaml
val na_name : non_att -> Common.na_key_serv
```
```ocaml
val na_keep_get_na_params : non_att -> bool
```
```ocaml
val max_use : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  int option
```
```ocaml
val timeout : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  float option
```
```ocaml
val https : ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t -> bool
```
```ocaml
val priority : att -> int
```
```ocaml
val _eliom_fragment_2xPKAs1 : 
  ('eliom_inferred_type_get_1 ->
    'eliom_inferred_type_post_0 ->
    result Lwt.t)
    Client_value.t ->
  ('eliom_inferred_type_get_1 ->
    'eliom_inferred_type_post_0 ->
    result Lwt.t)
    option
    ref
```
```ocaml
val internal_set_client_fun : 
  service:('get, 'post, 'a, 'b, 'c, 'd, 'e, [< suff ], 'f, 'g, 'h) t ->
  ('get -> 'post -> result Lwt.t) Client_value.t ->
  unit
```
```ocaml
val is_external : ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t -> bool
```
```ocaml
val default_priority : int
```
```ocaml
val meth : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  'c which_meth
```
```ocaml
val change_get_num : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ] as 'h, 'i, 'j, 'k) t ->
  att ->
  Common.att_key_serv ->
  ('a, 'b, 'c, att, 'l, 'm, 'n, 'h, 'i, 'j, 'o) t
```
```ocaml
val static_dir_ : 
  ?https:bool ->
  unit ->
  (string list,
    unit,
    get,
    att,
    'a,
    'b,
    'c,
    [ `WithSuffix ],
    [ `One of string list ] Parameter.param_name,
    unit,
    'd)
    t
```
Static directories \*

```ocaml
val static_dir : 
  unit ->
  (string list,
    unit,
    get,
    att,
    'a,
    'b,
    'c,
    [ `WithSuffix ],
    [ `One of string list ] Parameter.param_name,
    unit,
    'd)
    t
```
```ocaml
val https_static_dir : 
  unit ->
  (string list,
    unit,
    get,
    att,
    'a,
    'b,
    'c,
    [ `WithSuffix ],
    [ `One of string list ] Parameter.param_name,
    unit,
    'd)
    t
```
```ocaml
val get_static_dir_ : 
  ?https:bool ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  get_params:('a, [ `WithoutSuffix ], 'b) Parameter.params_type ->
  unit ->
  (string list * 'a,
    unit,
    get,
    att,
    'c,
    'd,
    'e,
    [ `WithSuffix ],
    [ `One of string list ] Parameter.param_name * 'b,
    unit,
    'f)
    t
```
```ocaml
val static_dir_with_params : 
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  get_params:('a, [ `WithoutSuffix ], 'b) Parameter.params_type ->
  unit ->
  (string list * 'a,
    unit,
    get,
    att,
    'c,
    'd,
    'e,
    [ `WithSuffix ],
    [ `One of string list ] Parameter.param_name * 'b,
    unit,
    'f)
    t
```
```ocaml
val https_static_dir_with_params : 
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  get_params:('a, [ `WithoutSuffix ], 'b) Parameter.params_type ->
  unit ->
  (string list * 'a,
    unit,
    get,
    att,
    'c,
    'd,
    'e,
    [ `WithSuffix ],
    [ `One of string list ] Parameter.param_name * 'b,
    unit,
    'f)
    t
```
```ocaml
val send_appl_content : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  send_appl_content
```
```ocaml
val set_send_appl_content : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  send_appl_content ->
  unit
```
```ocaml
val append_suffix : 'a list -> 'a list -> 'a list
```
```ocaml
val _eliom_fragment_2xPKAs2 : 
  (('eliom_inferred_type_a_8,
     'eliom_inferred_type_a_19,
     'eliom_inferred_type_c_10,
     att,
     'eliom_inferred_type_d_11,
     'eliom_inferred_type_e_12,
     'eliom_inferred_type_f_13,
     [< suff ],
     'eliom_inferred_type_g_14,
     'eliom_inferred_type_h_15,
     'eliom_inferred_type_i_16)
     t
   * 'eliom_inferred_type_a_8) ->
  (unit -> 'eliom_inferred_type_a_19 -> result Lwt.t) option ref
```
```ocaml
val preapply : 
  service:('a, 'b, 'c, att, 'd, 'e, 'f, [< suff ], 'g, 'h, 'i) t ->
  'a ->
  (unit, 'b, 'c, att, 'j, 'k, 'l, [ `WithoutSuffix ], unit, 'h, 'm) t
```
```ocaml
val reload_action_aux : 
  bool ->
  (unit, unit, get, non_att, 'a, 'b, 'c, [ `WithoutSuffix ], unit, unit, 'd) t
```
```ocaml
val reload_action : 
  (unit, unit, get, non_att, 'a, 'b, 'c, [ `WithoutSuffix ], unit, unit, 'd) t
```
```ocaml
val reload_action_https : 
  (unit, unit, get, non_att, 'a, 'b, 'c, [ `WithoutSuffix ], unit, unit, 'd) t
```
```ocaml
val reload_action_hidden_aux : 
  bool ->
  (unit, unit, get, non_att, 'a, 'b, 'c, [ `WithoutSuffix ], unit, unit, 'd) t
```
```ocaml
val reload_action_hidden : 
  (unit, unit, get, non_att, 'a, 'b, 'c, [ `WithoutSuffix ], unit, unit, 'd) t
```
```ocaml
val reload_action_https_hidden : 
  (unit, unit, get, non_att, 'a, 'b, 'c, [ `WithoutSuffix ], unit, unit, 'd) t
```
```ocaml
val _eliom_fragment_2xPKAs3 : 
  ('eliom_inferred_type_a_35,
    'eliom_inferred_type_c_34,
    'eliom_inferred_type_c_23,
    'eliom_inferred_type_d_24,
    'eliom_inferred_type_e_25,
    'eliom_inferred_type_f_26,
    'eliom_inferred_type_g_27,
    [< suff ],
    'eliom_inferred_type_h_28,
    'eliom_inferred_type_i_29,
    'eliom_inferred_type_j_30)
    t ->
  (('eliom_inferred_type_a_35 * 'eliom_inferred_type_b_36) ->
    'eliom_inferred_type_c_34 ->
    result Lwt.t)
    option
    ref
```
```ocaml
val add_non_localized_get_parameters : 
  params:('a, [ `WithoutSuffix ], 'b) Parameter.non_localized_params ->
  service:('c, 'd, 'e, 'f, 'g, 'h, 'i, [< suff ] as 'j, 'k, 'l, 'm) t ->
  ('c * 'a, 'd, 'e, 'f, 'n, 'o, 'p, 'j, 'k * 'b, 'l, 'q) t
```
```ocaml
val _eliom_fragment_2xPKAs4 : 
  ('eliom_inferred_type_a_55,
    'eliom_inferred_type_b_53,
    'eliom_inferred_type_c_42,
    'eliom_inferred_type_d_43,
    'eliom_inferred_type_e_44,
    'eliom_inferred_type_f_45,
    'eliom_inferred_type_g_46,
    [< suff ],
    'eliom_inferred_type_h_47,
    'eliom_inferred_type_i_48,
    'eliom_inferred_type_j_49)
    t ->
  ('eliom_inferred_type_a_55 ->
    ('eliom_inferred_type_b_53 * 'eliom_inferred_type_c_54) ->
    result Lwt.t)
    option
    ref
```
```ocaml
val add_non_localized_post_parameters : 
  params:('a, [ `WithoutSuffix ], 'b) Parameter.non_localized_params ->
  service:('c, 'd, 'e, 'f, 'g, 'h, 'i, [< suff ] as 'j, 'k, 'l, 'm) t ->
  ('c, 'd * 'a, 'e, 'f, 'n, 'o, 'p, 'j, 'k, 'l * 'b, 'q) t
```
```ocaml
val keep_nl_params : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  [ `All | `None | `Persistent ]
```
```ocaml
val untype : 
  ('get,
    'post,
    'meth,
    'attached,
    'co,
    'ext,
    'tipo,
    [< suff ] as 'a,
    'postnames,
    'register,
    'b)
    t ->
  ('get,
    'post,
    'meth,
    'attached,
    'co,
    'ext,
    'tipo,
    'a,
    'postnames,
    'register,
    'c)
    t
```
```ocaml
type (_, _, _) path_option = 
  | Path : Lib.Url.path -> (att, non_co, _) path_option
  | No_path : (non_att, co, unit) path_option
```
```ocaml
val eliom_appl_answer_content_type : string
```
```ocaml
val uniqueid : unit -> int
```
```ocaml
val new_state : unit -> string
```
```ocaml
val default_csrf_scope : [< Common.user_scope ] option -> Common.user_scope
```
```ocaml
exception Unreachable_exn
```
```ocaml
val attached_info : 
  ('a, 'b, 'c, att, 'd, 'e, 'f, [< suff ], 'g, 'h, 'i) t ->
  att
```
```ocaml
val non_attached_info : 
  ('a, 'b, 'c, non_att, 'd, 'e, 'f, [< suff ], 'g, 'h, 'i) t ->
  non_att
```
```ocaml
val no_client_fun : unit -> 'a option ref Client_value.t option
```
```ocaml
val main_service : 
  https:bool ->
  prefix:string ->
  path:Url.path ->
  ?force_site_dir:string list ->
  kind:service_kind ->
  meth:'a which_meth ->
  ?redirect_suffix:bool ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?priority:int ->
  get_params:('b, [< suff ] as 'c, 'd) Parameter.params_type ->
  post_params:('e, [ `WithoutSuffix ], 'f) Parameter.params_type ->
  reload_fun:reload_fun ->
  unit ->
  ('b, 'e, 'a, att, 'g, 'h, 'i, 'c, 'd, 'f, 'j) t
```
Create a main service (not a coservice), internal or external

```ocaml
val extern : 
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  prefix:string ->
  path:Url.path ->
  meth:('a, 'b, 'c, 'd, 'e, [< suff ] as 'f, 'g) meth ->
  unit ->
  ('b, 'd, 'a, att, 'h, 'i, 'j, 'f, 'c, 'e, 'k) t
```
```ocaml
val which_meth : 
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, [< suff ], 'h, 'i, 'j) t ->
  'c which_meth
```
```ocaml
val which_meth_untyped : 
  ('a, 'b, 'm, 'c, 'd, 'e, 'f, [< suff ], 'g, 'h, 'i) t ->
  [> `Delete | `Get | `Post | `Put ]
```