# Module `Eliom_parameter_base`

```ocaml
val section : Logs.src
```
```ocaml
type params = (string * Eliommod_parameters.param) list
```
```ocaml
type params' = (string * string) list
```
```ocaml
type +'a param_name = string
```
Type of names in a form

```ocaml
type no_param_name
```
empty type used when it is not possible to use the parameter in a form

```ocaml
type ('a, 'b) binsum = 
  | Inj1 of 'a
  | Inj2 of 'b
```
```ocaml
type 'an listnames = {
  it : 'el 'a. ('an -> 'el -> 'a -> 'a) -> 'el list -> 'a -> 'a;
}
```
```ocaml
type coordinates = {
  abscissa : int;
  ordinate : int;
}
```
```ocaml
type 'a setoneradio = [ 
  | `Set of 'a
  | `One of 'a
  | `Radio of 'a
 ]
```
```ocaml
type 'a oneradio = [ 
  | `One of 'a
  | `Radio of 'a
 ]
```
```ocaml
type 'a setone = [ 
  | `Set of 'a
  | `One of 'a
 ]
```
```ocaml
type 'a to_and_of = {
  of_string : string -> 'a;
  to_string : 'a -> string;
}
```
```ocaml
type _ atom = 
  | TFloat : float atom
  | TInt : int atom
  | TInt32 : int32 atom
  | TInt64 : int64 atom
  | TNativeint : nativeint atom
  | TBool : bool atom
  | TString : string atom
```
```ocaml
val string_of_atom : 'a atom -> 'a -> string
```
```ocaml
val atom_of_string : 'a atom -> string -> 'a
```
```ocaml
val to_from_of_atom : 'a atom -> 'a to_and_of
```
```ocaml
type 'a filter = ('a -> unit) option
```
```ocaml
type raw =
  ((string * string) * (string * string) list) option * Cohttp_lwt.Body.t
```
```ocaml
type 'a ocaml = string
```
```ocaml
type suff = [ 
  | `WithoutSuffix
  | `WithSuffix
  | `Endsuffix
 ]
```
```ocaml
type (_, _) params_type_ = 
  | TProd : (('a, 'an) params_type_ * ('b, 'bn) params_type_) -> ('a * 'b,
                                                                 'an * 'bn)
                                                                 params_type_
  | TOption : (('a, 'an) params_type_ * bool) -> ('a option, 'an) params_type_
  | TList : (string * ('a, 'an) params_type_) -> ('a list, 'an listnames)
                                                 params_type_
  | TSet : ('a, [ `One of 'an ] param_name) params_type_ -> ('a list,
                                                            [ `Set of 'an ]
                                                              param_name)
                                                            params_type_
  | TSum : (('a, 'an) params_type_ * ('b, 'bn) params_type_) -> (('a, 'b) binsum,
                                                                'an * 'bn)
                                                                params_type_
  | TAtom : (string * 'a atom) -> ('a, [ `One of 'a ] param_name) params_type_
  | TCoord : string -> (coordinates, [ `One of coordinates ] param_name)
                       params_type_
  | TFile : string -> (Eliom_lib.file_info,
                      [ `One of Eliom_lib.file_info ] param_name)
                      params_type_
  | TUserType : (string * 'a Eliom_common.To_and_of_shared.t) -> ('a,
                                                                 [ `One of 'a ]
                                                                   param_name)
                                                                 params_type_
  | TTypeFilter : (('a, 'an) params_type_ * 'a filter) -> ('a, 'an) params_type_
  | TESuffix : string -> (string list, [ `One of string list ] param_name)
                         params_type_
  | TESuffixs : string -> (string, [ `One of string ] param_name) params_type_
  | TESuffixu : (string * 'a Eliom_common.To_and_of_shared.t) -> ('a,
                                                                 [ `One of 'a ]
                                                                   param_name)
                                                                 params_type_
  | TSuffix : (bool * ('s, 'sn) params_type_) -> ('s, 'sn) params_type_
  | TUnit : (unit, unit) params_type_
  | TAny : ((string * string) list, unit) params_type_
  | TConst : string -> (unit, [ `One of unit ] param_name) params_type_
  | TNLParams : ('a, 'names) non_localized_params_ -> ('a, 'names) params_type_
  | TJson : (string * 'a Deriving_Json.t option) -> ('a,
                                                    [ `One of 'a ocaml ]
                                                      param_name)
                                                    params_type_
  | TRaw_post_data : (raw, no_param_name) params_type_
```
```ocaml
and ('a, 'names) non_localized_params_ = {
  name : string;
  persistent : bool;
  get : 'a option Polytables.key;
  post : 'a option Polytables.key;
  param : ('a, 'names) params_type_;
}
```
```ocaml
type ('a, +'suff, 'an) non_localized_params = ('a, 'an) non_localized_params_ constraint 'suff = [< suff ]
```
```ocaml
type ('a, +'suff, 'an) params_type = ('a, 'an) params_type_ constraint 'suff = [< suff ]
```
```ocaml
val int : string -> (int, [ `One of int ] param_name) params_type_
```
```ocaml
val int32 : string -> (int32, [ `One of int32 ] param_name) params_type_
```
```ocaml
val int64 : string -> (int64, [ `One of int64 ] param_name) params_type_
```
```ocaml
val float : string -> (float, [ `One of float ] param_name) params_type_
```
```ocaml
val bool : string -> (bool, [ `One of bool ] param_name) params_type_
```
```ocaml
val string : string -> (string, [ `One of string ] param_name) params_type_
```
```ocaml
val file : 
  string ->
  (Eliom_lib.file_info, [ `One of Eliom_lib.file_info ] param_name)
    params_type_
```
```ocaml
val unit : (unit, unit) params_type_
```
```ocaml
val coordinates : 
  string ->
  (coordinates, [ `One of coordinates ] param_name) params_type_
```
```ocaml
val type_checker : 
  ('a -> unit) ->
  ('a, 'b) params_type_ ->
  ('a, 'b) params_type_
```
```ocaml
val sum : 
  ('a, 'b) params_type_ ->
  ('c, 'd) params_type_ ->
  (('a, 'c) binsum, 'b * 'd) params_type_
```
```ocaml
val prod : 
  ('a, 'b) params_type_ ->
  ('c, 'd) params_type_ ->
  ('a * 'c, 'b * 'd) params_type_
```
```ocaml
val (**) : 
  ('a, 'b) params_type_ ->
  ('c, 'd) params_type_ ->
  ('a * 'c, 'b * 'd) params_type_
```
```ocaml
val opt : ('a, 'b) params_type_ -> ('a option, 'b) params_type_
```
```ocaml
val neopt : ('a, 'b) params_type_ -> ('a option, 'b) params_type_
```
```ocaml
val radio : 
  (string -> ('a, 'b) params_type_) ->
  string ->
  ('a option, 'b) params_type_
```
```ocaml
val list : 
  string ->
  ('a, 'b) params_type_ ->
  ('a list, 'b listnames) params_type_
```
```ocaml
val set : 
  (string -> ('a, [ `One of 'b ] param_name) params_type_) ->
  string ->
  ('a list, [ `Set of 'b ] param_name) params_type_
```
```ocaml
val any : ((string * string) list, unit) params_type_
```
```ocaml
val suffix_const : string -> (unit, [ `One of unit ] param_name) params_type_
```
```ocaml
val all_suffix : 
  string ->
  (string list, [ `One of string list ] param_name) params_type_
```
```ocaml
val all_suffix_string : 
  string ->
  (string, [ `One of string ] param_name) params_type_
```
```ocaml
val suffix : 
  ?redirect_if_not_suffix:bool ->
  ('a, 'b) params_type_ ->
  ('a, 'b) params_type_
```
```ocaml
val suffix_prod : 
  ?redirect_if_not_suffix:bool ->
  ('s, [< `Endsuffix | `WithoutSuffix ], 'sn) params_type ->
  ('a, [ `WithoutSuffix ], 'an) params_type ->
  ('s * 'a, [ `WithSuffix ], 'sn * 'an) params_type
```
```ocaml
val ocaml : 
  string ->
  'a Deriving_Json.t ->
  ('a, [ `One of 'a ocaml ] param_name) params_type_
```
```ocaml
val raw_post_data : (raw, no_param_name) params_type_
```
```ocaml
val make_list_suffix : int -> string
```
```ocaml
val make_suffix : 'a 'c. ('a, [< suff ], 'c) params_type -> 'a -> string list
```
```ocaml
val aux : 
  'a 'c. ('a, [< suff ], 'c) params_type ->
  string list option ->
  (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t ->
  'a ->
  string ->
  string ->
  (string * Eliommod_parameters.field) list ->
  string list option
  * (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t
  * (string * Eliommod_parameters.field) list
```
```ocaml
val construct_params_list_raw : 
  (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t ->
  ('a, [< suff ], 'b) params_type ->
  'a ->
  string list option
  * (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t
  * (string * Eliommod_parameters.field) list
```
```ocaml
val get_to_and_of : 'a 'c. ('a, [< suff ], 'c) params_type -> 'a to_and_of
```
Given a parameter type, get the two functions that converts from and to strings. You should only use this function on

- options ;
- basic types : int, int32, int64, float, string
- marshal
- unit
- string
- bool
```ocaml
val walk_parameter_tree : 
  'a 'c. string ->
  ('a, [< suff ], 'c) params_type ->
  'a to_and_of option
```
Walk the parameter tree to search for a parameter, given its name

```ocaml
val construct_params_string : (string * string) list -> string
```
```ocaml
val construct_params_list : 
  (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t ->
  ('a, [< suff ], 'b) params_type ->
  'a ->
  string list option * (string * Eliommod_parameters.field) list
```
```ocaml
val construct_params : 
  (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t ->
  ('a, [< suff ], 'b) params_type ->
  'a ->
  string list option * string
```
```ocaml
val make_params_names : ('a, [< suff ], 'b) params_type -> bool * 'b
```
```ocaml
val string_of_param_name : 'a -> 'a
```
```ocaml
val add_pref_params : 
  'a 'c. string ->
  ('a, [< suff ] as 'b, 'c) params_type ->
  ('a, 'b, 'c) params_type
```
```ocaml
val nl_prod : 
  ('a, [< suff ] as 'b, 'an) params_type ->
  ('s, [ `WithoutSuffix ], 'sn) non_localized_params ->
  ('a * 's, 'b, 'an * 'sn) params_type
```
```ocaml
val remove_from_nlp : 
  'a 'c. 'b Eliom_lib.String.Table.t ->
  ('a, [< suff ], 'c) params_type ->
  'b Eliom_lib.String.Table.t
```
```ocaml
type nl_params_set =
  (string * Eliommod_parameters.param) list Eliom_lib.String.Table.t
```
```ocaml
val empty_nl_params_set : 'a Eliom_lib.String.Table.t
```
```ocaml
val add_nl_parameter : 
  nl_params_set ->
  ('a, 'b) non_localized_params_ ->
  'a ->
  nl_params_set
```
```ocaml
val table_of_nl_params_set : 'a -> 'a
```
```ocaml
val list_of_nl_params_set : 
  (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t ->
  (string * Eliommod_parameters.field) list
```
```ocaml
val string_of_nl_params_set : 
  (string * Eliommod_parameters.field) list Eliom_lib.String.Table.t ->
  string
```
```ocaml
val get_nl_params_names : ('a, 'b) non_localized_params_ -> 'b
```
```ocaml
val make_nlp_name : bool -> string -> string -> string
```
```ocaml
val make_non_localized_parameters : 
  prefix:string ->
  name:string ->
  ?persistent:bool ->
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a, [ `WithoutSuffix ], 'b) non_localized_params
```
```ocaml
val contains_suffix : 'a 'c. ('a, [< suff ], 'c) params_type -> bool option
```
```ocaml
val wrap_param_type : 
  'a 'c. ('a, [< suff ] as 'b, 'c) params_type ->
  ('a, 'b, 'c) params_type
```
```ocaml
type _ is_unit = 
  | U_not : _ is_unit
  | U_yes : unit is_unit
```
```ocaml
val is_unit : 'a 'c. ('a, [< suff ], 'c) params_type -> 'a is_unit
```
```ocaml
type anon_params_type = int
```
```ocaml
val anonymise_params_type : ('a, [< suff ], 'c) params_type -> anon_params_type
```
```ocaml
type files = (string * Eliom_lib.file_info) list
```
```ocaml
type +'a res_reconstr_param = 
  | Res_ of 'a * params' * files
  | Errors_ of (string * string * exn) list * params' * files
```
```ocaml
val end_of_list : (string * 'a) list -> string -> bool
```
```ocaml
val reconstruct_params_ : 
  ('a, [< suff ], 'b) params_type ->
  params' ->
  files ->
  bool ->
  string list option ->
  'a
```
```ocaml
val reconstruct_params : 
  sp:Eliom_common.server_params ->
  ('a, [< suff ], 'c) params_type ->
  params' Lwt.t option ->
  files Lwt.t option ->
  bool ->
  string list option ->
  'a Lwt.t
```
