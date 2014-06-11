(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomparameters.mli
 * Copyright (C) 2007 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** See documentation in the server side version:
    {% <<a_api subproject="server"|module Eliom_parameter>>%}. *)

open Eliom_lib

type suff = [ `WithoutSuffix | `WithSuffix | `Endsuffix ]

type ('a, +'b, 'c) params_type constraint 'b = [<suff]

type +'a param_name

type no_param_name

type +'a setoneradio = [ `Set of 'a | `One of 'a | `Radio of 'a ]

type +'a oneradio = [ `One of 'a | `Radio of 'a ]

type +'a setone = [ `Set of 'a | `One of 'a ]

type 'a to_and_from = {
  of_string : string -> 'a;
  to_string : 'a -> string
}


type ('a, 'b) binsum = Inj1 of 'a | Inj2 of 'b

type 'an listnames =
    {it:'el 'a. ('an -> 'el -> 'a -> 'a) -> 'el list -> 'a -> 'a}

val int : string ->
  (int, [ `WithoutSuffix ], [ `One of int ] param_name) params_type

val int32 : string ->
  (int32, [ `WithoutSuffix ], [ `One of int32 ] param_name) params_type

val int64 : string ->
  (int64, [ `WithoutSuffix ], [ `One of int64 ] param_name) params_type

val float : string ->
  (float, [ `WithoutSuffix ], [ `One of float ] param_name) params_type

val string :
    string ->
      (string, [ `WithoutSuffix ], [ `One of string ] param_name) params_type

val bool :
    string ->
      (bool, [ `WithoutSuffix ], [ `One of bool ] param_name) params_type

val file :
    string -> (file_info, [ `WithoutSuffix ],
               [ `One of file_info ] param_name) params_type

val unit : (unit, [ `WithoutSuffix ], unit) params_type

val user_type :
  of_string:(string -> 'a) ->
  to_string:('a -> string) ->
  string ->
  ('a, [ `WithoutSuffix ], [ `One of 'a ] param_name) params_type

type coordinates =
    {abscissa: int;
     ordinate: int}

val coordinates :
  string ->
  (coordinates, [ `WithoutSuffix ],
   [ `One of coordinates ] param_name) params_type

val string_coordinates :
  string ->
  (string * coordinates, [ `WithoutSuffix ],
   [ `One of (string * coordinates) ] param_name) params_type

val int_coordinates :
    string ->
      (int * coordinates, [`WithoutSuffix],
       [ `One of (int * coordinates) ] param_name) params_type

val int32_coordinates :
    string ->
      (int32 * coordinates, [`WithoutSuffix],
       [ `One of (int32 * coordinates) ] param_name) params_type

val int64_coordinates :
    string ->
      (int64 * coordinates, [`WithoutSuffix],
       [ `One of (int64 * coordinates) ] param_name) params_type

val float_coordinates :
    string ->
      (float * coordinates, [`WithoutSuffix],
       [ `One of (float * coordinates) ] param_name) params_type

val user_type_coordinates :
  of_string:(string -> 'a) -> to_string:('a -> string) -> string ->
  ('a * coordinates, [`WithoutSuffix],
   [ `One of ('a * coordinates) ] param_name) params_type

val ( ** ) :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('c, [< `WithoutSuffix | `Endsuffix ] as 'e, 'd) params_type ->
  ('a * 'c, 'e, 'b * 'd) params_type

val prod :
    ('a, [ `WithoutSuffix ], 'b) params_type ->
      ('c, [< `WithoutSuffix | `Endsuffix ] as 'e, 'd) params_type ->
        ('a * 'c, 'e, 'b * 'd) params_type

val sum :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('c, [ `WithoutSuffix ], 'd) params_type ->
  (('a, 'c) binsum, [ `WithoutSuffix ], 'b * 'd) params_type

val opt :
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a option, [ `WithoutSuffix ], 'b) params_type

val radio :
  (string ->
   ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type) ->
  string ->
  ('a option, [ `WithoutSuffix ], [ `Radio of 'b ] param_name) params_type

val any :
      ((string * string) list, [ `WithoutSuffix ], unit) params_type

val set :
    (string ->
      ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type) ->
        string ->
          ('a list, [ `WithoutSuffix ], [ `Set of 'b ] param_name) params_type

val list :
    string ->
      ('a, [ `WithoutSuffix ], 'b) params_type ->
        ('a list, [ `WithoutSuffix ], 'b listnames) params_type

val guard : (string -> ('a, 'b, 'c) params_type) -> string
  -> ('a -> bool) -> ('a, 'b, [ `One of 'a ] param_name) params_type

val suffix :
  ?redirect_if_not_suffix:bool ->
  ('s, [< `WithoutSuffix | `Endsuffix ], 'sn) params_type ->
  ('s, [ `WithSuffix ], 'sn) params_type

val all_suffix :
  string ->
  (string list, [`Endsuffix], [` One of string list ] param_name) params_type

val all_suffix_string :
  string -> (string, [`Endsuffix], [` One of string ] param_name) params_type

val all_suffix_user :
  of_string:(string -> 'a) ->
  to_string:('a -> string) -> string ->
  ('a, [ `Endsuffix ], [` One of 'a ] param_name) params_type

val suffix_prod :
  ?redirect_if_not_suffix:bool ->
  ('s,[<`WithoutSuffix|`Endsuffix],'sn) params_type ->
  ('a,[`WithoutSuffix], 'an) params_type ->
  (('s * 'a), [`WithSuffix], 'sn * 'an) params_type

val suffix_const :
    string ->
      (unit, [ `WithoutSuffix ], [ `One of unit ] param_name) params_type

type 'a ocaml

val ocaml :
  string ->
  'a Deriving_Json.t ->
  ('a, [ `WithoutSuffix ], [ `One of 'a ocaml ] param_name) params_type

type raw_post_data =
  ((string * string) * (string * string) list) option *
    string Ocsigen_stream.t option

val raw_post_data :
  (raw_post_data,
   [ `WithoutSuffix ], no_param_name) params_type

type ('a, +'b, 'names) non_localized_params  constraint 'b = [<suff]

val make_non_localized_parameters :
  prefix : string ->
  name : string ->
  ?persistent:bool ->
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a, [ `WithoutSuffix ], 'b) non_localized_params

type nl_params_set

val empty_nl_params_set : nl_params_set

val add_nl_parameter :
  nl_params_set ->
  ('a, [< `WithSuffix | `WithoutSuffix ], _) non_localized_params ->
  'a -> nl_params_set

val get_nl_params_names :
  (_, [< `WithSuffix | `WithoutSuffix ], 'a) non_localized_params -> 'a

val get_to_and_from : ('a, 'b, 'c) params_type -> 'a to_and_from

val walk_parameter_tree : string -> ('a, 'b, 'c) params_type -> 'a to_and_from option
val contains_suffix : ('a, 'b, 'c) params_type -> bool option

val add_pref_params :
    string ->
      ('a, 'b, 'c) params_type ->
        ('a, 'b, 'c) params_type

val construct_params :
  (string * Eliommod_parameters.param) list String.Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  'a -> string list option * string

val construct_params_string :
  (string * Eliommod_parameters.param) list -> string

val construct_params_list_raw :
  (string * Eliommod_parameters.param) list String.Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  'a -> string list option *
  (string * Eliommod_parameters.param) list String.Table.t *
     (string * Eliommod_parameters.param) list

val construct_params_list :
  (string * Eliommod_parameters.param) list String.Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  'a ->
  string list option * (string * Eliommod_parameters.param) list

val make_params_names :
    ('a, 'b, 'c) params_type -> bool * 'c

val string_of_param_name : 'a param_name -> string

val nl_prod :
  ('a, 'su, 'an) params_type ->
  ('s, [ `WithoutSuffix ], 'sn) non_localized_params ->
  ('a * 's, 'su, 'an * 'sn) params_type

val remove_from_nlp :
  (string * 'c) list String.Table.t ->
  ('a, [< `WithSuffix | `WithoutSuffix ], 'b) params_type ->
  (string * 'c) list String.Table.t

val table_of_nl_params_set : nl_params_set ->
  (string * Eliommod_parameters.param) list String.Table.t

val list_of_nl_params_set :
  nl_params_set -> (string * Eliommod_parameters.param) list

val string_of_nl_params_set : nl_params_set -> string

val wrap_param_type : ('a, 'b, 'c) params_type -> ('a, 'b, 'c) params_type
