open Eio.Std

(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007-2016 Vincent Balat
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

(** Ad-hoc runtime type representation for service parameters.

    {b Please read Eliom's manual before this page
    to learn how to use
    {% <<a_manual chapter="server-params"|service parameters>>%}. }

    {% <<outline| <<header| **Table of contents** >> >>%} *)

module type S = sig
  type suff = [`WithoutSuffix | `WithSuffix | `Endsuffix]
  (** This type is used as a phantom type in {!params_type} to
      describe whether a parameter is encoded in the path of the URI as
      a suffix parameter. *)

  type ('a, +'b, 'c) params_type constraint 'b = [< suff]
  (** Abstract type for service parameters. See for example the
      parameter [~get_param] of {!val:Eliom_service.Http.service}.

      - [ 'a] is the type for the OCaml type of the
        parameter as expected by the service handler.

      - [ 'b] is a phantom type, subtype of {!suff}, stating the kind
        of the parameter: suffix or not.

      - [ 'c] is the type of the parameter name, usually an instance
        of {!Eliom_parameter.param_name}, as used by forms
        construction functions (e.g., the last parameter of
        {!Eliom_content.Html.D.get_form}), and specialized form
        widget (see for example the section
        {{!section:Eliom_content.Html.D.form_widgets}Form widget} of
        {!Eliom_content.HTML5.D}). ) *)

  (** {2 Typed parameter's name} *)

  type +'a param_name
  (** Abstract type for parameters' name. The ['a] type parameter is a
      phantom type, usually a subtype of {!setoneradio}, used to
      denotes the parameter's arity. *)

  type no_param_name
  (** Empty type used to denotes it is not possible to use the
      parameter in a form. See for example {!raw_post_data}.  *)

  type +'a setoneradio = [`Set of 'a | `One of 'a | `Radio of 'a]
  (** A parameter arity could either be:
      - [`Set of 'a] means: any number of ['a].
      - [`One of 'a] means: exactly one ['a].
      - [`Radio of 'a] means: zero or one ['a].
  *)

  type +'a oneradio = [`One of 'a | `Radio of 'a]
  (** Restriction of {!setoneradio} unary and optional parameters. *)

  type +'a setone = [`Set of 'a | `One of 'a]
  (** Restriction of {!setoneradio} unary and set parameters. *)

  (** {2 Type helpers} *)

  (** Helpers type used for parameters of type binary sum, see
      {!sum}. *)
  type ('a, 'b) binsum = Inj1 of 'a | Inj2 of 'b

  type 'an listnames =
    {it : 'el 'a. ('an -> 'el -> 'a -> 'a) -> 'el list -> 'a -> 'a}
  (** Helpers type used to construct forms from lists, see {!list}. *)

  type 'a to_and_of = {of_string : string -> 'a; to_string : 'a -> string}

  (** {2 Basic types of pages parameters} *)

  val int :
     string
    -> (int, [`WithoutSuffix], [`One of int] param_name) params_type
  (** [int s] means that the service takes an integer as the parameter
      named [s]. *)

  val int32 :
     string
    -> (int32, [`WithoutSuffix], [`One of int32] param_name) params_type
  (** [int32 s] means that the service takes a 32-bit integer as the
      parameter named [s]. *)

  val int64 :
     string
    -> (int64, [`WithoutSuffix], [`One of int64] param_name) params_type
  (** [int64 s] means that the service takes a 64-bit integer as the
      parameter named [s]. *)

  val float :
     string
    -> (float, [`WithoutSuffix], [`One of float] param_name) params_type
  (** [float s] means that the service takes a float as the parameter
      named [s]. *)

  val string :
     string
    -> (string, [`WithoutSuffix], [`One of string] param_name) params_type
  (** [string s] means that the service takes a string as the parameter
      named [s]. *)

  val bool :
     string
    -> (bool, [`WithoutSuffix], [`One of bool] param_name) params_type
  (** [bool s] means that the service takes a Boolean as the parameter
      named [s]. (To be used, for example, with Boolean
      checkboxes.) *)

  val file :
     string
    -> ( Eliom_lib.file_info
         , [`WithoutSuffix]
         , [`One of Eliom_lib.file_info] param_name )
         params_type
  (** [file s] means that the service takes a file as the parameter
      named [s]. *)

  val unit : (unit, [`WithoutSuffix], unit) params_type
  (** Specifying parameter as [unit] is used for services that don't
      have any parameters *)

  type coordinates = {abscissa : int; ordinate : int}
  (** The type [coordinates] represents the data sent by an [<input
      type="image" ...>]. *)

  val coordinates :
     string
    -> ( coordinates
         , [`WithoutSuffix]
         , [`One of coordinates] param_name )
         params_type
  (** [coordinates s] means that the service takes as parameters the
      coordinates of a point in an [<input type="image" ...>]. *)

  (** {2 Composing types of pages parameters} *)

  val ( ** ) :
     ('a, [`WithoutSuffix], 'b) params_type
    -> ('c, ([< `WithoutSuffix | `Endsuffix] as 'e), 'd) params_type
    -> ('a * 'c, 'e, 'b * 'd) params_type
  (** The combinator [p1 ** p2] allows one to define a service that
      takes a pair of parameters. The associated service handler
      should expect a pair [(p1, p2)]. *)

  val prod :
     ('a, [`WithoutSuffix], 'b) params_type
    -> ('c, ([< `WithoutSuffix | `Endsuffix] as 'e), 'd) params_type
    -> ('a * 'c, 'e, 'b * 'd) params_type
  (** Same as {!(**)}. *)

  val sum :
     ('a, [`WithoutSuffix], 'b) params_type
    -> ('c, [`WithoutSuffix], 'd) params_type
    -> (('a, 'c) binsum, [`WithoutSuffix], 'b * 'd) params_type
  (** The combinator [sum p1 p2] allows one to define service that
      expect either the parameter [p1] or the parameter [p2].  *)

  val opt :
     ('a, [`WithoutSuffix], 'b) params_type
    -> ('a option, [`WithoutSuffix], 'b) params_type
  (** The combinator [opt p] allows defining optional parameters. *)

  val neopt :
     ('a, [`WithoutSuffix], 'b) params_type
    -> ('a option, [`WithoutSuffix], 'b) params_type
  (** The combinator [neopt p] allows defining an optional parameter
      assumed to be None if empty. *)

  val radio :
     (string -> ('a, [`WithoutSuffix], [`One of 'b] param_name) params_type)
    -> string
    -> ('a option, [`WithoutSuffix], [`Radio of 'b] param_name) params_type
  (** A parameter as [radio f s] specifies that the service takes an
      optional argument labeled [s], of type [f s].  Use [radio]
      instead of {!opt} if you want to use this parameter with a radio
      button.  *)

  val any : ((string * string) list, [`WithoutSuffix], unit) params_type
  (** Use this if you want to take any parameters.  The service will
      answer to all the request, and get all parameters as an
      association list of strings. *)

  val set :
     (string -> ('a, [`WithoutSuffix], [`One of 'b] param_name) params_type)
    -> string
    -> ('a list, [`WithoutSuffix], [`Set of 'b] param_name) params_type
  (** Use this if you want your service to take several parameters
      with the same name. The service handler will receive a list of
      values.  To create the form, just use the same name several
      times.  For example [set int "i"] will match the parameter
      string [i=4&i=22&i=111] and send to the service handler a list
      containing the three integers 4, 22 and 111. The order is
      unspecified.  *)

  val list :
     string
    -> ('a, [`WithoutSuffix], 'b) params_type
    -> ('a list, [`WithoutSuffix], 'b listnames) params_type
  (** The service takes a list of parameters. The first parameter of
      this function is the name of the list. The service handler will
      receive a list of values. To create the form, an iterator of
      type {!Eliom_parameter.listnames} is given to generate the name
      for each value. *)

  val suffix :
     ?redirect_if_not_suffix:bool
    -> ('s, [< `WithoutSuffix | `Endsuffix], 'sn) params_type
    -> ('s, [`WithSuffix], 'sn) params_type
  (** Tells that the parameter of the service handler is the suffix of
      the URL of the current service.  e.g. [suffix (int "i" ** string
      "s")] will match an URL ending by [380/yo].  and send [(380,
      "yo")] to the service handler.

      For each service with suffix, there is also a service with
      regular parameters (without suffix) that will be used if you
      create a form towards a service with suffix.  If
      [redirect_if_not_suffix] is [true] (default), this service
      without suffix will be redirected to the suffix version.  *)

  val all_suffix :
     string
    -> (string list, [`Endsuffix], [`One of string list] param_name) params_type
  (** Takes the whole suffix, as long as possible, as a (slash
      separated) string list *)

  val all_suffix_string :
     string
    -> (string, [`Endsuffix], [`One of string] param_name) params_type
  (** Takes the whole suffix, as long as possible, as a string *)

  val suffix_prod :
     ?redirect_if_not_suffix:bool
    -> ('s, [< `WithoutSuffix | `Endsuffix], 'sn) params_type
    -> ('a, [`WithoutSuffix], 'an) params_type
    -> ('s * 'a, [`WithSuffix], 'sn * 'an) params_type
  (** Tells that the function that will generate the service takes a
      pair whose first element is the suffix of the URL of the current
      service, and the second element corresponds to other (regular)
      parameters.  e.g.: [suffix_prod (int "suff" ** all_suffix
      "endsuff") (int "i")] will match an URL ending by
      [777/go/go/go?i=320] and send the value [((777, ["go";"go";"go"]),
      320)] to the service handler.  *)

  val suffix_const :
     string
    -> (unit, [`WithoutSuffix], [`One of unit] param_name) params_type
  (** [suffix_const v] is used only inside suffixes. It does nothing
      for regular parameters. It specifies that the service takes a
      constant parameter inside the suffix, whose value must be [v].
      It is used for putting constant directory names inside suffix
      parameters (and thus allows suffix parameters that are anywhere
      in the path, e.g. [/param1/const/param2]).  *)

  type 'a ocaml
  (** marshaled OCaml values of type 'a *)

  val ocaml :
     string
    -> 'a Deriving_Json.t
    -> ('a, [`WithoutSuffix], [`One of 'a ocaml] param_name) params_type
  (** [ocaml s] tells that the service is expecting some caml (client
      side) program to send some value of type 'a, marshaled.  As usual
      [s] is the name of the parameter. *)

  type raw_post_data
  (** When the content type is neither URLencoded form data nor
      multipart data, it is possible to get it as a stream of strings.
      The first element of the pair is the content-type.  This kind of
      parameter cannot be combined with others. It is not possible to
      create a form towards a service taking such a parameter. *)

  val raw_post_data :
    (raw_post_data, [`WithoutSuffix], no_param_name) params_type

  (** {2 Non localized parameters} *)

  type ('a, +'b, 'names) non_localized_params constraint 'b = [< suff]

  val make_non_localized_parameters :
     prefix:string
    -> name:string
    -> ?persistent:bool
    -> ('a, [`WithoutSuffix], 'b) params_type
    -> ('a, [`WithoutSuffix], 'b) non_localized_params
  (** Create a new specification for non localized parameters. You
      must give a name to this set of parameters. Warning: the names
      must be unique for the whole application. That's why the name is
      composed by a prefix (the name of your project) and another
      string (the name of your non localized parameters).

      Will fail with exception [Failure _] if the name contains a dot.
      If [?persistent] is [true], the non localized parameter may
      remain if you call another service, if this service allows this
      (default [false]). *)

  type nl_params_set
  (** Use this type to give non localized parameters to a link or a
      form *)

  val empty_nl_params_set : nl_params_set

  val add_nl_parameter :
     nl_params_set
    -> ('a, [< `WithSuffix | `WithoutSuffix], _) non_localized_params
    -> 'a
    -> nl_params_set

  val get_nl_params_names :
     (_, [< `WithSuffix | `WithoutSuffix], 'a) non_localized_params
    -> 'a

  val get_to_and_of : ('a, 'b, 'c) params_type -> 'a to_and_of
  (** Given a parameter type, get the two functions that converts from
      and to strings. You should only use this function on
      - options ;
      - basic types : int, int32, int64, float, string
      - marshal
      - unit
      - string
      - bool *)

  (**/**)

  val walk_parameter_tree :
     [`One of string] param_name
    -> ('a, 'b, 'c) params_type
    -> 'a to_and_of option

  (* None = no suffix. The bool means : redirect_if_not_suffix *)
  val contains_suffix : ('a, 'b, 'c) params_type -> bool option

  val add_pref_params :
     string
    -> ('a, 'b, 'c) params_type
    -> ('a, 'b, 'c) params_type

  type params = (string * Eliommod_parameters.param) list

  val construct_params :
     params Eliom_lib.String.Table.t
    -> ('a, [< `WithSuffix | `WithoutSuffix], 'b) params_type
    -> 'a
    -> string list option * string

  val construct_params_string : params -> string

  val construct_params_list_raw :
     params Eliom_lib.String.Table.t
    -> ('a, [< `WithSuffix | `WithoutSuffix], 'b) params_type
    -> 'a
    -> string list option * params Eliom_lib.String.Table.t * params

  val construct_params_list :
     params Eliom_lib.String.Table.t
    -> ('a, [< `WithSuffix | `WithoutSuffix], 'b) params_type
    -> 'a
    -> string list option * params

  val reconstruct_params :
     sp:Eliom_common.server_params
    -> ('a, [< `WithSuffix | `WithoutSuffix], 'c) params_type
    -> (string * string) list Promise.t option
    -> (string * Eliom_lib.file_info) list Promise.t option
    -> bool
    -> Eliom_lib.Url.path option
    -> 'a

  val make_params_names : ('a, 'b, 'c) params_type -> bool * 'c
  (* bool = contains a suffix *)

  val string_of_param_name : 'a param_name -> string

  val nl_prod :
     ('a, 'su, 'an) params_type
    -> ('s, [`WithoutSuffix], 'sn) non_localized_params
    -> ('a * 's, 'su, 'an * 'sn) params_type

  val remove_from_nlp :
     (string * 'c) list Eliom_lib.String.Table.t
    -> ('a, [< `WithSuffix | `WithoutSuffix], 'b) params_type
    -> (string * 'c) list Eliom_lib.String.Table.t

  val table_of_nl_params_set : nl_params_set -> params Eliom_lib.String.Table.t
  val list_of_nl_params_set : nl_params_set -> params
  val string_of_nl_params_set : nl_params_set -> string
  val wrap_param_type : ('a, 'b, 'c) params_type -> ('a, 'b, 'c) params_type

  type _ is_unit = U_not : _ is_unit | U_yes : unit is_unit

  val is_unit : ('a, _, _) params_type -> 'a is_unit
  val anonymise_params_type : ('a, 'b, 'c) params_type -> int
end

val section : Logs.src
