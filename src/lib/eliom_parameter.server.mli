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

include
  Eliom_parameter_sigs.S
  with type raw_post_data =
    ((string * string) * (string * string) list) option * Cohttp_lwt.Body.t

val user_type :
   ?client_to_and_of:'a to_and_of Eliom_client_value.t
  -> of_string:(string -> 'a)
  -> to_string:('a -> string)
  -> string
  -> ('a, [`WithoutSuffix], [`One of 'a] param_name) params_type
(** [user_type ~of_string ~to_string s] construct a parameter, labeled
    [s], such that the server will have to use [of_string] and
    [to_string] to make the conversion between the OCaml
    representation of the parameter and it's representation as a
    string. It allows one to use any type for a parameter. Providing
    converters via the optional [?client_to_and_from] parameter allows
    injecting the parameter (or a service that uses it) for use in
    client code. *)

val all_suffix_user :
   ?client_to_and_of:'a to_and_of Eliom_client_value.t
  -> of_string:(string -> 'a)
  -> to_string:('a -> string)
  -> string
  -> ('a, [`Endsuffix], [`One of 'a] param_name) params_type
(** Takes the whole suffix, as long as possible, with a type specified
    by the user. See [user_type] for the description of the
    arguments. *)

val type_checker :
   ('a -> unit)
  -> ('a, ([< suff] as 'b), 'c) params_type
  -> ('a, 'b, 'c) params_type
(** Specifying parameter as [type_checker check t] is equivalent as
    [t] but the check function is called after decoding the
    parameters, allowing you to make more checks on the parameters
    before the service handler is called. Raise an exception if the
    parameter is not correct, and the error handler will be called
    instead of the service handler. *)

val regexp :
   Re.Pcre.regexp
  -> string
  -> to_string:(string -> string)
  -> string
  -> (string, [`WithoutSuffix], [`One of string] param_name) params_type
(** [regexp r d s] tells that the service takes a string that matches
    the regular expression [r] as parameter, labeled [s], and that will
    be rewritten in d.  The syntax of regexp is PCRE's one (uses then
    [Pcre] bindings).  For example: [regexp
    (Re.Pcre.regexp "\\[(.* )\\]") "($1)" "myparam"] will match
    the parameter [myparam=[hello]] and send the string ["(hello)"] to
    the service handler.  *)

val all_suffix_regexp :
   Re.Pcre.regexp
  -> string
  -> to_string:(string -> string)
  -> string
  -> (string, [`Endsuffix], [`One of string] param_name) params_type
(** [all_suffix_regexp r d s] takes all the suffix, as long as
    possible, matching the regular expression [r], name [s], and
    rewrite it in [d].  *)

val get_non_localized_get_parameters :
   ('a, [`WithoutSuffix], 'b) non_localized_params
  -> 'a option
(** [get_non_localized_get_parameters ~sp p] decodes and returns non
    localized GET parameters specified by [p] if present. *)

val get_non_localized_post_parameters :
   ('a, [`WithoutSuffix], 'b) non_localized_params
  -> 'a option
(** [get_non_localized_post_parameters ~sp p] decodes and returns non
    localized POST parameters specified by [p] if present. *)

(**/**)

val anonymise_params_type : ('a, 'b, 'c) params_type -> int
