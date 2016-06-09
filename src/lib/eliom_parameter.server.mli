(* Ocsigen
 * http://www.ocsigen.org
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

include Eliom_parameter_sigs.S

(** Specifying parameter as [type_checker check t] is equivalent as
    [t] but the check function is called after decoding the
    parameters, allowing you to make more checks on the parameters
    before the service handler is called. Raise an exception if the
    parameter is not correct, and the error handler will be called
    instead of the service handler. *)
val type_checker :
  ('a -> unit) ->
  ('a, [<suff] as 'b, 'c) params_type ->
  ('a, 'b, 'c) params_type

(** [regexp r d s] tells that the service takes a string that matches
    the regular expression [r] as parameter, labeled [s], and that will
    be rewritten in d.  The syntax of regexp is PCRE's one (uses
    [Netstring_pcre], from OCamlnet).  For example: [regexp
    (Netstring_pcre.regexp "\\[(.* )\\]") "($1)" "myparam"] will match
    the parameter [myparam=[hello]] and send the string ["(hello)"] to
    the service handler.  *)
val regexp :
  Netstring_pcre.regexp ->
  string ->
  to_string:(string -> string) ->
  string ->
  (string, [ `WithoutSuffix ], [` One of string ] param_name) params_type

(** [all_suffix_regexp r d s] takes all the suffix, as long as
    possible, matching the regular expression [r], name [s], and
    rewrite it in [d].  *)
val all_suffix_regexp :
  Netstring_pcre.regexp ->
  string ->
  to_string:(string -> string) ->
  string ->
  (string, [ `Endsuffix ], [` One of string ] param_name) params_type

(** [get_non_localized_get_parameters ~sp p] decodes and returns non
    localized GET parameters specified by [p] if present. *)
val get_non_localized_get_parameters :
  ('a, [ `WithoutSuffix ], 'b) non_localized_params ->
  'a option

(** [get_non_localized_post_parameters ~sp p] decodes and returns non
    localized POST parameters specified by [p] if present. *)
val get_non_localized_post_parameters :
  ('a, [ `WithoutSuffix ], 'b) non_localized_params ->
  'a option

(**/**)

val anonymise_params_type : ('a, 'b, 'c) params_type -> int
