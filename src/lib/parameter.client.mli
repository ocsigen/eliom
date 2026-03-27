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

open Js_of_ocaml
include Eliom_parameter_sigs.S with type raw_post_data = unit

val user_type :
   of_string:(string -> 'a)
  -> to_string:('a -> string)
  -> string
  -> ('a, [`WithoutSuffix], [`One of 'a] param_name) params_type
(** Specifying parameter as [user_type ~of_string ~to_string s] tells
    that the service take a parameter, labeled [s], and that the
    server will have to use [of_string] and [to_string] to make the
    conversion between the OCaml representation of the parameter and
    it's external representation as a string. It allows one to use
    whatever type you want for a parameter of the service.  *)

val all_suffix_user :
   of_string:(string -> 'a)
  -> to_string:('a -> string)
  -> string
  -> ('a, [`Endsuffix], [`One of 'a] param_name) params_type
(** Takes the whole suffix, as long as possible, with a type specified
    by the user. *)

val reconstruct_params_form :
   (string * Form.form_elt) list
  -> ('a, _, _) params_type
  -> 'a option

val get_non_localized_get_parameters :
   ('a, [`WithoutSuffix], 'b) non_localized_params
  -> 'a option
