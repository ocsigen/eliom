(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_uri
 * Copyright (C) 2007 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Low-level functions for relative or absolute URL calculation. *)

open Lwt

open Eliom_lib
open Eliom_common
open Eliom_parameter
open Eliom_service

(** {2 Compute service's URL}

    Please note that for many functions of this section, the returned
    URL depends on whether the function is called from a service
    handler or not:

    - relative URL could not be computed outside of a service handler.
    - "kept" non localized parameters outside a service handler are
      restricted to preapplied parameters.

    To define {e global} link (i.e. outside of a service handler) and
    recompute a relative URL at each request, use
    {!Eliom_registration.Html5.a} or other specialized functions from
    {!Eliom_registration.Html5}.

*)

(** The function [make_string_uri ~service get_params] creates the
    string corresponding to the URL of the service [service] applied
    to the GET parameters [get_params].

    See {!Eliom_registration.Html5.make_string_uri} or any other
    {!Eliom_registration}[.*.make_string_uri] for a detailled description of
    optional parameters.
*)
val make_string_uri :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, unit, [<get_service_kind], _, _,
           [< suff ], 'gn, unit,
           [< registrable ], 'return) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:nl_params_set -> 'get -> string

(** The function [make_uri_components service get_params] returns the
    a triplet [(path, get_params, fragment)] that is a decomposition
    of the URL of [service] applied to the GET parameters
    [get_params].

    See {!Eliom_registration.Html5.make_uri_components} or any other
    {!Eliom_registration}[.*.make_uri_components] for a detailled
    description. *)
val make_uri_components :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, unit, [< get_service_kind], _, _,
           [< suff ], 'gn, unit,
           [< registrable ], 'return) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:nl_params_set ->
  'get -> string * (string * Eliommod_parameters.param) list * string option


(** Same a {!make_uri_components}, but also returns a table of post
    parameters. *)
val make_post_uri_components :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('get, 'post, [< post_service_kind], _, _,
           [< suff ], 'gn, 'pn,
           [< registrable ], 'return) service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?nl_params:nl_params_set ->
  ?keep_get_na_params:bool ->
  'get ->
  'post ->
  string * (string * Eliommod_parameters.param) list * string option *
    (string * Eliommod_parameters.param) list

(** The function [make_string_uri_from_components path get_params
    fragment] build the corresponding string URL. The [path] should
    be URL encoded.

    The function {!make_string_uri} is the composition of
    {!make_uri_components} and [make_string_uri_from_components].
*)
val make_string_uri_from_components :
  string * (string * Eliommod_parameters.param) list * string option -> string

(** {2 Relative paths} *)

(** The function [reconstruct_relative_url_path src dest] returns a
    path to [dest] that is relative to [src].
*)
val reconstruct_relative_url_path :
  string list -> string list -> string list


(**/**)

(* make_string_uri_ and make_post_uri_components__ are alias to
   make_string_uri and make_post_uri_components with a less
   restrictive type. They should be removed once there is way to
   downcast a "service_method" service to "get" or "post" service. See
   Eliom_mkreg and Eliom_client. *)

val make_string_uri_ :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b, _, _, _,
           [< Eliom_service.suff ], 'c, 'd,
           [< Eliom_service.registrable ], 'e)
    Eliom_service.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:nl_params_set -> 'a -> string

val make_post_uri_components__ :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b, _, _, _,
           [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
    Eliom_service.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  'b ->
  string * (string * Eliommod_parameters.param) list * string option *
    (string * Eliommod_parameters.param) list


val make_uri_components_ :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b, _, _, _,
           [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
    service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:nl_params_set ->
  unit -> string * (string * Eliommod_parameters.param) list * string option

val make_post_uri_components_ :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b, _, _, _,
           [< `WithSuffix | `WithoutSuffix ], 'g, 'h, 'i, 'j)
    service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  unit ->
  string * (string * Eliommod_parameters.param) list * string option *
    (string * Eliommod_parameters.param) list




val make_actual_path: string list -> string list

val make_proto_prefix :
  ?hostname:string -> ?port:int ->
  bool -> string

val make_cookies_info :
  bool option *
  ('a, 'b, _, [< attached], _, [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h) Eliom_service.service ->
  (bool * Url.path) option
