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

open Eliom_lib
open Eliom_parameter

(** {2 Compute service's URL}

    Please note that for many functions of this section, the returned
    URL depends on whether the function is called from a service
    handler or not:

    - relative URL could not be computed outside of a service handler.
    - "kept" non localized parameters outside a service handler are
      restricted to preapplied parameters.

    To define {e global} link (i.e. outside of a service handler) and
    recompute a relative URL at each request, use
    {!Eliom_registration.Html.a} or other specialized functions from
    {!Eliom_registration.Html}.

*)

val make_string_uri :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'get
         , unit
         , Eliom_service.get
         , _
         , _
         , _
         , _
         , _
         , _
         , unit
         , 'return )
         Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:nl_params_set
  -> 'get
  -> string
(** The function [make_string_uri ~service get_params] creates the
    string corresponding to the URL of the service [service] applied
    to the GET parameters [get_params].

    See {!Eliom_registration.Html.make_string_uri} or any other
    {!Eliom_registration}[.*.make_string_uri] for a detailed
    description of optional parameters.

    {e Warning: The function [make_string_uri] should not be called
    outside of a service handler, unless [hostname] is not [None] and
    one of the following condition is met:}

    - the optional parameter [~absolute_path] is [true].
    - the optional parameter [~absolute] is [true].
    - the optional parameter [~https] is [true].
    - the [service] has been created with [~https:true].
    - the [service] is an external service. *)

val make_uri_components :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ('get, _, Eliom_service.get, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:nl_params_set
  -> 'get
  -> string * (string * Eliommod_parameters.param) list * string option
(** The function [make_uri_components service get_params] returns the
    a triplet [(path, get_params, fragment)] that is a decomposition
    of the URL of [service] applied to the GET parameters
    [get_params].

    See {!Eliom_registration.Html.make_uri_components} or any other
    {!Eliom_registration}[.*.make_uri_components] for a detailed
    description. *)

val make_post_uri_components :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ('get, 'post, Eliom_service.post, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> ?nl_params:nl_params_set
  -> ?keep_get_na_params:bool
  -> 'get
  -> 'post
  -> string
     * (string * Eliommod_parameters.param) list
     * string option
     * (string * Eliommod_parameters.param) list
(** Same a {!make_uri_components}, but also returns a table of post
    parameters. *)

val make_string_uri_from_components :
   string * (string * Eliommod_parameters.param) list * string option
  -> string
(** The function [make_string_uri_from_components path get_params
    fragment] build the corresponding string URL. The [path] should
    be URL encoded.

    The function {!make_string_uri} is the composition of
    {!make_uri_components} and [make_string_uri_from_components].
*)

(** {2 Relative paths} *)

val reconstruct_relative_url_path : string list -> string list -> string list
(** The function [reconstruct_relative_url_path src dest] returns a
    path to [dest] that is relative to [src].
*)

(**/**)

(* make_string_uri_ and make_post_uri_components__ are alias to
   make_string_uri and make_post_uri_components with a less
   restrictive type. They should be removed once there is way to
   downcast a "service_method" service to "get" or "post" service. See
   Eliom_mkreg and Eliom_client. *)

val make_string_uri_ :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:('get, _, _, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:nl_params_set
  -> 'get
  -> string

val make_post_uri_components__ :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:('get, 'post, _, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:nl_params_set
  -> ?keep_get_na_params:bool
  -> 'get
  -> 'post
  -> string
     * (string * Eliommod_parameters.param) list
     * string option
     * (string * Eliommod_parameters.param) list

val make_uri_components_ :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:(_, _, _, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:nl_params_set
  -> unit
  -> string * (string * Eliommod_parameters.param) list * string option

val make_post_uri_components_ :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:('get, 'post, _, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:nl_params_set
  -> ?keep_get_na_params:bool
  -> 'get
  -> unit
  -> string
     * (string * Eliommod_parameters.param) list
     * string option
     * (string * Eliommod_parameters.param) list

val make_actual_path : string list -> string list

val make_proto_prefix : ?hostname:string -> ?port:int -> bool -> string
(** Creates the string corresponding to the beginning of the URL,
    containing the scheme (protocol), server and port number (if
    necessary).  *)

val make_cookies_info :
   bool option * (_, _, _, _, _, _, _, _, _, _, _) Eliom_service.t
  -> (bool * Url.path) option
