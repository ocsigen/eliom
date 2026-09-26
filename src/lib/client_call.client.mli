(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Copyright (C) 2011 Jérôme Vouillon, Grégoire Henry, Pierre Chambart
 * Copyright (C) 2012 Benedikt Becker
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

(** Low-level calls to services: building the request, sending it, and
    leaving the application for another page. Internal module. *)

open Js_of_ocaml

val create_request_ :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'a
         , 'b
         , _
         , _
         , _
         , _
         , _
         , [< `WithSuffix | `WithoutSuffix]
         , _
         , _
         , _ )
         Service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> 'a
  -> 'b
  -> [ `Get of string * (string * Mod_parameters.param) list
     | `Post of
         string
         * (string * Mod_parameters.param) list
         * (string * Mod_parameters.param) list
     | `Put of
         string
         * (string * Mod_parameters.param) list
         * (string * Mod_parameters.param) list
     | `Delete of
         string
         * (string * Mod_parameters.param) list
         * (string * Mod_parameters.param) list ]
(** The URI and the parameters of a request to a service, tagged with
    its HTTP method. For a [`Get], the parameters are the GET ones;
    otherwise they are the GET and the POST ones. *)

val raw_call_service :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'a
         , 'b
         , _
         , _
         , _
         , _
         , _
         , [< `WithSuffix | `WithoutSuffix]
         , _
         , _
         , _ )
         Service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> 'a
  -> 'b
  -> (string * string) Lwt.t
(** Calls a service and returns the URI of the response and its content.
    Fails with [Request.Failed_request 204] if there is no content. *)

val call_service :
   ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'a
         , 'b
         , _
         , _
         , _
         , _
         , _
         , [< `WithSuffix | `WithoutSuffix]
         , _
         , _
         , _ )
         Service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> 'a
  -> 'b
  -> string Lwt.t
(** See {!Client.call_service}. *)

val exit_to :
   ?window_name:string
  -> ?window_features:string
  -> ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'a
         , 'b
         , _
         , _
         , _
         , _
         , _
         , [< `WithSuffix | `WithoutSuffix]
         , _
         , _
         , _ )
         Service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> 'a
  -> 'b
  -> unit
(** See {!Client.exit_to}. *)

val window_open :
   window_name:Js.js_string Js.t
  -> ?window_features:Js.js_string Js.t
  -> ?absolute:bool
  -> ?absolute_path:bool
  -> ?https:bool
  -> service:
       ( 'a
         , unit
         , _
         , _
         , _
         , _
         , _
         , [< `WithSuffix | `WithoutSuffix]
         , _
         , _
         , _ )
         Service.t
  -> ?hostname:string
  -> ?port:int
  -> ?fragment:string
  -> ?keep_nl_params:[`All | `None | `Persistent]
  -> ?nl_params:Parameter.nl_params_set
  -> ?keep_get_na_params:bool
  -> 'a
  -> Dom_html.window Js.t Js.opt
(** See {!Client.window_open}. *)
