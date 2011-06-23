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

(** Functions to add non localised parameters to services and
    client side declaration of void coservices. Void coservices are the only ones
    defined on client side. *)

open Eliom_parameters
open Eliom_pervasives

(** {2 Types of services} *)

type suff = [ `WithSuffix | `WithoutSuffix ]

type servcoserv = [ `Service | `Coservice ]

type getpost = [ `Get | `Post ]
      (* `Post means that there is at least one post param
         (possibly only the state post param).
         `Get is for all the other cases.
       *)

type attached_service_kind =
    [ `Internal of servcoserv
    | `External ]

type internal =
    [ `Internal of servcoserv ]

type registrable = [ `Registrable | `Unregistrable ]
(** You can call register function only on registrable services *)
(* Registrable means not pre-applied *)

type (+'a, +'b) a_s

type +'a na_s

type service_kind =
    [ `Attached of (attached_service_kind, getpost) a_s
    | `Nonattached of getpost na_s ]

type internal_service_kind =
    [ `Attached of (internal, getpost) a_s
    | `Nonattached of getpost na_s ]

type get_service_kind =
    [ `Attached of (attached_service_kind, [ `Get ]) a_s
    | `Nonattached of [ `Get ] na_s ]

type post_service_kind =
    [ `Attached of (attached_service_kind, [ `Post ]) a_s
    | `Nonattached of [ `Post ] na_s ]

type attached =
    [ `Attached of (attached_service_kind, getpost) a_s ]

type nonattached =
    [ `Nonattached of getpost na_s ]

type http (** default return type for services *)

type appl_service (** return type for service that are entry points for an
                      application *)

type ('get,'post,+'kind,+'tipo,+'getnames,+'postnames,+'registr,+'return) service
(** Type of services.
    - [ 'get] is the type of GET parameters
    - [ 'post] is the type of POST parameters
    - [ 'kind] is a subtype of {!Eliom_services.service_kind}
    (attached or non-attached
    service, internal or external, GET only or with POST parameters)
    - [ 'tipo] is a phantom type stating the kind of parameters it uses
    (suffix or not)
    - [ 'getnames] is the type of GET parameters names
    - [ 'postnames] is the type of POST parameters names
    - [ 'registrable] is a phantom type,
    subtype of {!Eliom_services.registrable},
    telling if it is possible to register a handler on this service.
    - [ 'return ] is an information on what the service returns
 *)


(** {3 Void non-attached coservices} *)

val void_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service
(** A predefined non-attached action with special behaviour:
    it has no parameter at all, even non-attached parameters.
    Use it if you want to make a link to the current page without non-attached
    parameters.
    It is almost equivalent to a POST non-attached service without POST
    parameters, on which you register an action that does nothing,
    but you can use it with <a> links, not only forms.
    It does not keep non attached GET parameters.
 *)

val https_void_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service
(** The same, but forcing https. *)

val void_hidden_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service
(** Same as [void_coservice'] but keeps non attached GET parameters.
 *)

val https_void_hidden_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service
(** The same, but forcing https. *)


(** {2 Miscellaneous} *)

val add_non_localized_get_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a * 'p, 'b, 'c, 'd, 'e * 'pn, 'f, 'g, 'return) service
(** Adds non localized GET parameters to a service *)

val add_non_localized_post_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'b * 'p, 'c, 'd, 'e, 'f * 'pn, 'g, 'return) service
(** Adds non localized POST parameters to a service *)


(**/**)

(* used by Eliom_uri *)
val get_get_or_post :
  ('a, 'b,
   [< `Attached of (attached_service_kind, [< getpost]) a_s
   | `Nonattached of [< getpost ] na_s ], 'd, 'e, 'f, 'g, 'h) service ->
  getpost

val get_kind_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service -> 'c
val get_pre_applied_parameters_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  (string * string) list String.Table.t *
  (string * string) list
val get_get_params_type_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'd, 'e) Eliom_parameters.params_type
val get_post_params_type_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('b, [ `WithoutSuffix ], 'f) Eliom_parameters.params_type
val get_att_kind_ : ('a, 'b) a_s -> 'a
val get_sub_path_ : ('a, 'b) a_s -> Url.path
val get_full_path_ : ('a, 'b) a_s -> Url.path
val get_prefix_ : ('a, 'b) a_s -> string
val get_get_name_ : ('a, 'b) a_s -> Eliom_common.att_key_serv
val get_post_name_ : ('a, 'b) a_s -> Eliom_common.att_key_serv
val get_redirect_suffix_ : ('a, 'b) a_s -> bool
val get_na_name_ : 'a na_s -> Eliom_common.na_key_serv
val get_na_kind_ : 'a na_s -> [ `Get | `Post of bool ]
val get_max_use_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service -> int option
val get_timeout_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service -> float option
val get_https : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service -> bool
val get_priority_ : ('a, 'b) a_s -> int

val keep_nl_params : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  [ `All | `Persistent | `None ]

val change_get_num :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('h, 'hh) a_s ->
  Eliom_common.att_key_serv ->
  ('a, 'b, [> `Attached of ('h, 'hh) a_s ], 'd, 'e, 'f, 'i, 'return) service

(* Not implemented on client side: TODO should not be called in Eliom_uri *)
val register_delayed_get_or_na_coservice :
  sp:Eliom_common.server_params ->
  (int * Eliom_common.user_scope * bool option) ->
  string

val register_delayed_post_coservice :
  sp:Eliom_common.server_params ->
  (int * Eliom_common.user_scope * bool option) ->
  Eliom_common.att_key_serv -> string

(* used by eliommod_mkform *)
type send_appl_content =
  | XNever
  | XAlways
  | XSame_appl of string
(** Whether the service is capable to send application content or not.
    (application content has type Eliom_services.eliom_appl_answer:
    content of the application container, or xhr redirection ...).
    A link towards a service with send_appl_content = XNever will
    always answer a regular http frame (this will stop the application if
    used in a regular link or form, but not with XHR).
    XAlways means "for all applications" (like redirections/actions).
    XSame_appl means "only for this application".
    If there is a client side application, and the service has
    XAlways or XSame_appl when it is the same application,
    then the link (or form or change_page) will expect application content.
*)

(** Returns the name of the application to which belongs the service, if any. *)
val get_send_appl_content : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> send_appl_content

val need_process_cookies :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> bool

val appl_content_capable :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> bool

val xhr_with_cookies :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> bool
