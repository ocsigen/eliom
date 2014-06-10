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

open Eliom_parameter
open Eliom_lib

(** {2 Types of services} *)

type suff = [ `WithSuffix | `WithoutSuffix ]

type getpost = [ `Get | `Post | `Put | `Delete ]
      (* `Post means that there is at least one post param
         (possibly only the state post param).
         `Get doesn't have any body content.
         `Put means the HTTP PUT method has been used.
         `Delete means the HTTP DELETE method has been used.
         `Put and `Delete have [raw_post_data] as body content.
       *)

type registrable = [ `Registrable | `Unregistrable ]
(** You can call register function only on registrable services *)
(* Registrable means not pre-applied *)

type internal_service_kind =
  [ `Service
  | `AttachedCoservice
  | `NonattachedCoservice ]

type service_kind =
  [ internal_service_kind
  | `External ]


type a_s
type na_s
(** An attached service could either be an [`Internal] Eliom service or an
    abstraction for an [`External] service. *)
type attached_kind = [ `Attached of a_s ]
type non_attached_kind = [ `Nonattached of na_s ]
type attached = [ attached_kind | non_attached_kind ]

(** Type of services.
    - [ 'get] is the type of GET parameters expected by the service.
    - [ 'post] is the type of POST parameters expected by the service.
    - [ 'meth] the HTTP method
    - [ 'attached] attached or non-attached
    - [ 'kind] describes the services's kind : service, coservice, external. It is a subtype of {!service_kind}.
    - [ 'tipo] is a phantom type, subtype of {!suff} stating the kind
            of parameters it uses: suffix or not.
    - [ 'gn] is the type of GET parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_registration.Html5.get_form}).
    - [ 'pn] is the type of POST parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_registration.Html5.post_form}).
    - [ 'reg] is a phantom type,  subtype of {!registrable},
            telling if it is possible to register a handler
            on this service.
    - [ 'ret] is an information on what the service returns.
            See {!Eliom_registration.kind}.
*)
type ('get,'post,+'meth,+'attached,+'kind,+'tipo,'gn,'pn,+'reg,+'ret) service
  constraint 'meth = [< getpost ]
  constraint 'attached = [< attached]
  constraint 'kind = [< service_kind ]
  constraint 'tipo = [< suff ]
  constraint 'reg = [< registrable ]

(** Types of groups of services. *)
type http_service = [ `Http ]
type appl_service = [ `Appl ]
type 'a ocaml_service

type get_service_kind = [`Get]
type post_service_kind = [`Post]
type put_service_kind = [`Put]
type delete_service_kind = [`Delete]


(** The type [non_ocaml_service] is used as phantom type parameters for
    the {!Eliom_registration.kind}. It used to type functions that operates
    over service that do not returns OCaml values, like
    {!appl_self_redirect}. *)
type non_ocaml_service = [ appl_service | http_service ]

(** Helper for typing OCaml services *)
type 'rt rt
val rt : 'rt rt

(***** Static dir and actions do not depend on the type of pages ******)

(** {2 Registration of named modules}

    This functionality allows one to register initialization functions for
    Eliom modules which will be executed when the corresponding module
    is loaded in [ocsigenserver.conf].

*)

module Unsafe : "sigs/eliom_service_with_external.mli"
  subst type returnB := 'returnB
  and type returnT := 'returnT
(** Module for creating services that are applications *)
module App : "sigs/eliom_service.mli"
  subst type returnB := [> appl_service ]
  and type returnT := [< non_ocaml_service ]
(** Module for creating services that returns OCaml values *)
module Ocaml : "sigs/eliom_service_with_external.mli"
  subst type returnB := 'rt ocaml_service
  and type returnT := 'rt ocaml_service
(** Default module for creating services *)
module Http : "sigs/eliom_service_with_external.mli"
  subst type returnB := [> http_service ]
  and type returnT := [< non_ocaml_service ]



(** {2 Predefined services} *)

(** {3 Static files} *)

(** The predefined service [static_dir] allows one to create links to
    static files. This service takes the name of a static file as a
    parameter (a string list, slash separated). The actual directory
    in filesystem where static pages will be found must be set up in
    the configuration file with the staticmod extension. *)
val static_dir :
  unit ->
  (string list, unit, [> `Get], [> attached_kind], [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ], 'return)
    service

(** Same as {!static_dir} but forcing https link. *)
val https_static_dir :
  unit ->
  (string list, unit, [> `Get], [> attached_kind], [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ], 'return)
    service

(** Like [static_dir], but allows one to put GET parameters *)
val static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit, [> `Get], [> attached_kind], [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ], 'return)
    service

(** Same as {!static_dir_with_params} but forcing https link. *)
val https_static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit, [> `Get], [> attached_kind], [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ], 'return)
    service


(** {3 Void non-attached coservices} *)

val void_coservice' :
  (unit, unit, [> `Get], [> non_attached_kind], [> `NonattachedCoservice],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
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
  (unit, unit, [> `Get], [> non_attached_kind], [> `NonattachedCoservice],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service
(** The same, but forcing https. *)

val void_hidden_coservice' :
  (unit, unit, [> `Get], [>non_attached_kind], [> `NonattachedCoservice],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service
(** Same as [void_coservice'] but keeps non attached GET parameters.
 *)

val https_void_hidden_coservice' :
  (unit, unit, [> `Get], [>non_attached_kind], [> `NonattachedCoservice],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service
(** The same, but forcing https. *)


(** {2 Miscellaneous} *)

(** The function [preapply ~service paramaters] creates a new service
    by preapplying [service] to the GET [parameters]. It is not
    possible to register a handler on an preapplied service ;
    preapplied services may be used in links or as fallbacks for
    coservices *)
val preapply :
  service:('a, 'b, 'meth,[> attached_kind] as 'att,'kind, [< suff ], 'e, 'f, 'g, 'return) service ->
  'a ->
  (unit, 'b, 'meth,'att,'kind, [ `WithoutSuffix ], unit, 'f, [> `Unregistrable ], 'return) service

(** [attach_coservice' ~fallback ~service] attaches the non-attached
    coservice [service] on the URL of [fallback]. This allows to
    create a link to a non-attached coservice but with another URL
    than the current one. It is not possible to register something
    on the service returned by this function. *)
val attach_coservice' :
  fallback:(unit, unit, [< `Get ],[< attached_kind],[< `AttachedCoservice | `Service ],
	   [< suff ], unit, unit, 'rg1, 'return1) service ->
  service: ('get, 'post, 'meth, [< non_attached_kind], [< `NonattachedCoservice],
            [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'rg2, 'return) service ->
  ('get, 'post, 'meth, [> attached_kind], [> `AttachedCoservice ],
   'sf, 'gn, 'pn, [< registrable > `Unregistrable ], 'return) service


(** {3 Localized parameters} *)

val add_non_localized_get_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'meth,'attach, 'kind, 'd, 'e, 'f, 'g, 'return) service ->
  ('a * 'p, 'b, 'meth, 'attach, 'kind, 'd, 'e * 'pn, 'f, 'g, 'return) service
(** Adds non localized GET parameters to a service *)

val add_non_localized_post_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'meth,'attach,'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'b * 'p, 'meth,'attach, 'c, 'd, 'e, 'f * 'pn, 'g, 'return) service
(** Adds non localized POST parameters to a service *)

(** {3 Static files} *)

(** The predefined service [static_dir] allows one to create links to
    static files. This service takes the name of a static file as a
    parameter (a string list, slash separated). The actual directory
    in filesystem where static pages will be found must be set up in
    the configuration file with the staticmod extension. *)
val static_dir :
  unit ->
  (string list, unit, [> `Get], [> attached_kind], [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ],
   [> http_service ])
    service

(** Same as {!static_dir} but forcing https link. *)
val https_static_dir :
  unit ->
  (string list, unit, [> `Get], [> attached_kind], [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ],
   [> http_service ])
    service

(** Like [static_dir], but allows one to put GET parameters *)
val static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit, [> `Get], [> attached_kind], [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ],
   [> http_service ])
    service

(** Same as {!static_dir_with_params} but forcing https link. *)
val https_static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit,
   [> `Get],
   [> attached_kind],
   [> `Service ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ],
   [> http_service ])
    service




(**/**)

(* used by Eliom_uri *)
val get_get_or_post :
  ('a, 'b,[<getpost] as 'c,[< attached],'kind, 'd, 'e, 'f, 'g, 'h) service -> 'c

val get_info_ : (_,_,_,'attached,_,_,_,_,_,_) service -> 'attached
val get_kind_ : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service -> service_kind
val get_pre_applied_parameters_ : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service ->
  (string * Eliommod_parameters.param) list String.Table.t *
  (string * Eliommod_parameters.param) list
val get_get_params_type_ : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'd, 'e) Eliom_parameter.params_type
val get_post_params_type_ : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service ->
  ('b, [ `WithoutSuffix ], 'f) Eliom_parameter.params_type
val get_sub_path_ : a_s -> Url.path
val get_full_path_ : a_s -> Url.path
val get_prefix_ :   a_s -> string
val get_get_name_ : a_s -> Eliom_common.att_key_serv
val get_post_name_ : a_s -> Eliom_common.att_key_serv
val get_redirect_suffix_ : a_s -> bool
val get_na_name_ : na_s -> Eliom_common.na_key_serv
val get_na_keep_get_na_params_: na_s -> bool
val get_max_use_ : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service -> int option
val get_timeout_ : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service -> float option
val get_https : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service -> bool
val get_priority_ : a_s -> int

val keep_nl_params : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service ->
  [ `All | `Persistent | `None ]

val change_get_num :
  ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service ->
  a_s ->
  Eliom_common.att_key_serv ->
  ('a, 'b, 'meth,[> attached_kind ], 'kind, 'd, 'e, 'f, 'i, 'return) service

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
  | XSame_appl of string * string option
(** Whether the service is capable to send application content or not.
    (application content has type Eliom_service.eliom_appl_answer:
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
val get_send_appl_content : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'h) service -> send_appl_content

val xhr_with_cookies :
  ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'h) service -> string option option
