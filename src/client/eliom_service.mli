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
open Eliom_content_core

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

(** Type of services.
    - [ 'a] is the type of GET parameters expected by the service.
    - [ 'b] is the type of POST parameters expected by the service.
    - [ 'c] describes the services's kind: attached or non-attached,
            internal or external, GET only or with POST
            parameters. It is a subtype of {!service_kind}.
    - [ 'd] is a phantom type, subtype of {!suff} stating the kind
            of parameters it uses: suffix or not.
    - [ 'e] is the type of GET parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_registration.Html5.get_form}).
    - [ 'f] is the type of POST parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_registration.Html5.post_form}).
    - [ 'g] is a phantom type,  subtype of {!registrable},
            telling if it is possible to register a handler
            on this service.
    - [ 'h] is an information on what the service returns.
            See {!Eliom_registration.kind}.
*)
type ('a,'b,+'c,+'d,+'e,+'f,+'g,+'h) service
constraint 'd = [< suff ]
constraint 'g = [< registrable ]

(** Types of groups of services. *)
type http_service = [ `Http ]
type appl_service = [ `Appl ]
type 'a caml_service

(** The type [non_caml_service] is used as phantom type parameters for
    the {!Eliom_registration.kind}. It used to type functions that operates
    over service that do not returns OCaml values, like
    {!appl_self_redirect}. *)
type non_caml_service = [ appl_service | http_service ]

(** Helper for typing caml services *)
type 'rt rt
val rt : 'rt rt

(***** Static dir and actions do not depend on the type of pages ******)

(** {2 Registration of named modules}

    This functionality allows one to register initialization functions for
    Eliom modules which will be executed when the corresponding module
    is loaded in [ocsigenserver.conf].

*)

module Unsafe : "../server/sigs/eliom_service.mli"
  subst type returnB := 'returnB
  and type returnT := 'returnT
(** Module for creating services that are applications *)
module App : "../server/sigs/eliom_service.mli"
  subst type returnB := [> appl_service ]
  and type returnT := [< non_caml_service ]
(** Module for creating services that returns caml values *)
module OCaml : "../server/sigs/eliom_service.mli"
  subst type returnB := 'rt caml_service
  and type returnT := 'rt caml_service
(** Default module for creating services *)
module Http : "../server/sigs/eliom_service.mli"
  subst type returnB := [> http_service ]
  and type returnT := [< non_caml_service ]

(** {2 Definitions of services}

    {e Warning: These functions must be called when the site
    information is available, that is, either during a request or
    during the initialisation phase of the site.  Otherwise, it will
    raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.  If you are
    using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_service.register_eliom_module}. Otherwise you will also
    get this exception.}
*)

(** The function [external_service ~prefix ~path ~get_params ()]
    creates a service for an external web site, that will use GET
    method and requires [get_params] as parameters. This allows one to
    creates links or forms towards other Web sites using Eliom's
    syntax.

    The parameter labelled [~path] is the URL path. Each element of
    the list will be URL-encoded.

    The parameter labelled [~prefix] contains all what you want to put
    before the path. It usually starts with "http://" plus the name of
    the server. The prefix is not URL encoded.

    The whole URL is constructed from the prefix, the path and GET
    parameters. Hence, an empty prefix can be used to make a link to
    another site of the same server.

    See {!val:service} for a description of the optional
    [~keep_nl_params] parameter.
*)
val external_service :
  prefix: string ->
  path:Url.path ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  unit ->
  ('get, unit, [> `Attached of ([> `External ], [> `Get ]) a_s ], 'tipo,
   'gn, unit, [> `Unregistrable ], 'return) service


(** Same as {!external_service} but with POST method. *)
val external_post_service :
  prefix: string ->
  path:Url.path ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
  post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
  unit ->
  ('get, 'post, [> `Attached of ([> `External ], [> `Post ]) a_s ], 'tipo,
   'gn, 'pn, [> `Unregistrable ], 'return) service



(** {2 Predefined services} *)

(** {3 Static files} *)

(** The predefined service [static_dir] allows one to create links to
    static files. This service takes the name of a static file as a
    parameter (a string list, slash separated). The actual directory
    in filesystem where static pages will be found must be set up in
    the configuration file with the staticmod extension. *)
val static_dir :
  unit ->
  (string list, unit, [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ], 'return)
    service

(** Same as {!static_dir} but forcing https link. *)
val https_static_dir :
  unit ->
  (string list, unit, [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ], 'return)
    service

(** Like [static_dir], but allows one to put GET parameters *)
val static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit,
   [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ], 'return)
    service

(** Same as {!static_dir_with_params} but forcing https link. *)
val https_static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit,
   [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ], 'return)
    service


(** {3 Void non-attached coservices} *)

val void_coservice' :
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_caml_service ])
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
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_caml_service ])
  service
(** The same, but forcing https. *)

val void_hidden_coservice' :
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_caml_service ])
  service
(** Same as [void_coservice'] but keeps non attached GET parameters.
 *)

val https_void_hidden_coservice' :
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
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
  service:('a, 'b, [> `Attached of ('d, 'dd) a_s ] as 'c,
	   [< suff ], 'e, 'f, 'g, 'return)
  service ->
  'a ->
  (unit, 'b, 'c,
   [ `WithoutSuffix ], unit, 'f, [> `Unregistrable ], 'return) service

(** [attach_coservice' ~fallback ~service] attaches the non-attached
    coservice [service] on the URL of [fallback]. This allows to
    create a link to a non-attached coservice but with another URL
    than the current one. It is not possible to register something
    on the service returned by this function. *)
val attach_coservice' :
  fallback:(unit, unit, [< `Attached of ([< `Internal of 'sc1 ],
                                         [< `Get ]) a_s ],
	   [< suff ], unit, unit, 'rg1, 'return1) service ->
  service: ('get, 'post, [< `Nonattached of 'gp na_s ],
            [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'rg2, 'return) service ->
  ('get, 'post, [> `Attached of ([> `Internal of [> `Coservice ] ],
                                 'gp) a_s ],
   'sf, 'gn, 'pn, [< registrable > `Unregistrable ], 'return) service


(** {3 Localized parameters} *)

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

(** {3 Static files} *)

(** The predefined service [static_dir] allows one to create links to
    static files. This service takes the name of a static file as a
    parameter (a string list, slash separated). The actual directory
    in filesystem where static pages will be found must be set up in
    the configuration file with the staticmod extension. *)
val static_dir :
  unit ->
  (string list, unit, [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ],
   [> http_service ])
    service

(** Same as {!static_dir} but forcing https link. *)
val https_static_dir :
  unit ->
  (string list, unit, [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, [> `Unregistrable ],
   [> http_service ])
    service

(** Like [static_dir], but allows one to put GET parameters *)
val static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit,
   [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
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
   [> `Attached of
      ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, [> `Unregistrable ],
   [> http_service ])
    service




(**/**)

(* used by Eliom_uri *)
val get_get_or_post :
  ('a, 'b,
   [< `Attached of (attached_service_kind, [< getpost]) a_s
   | `Nonattached of [< getpost ] na_s ], 'd, 'e, 'f, 'g, 'h) service ->
  getpost

val get_kind_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service -> 'c
val get_pre_applied_parameters_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  (string * Eliommod_parameters.param) list String.Table.t *
  (string * Eliommod_parameters.param) list
val get_get_params_type_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'd, 'e) Eliom_parameter.params_type
val get_post_params_type_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('b, [ `WithoutSuffix ], 'f) Eliom_parameter.params_type
val get_att_kind_ : ('a, 'b) a_s -> 'a
val get_sub_path_ : ('a, 'b) a_s -> Url.path
val get_full_path_ : ('a, 'b) a_s -> Url.path
val get_prefix_ : ('a, 'b) a_s -> string
val get_get_name_ : ('a, 'b) a_s -> Eliom_common.att_key_serv
val get_post_name_ : ('a, 'b) a_s -> Eliom_common.att_key_serv
val get_redirect_suffix_ : ('a, 'b) a_s -> bool
val get_na_name_ : 'a na_s -> Eliom_common.na_key_serv
val get_na_kind_ : [< getpost ] na_s -> [ `Get | `Post of bool ]
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
val get_send_appl_content : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> send_appl_content

val xhr_with_cookies :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> string option option
