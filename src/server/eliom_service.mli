(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_service.mli
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


(** Creation and manipulation of Eliom services.

    See the Eliom manual for a detailed introduction to the concept of
    {% <<a_manual chapter="server-services"|Eliom services>>%}.

    {% <<outline>> %}
 *)

open Ocsigen_extensions

open Eliom_lib
open Eliom_content_core
open Eliom_parameter


(** {2 Type definitions for services} *)

(** {3 Services kind} *)

(** {4 Internal or external} *)

(** An internal attached service could either be a [`Service] or a [`Coservice]. *)
type servcoserv = [ `Service | `Coservice ]

(** An attached service could either be an [`Internal] Eliom service or an
    abstraction for an [`External] service. *)
type attached_service_kind =
  [ `Internal of servcoserv
  | `External ]

(** {4 POST or GET parameters} *)

(** The kind of a service is [`Post] when there is at least one POST
    parameters. It is [`Get] otherwise. *)
type getpost = [ `Get | `Post ]

(** {4 Attached or Non-attached} *)

(** The abstract type for attached service representation.
    - ['a] is a subtype of {!attached_service_kind}
    - ['b] is a subtype of {!getpost} *)
type (+'a, +'b) a_s

(** The abstract type for non-attached service representation.
    - ['a] is a subtype of {!getpost} *)
type +'a na_s

(** The type [service_kind] describe all four kind of services:
    - external (attached) services
    - (internal) attached services
    - (internal) attached coservices
    - (internal) non-attached coservices
*)
type service_kind =
    [ `Attached of (attached_service_kind, getpost) a_s
    | `Nonattached of getpost na_s ]

(** {4 Common subtypes of [service_kind] } *)

(** Restriction of [service_kind] to services without POST parameters. *)
type get_service_kind =
    [ `Attached of (attached_service_kind, [ `Get ]) a_s
    | `Nonattached of [ `Get ] na_s ]

(** Restriction of [service_kind] to services with at least one POST parameters. *)
type post_service_kind =
    [ `Attached of (attached_service_kind, [ `Post ]) a_s
    | `Nonattached of [ `Post ] na_s ]

(** Restriction of [service_kind] to attached services. *)
type attached =
    [ `Attached of (attached_service_kind, getpost) a_s ]

(** Restriction of [service_kind] to non-attached services. *)
type nonattached =
    [ `Nonattached of getpost na_s ]

(** Restriction of [service_kind] to internal services. *)
type internal_service_kind =
    [ `Attached of ([ `Internal of servcoserv ], getpost) a_s
    | `Nonattached of getpost na_s ]


(** {3 Kind of parameters} *)

(** The kind of parameters for a service is [`WithSuffix] when it have
    a suffix parameter, for examples {!Eliom_parameter.suffix} or
    {!Eliom_parameter.suffix_prod}. Otherwise it is
    [`WithoutSuffix]. *)
type suff = [ `WithSuffix | `WithoutSuffix ]

(** {3 Registrable service} *)

(** A service is [`Registrable] only if it isn't a pre-applied
    service, see {!preapply}. *)
type registrable = [ `Registrable | `Unregistrable ]

(** {3 Abstract type of services} *)

(** Type of services.
    - [ 'a] is the type of GET parameters expected by the service.
    - [ 'b] is the type of POST parameters expected by the service.
    - [ 'c] describes the services's kind: attached or non-attached,
            internal or external, GET only or with POST
            parameters. It is a subtype of {!service_kind}.
    - the type paremeter of subtype {!suff} states the kind
      of parameters it uses: suffix or not.
    - [ 'd] is the type of GET parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_content.Html5.D.get_form}).
    - [ 'e] is the type of POST parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_content.Html5.D.post_form}).
    - the type parameter of subtype {!registrable} tells if it is possible to
      register a handler on this service.
    - [ 'f] is an information on what the service returns.
            See {!Eliom_registration.kind}.
*)
type ('a,'b,+'c,+'d,+'e,+'f,+'g,+'h) service
constraint 'd = [< suff ]
constraint 'g = [< registrable ]

(** Types of groups of services. *)

type http_service = [ `Http ]
type appl_service = [ `Appl ]
type 'a ocaml_service

(** The type [non_ocaml_service] is used as phantom type parameters for
    the {!Eliom_registration.kind}. It used to type functions that operates
    over service that do not returns OCaml values, like
    {!appl_self_redirect}. *)
type non_ocaml_service = [ appl_service | http_service ]

(** Helper for typing ocaml services *)
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
(** Module for creating services that returns ocaml values *)
module Ocaml : "sigs/eliom_service_with_external.mli"
  subst type returnB := 'rt ocaml_service
  and type returnT := 'rt ocaml_service
(** Default module for creating services *)
module Http : "sigs/eliom_service_with_external.mli"
  subst type returnB := [> http_service ]
  and type returnT := [< non_ocaml_service ]

(** The function [register_eliom_module mod f] is used to register the
    initialization function [f] to be executed when then module [mod]
    is "loaded" by Ocsigen server. The module [mod] could either be a
    dynamically loaded module or linked statically into the server: in
    each case, the [f] function will be invoked when the module is
    initialized in the configuration file using [<eliom name="name">
    ... </eliom>]. If [register_eliom_module] is called twice with the
    same module name, the second initialization function will replace
    the previous one. *)
val register_eliom_module : string -> (unit -> unit) -> unit

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



(** {3 Void non-attached coservices} *)


(** The service [void_coservice'] is a predefined non-attached action
    with special behaviour: it has no parameter at all, even
    non-attached parameters.  Use it if you want to make a link to the
    current page without non-attached parameters.  It is almost
    equivalent to a POST non-attached service without POST parameters,
    on which you register an action that does nothing, but you can use
    it with <a> links, not only forms.  It does not keep non attached
    GET parameters.  *)
val void_coservice' :
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service

(** Same as {!void_coservice'} but forcing https. *)
val https_void_coservice' :
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service

(** Same as {!void_coservice'} but keeps non attached GET parameters. *)
val void_hidden_coservice' :
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service

(** Same as {!void_hidden_coservice'} but forcing https. *)
val https_void_hidden_coservice' :
  (unit, unit, [> `Nonattached of [> `Get ] na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service


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

(** The function [add_non_localized_get_parameters ~params ~service]
    Adds non localized GET parameters [params] to [service]. See the
    Eliom manual for more information about {% <<a_manual
    chapter="server-params" fragment="nonlocalizedparameters"|non localized
    parameters>>%}. *)
val add_non_localized_get_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a * 'p, 'b, 'c, 'd, 'e * 'pn, 'f, 'g, 'return) service

(** Same as {!add_non_localized_get_parameters} but with POST
    parameters.*)
val add_non_localized_post_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'b * 'p, 'c, 'd, 'e, 'f * 'pn, 'g, 'return) service

(** The function [unregister service] unregister the service handler
    previously associated to [service] with
    {!Eliom_registration.Html5.register},
    {!Eliom_registration.App.register} or any other
    {!Eliom_registration}[.*.register] functions. See the documentation of
    those functions for a description of the [~scope] and [~secure]
    optional parameters. *)
val unregister :
  ?scope:[< Eliom_common.scope ] ->
  ?secure:bool ->
  ('a, 'b, [< `Attached of ([> `Internal of 'c ], [< `Get | `Post ]) a_s
	   | `Nonattached of 'd na_s ], 'e, 'f, 'g, 'h, 'return) service ->
  unit


(**/**)
val get_get_or_post :
  ('a, 'b,
   [< `Attached of (attached_service_kind, [< getpost]) a_s
   | `Nonattached of [< getpost ] na_s ], 'd, 'e, 'f, 'g, 'h) service ->
  getpost
val get_kind_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service -> 'c
val get_or_post_ : ('a, [< `Get | `Post ]) a_s ->
  Ocsigen_http_frame.Http_header.http_method
val get_pre_applied_parameters_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  (string * string) list String.Table.t *
  (string * string) list
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
(* val reconstruct_absolute_Url.path : Url.path -> Url.path -> Url.path option -> string
val reconstruct_relative_Url.path : Url.path -> Url.path -> Url.path option -> string
*)

val keep_nl_params : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  [ `All | `Persistent | `None ]

val change_get_num :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('h, 'hh) a_s ->
  Eliom_common.att_key_serv ->
  ('a, 'b, [> `Attached of ('h, 'hh) a_s ], 'd, 'e, 'f, 'i, 'return) service


val new_state : unit -> string

val untype_service_ : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'rr) service ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'return) service

(*****************************************************************************)

val register_delayed_get_or_na_coservice :
  sp:Eliom_common.server_params ->
  (int * [< Eliom_common.user_scope ] * bool option) ->
  string

val register_delayed_post_coservice :
  sp:Eliom_common.server_params ->
  (int * [< Eliom_common.user_scope ] * bool option) ->
  Eliom_common.att_key_serv -> string

val set_delayed_get_or_na_registration_function :
  Eliom_common.tables ->
  int ->
  (sp:Eliom_common.server_params -> string) -> unit

val set_delayed_post_registration_function :
  Eliom_common.tables ->
  int ->
  (sp:Eliom_common.server_params -> Eliom_common.att_key_serv -> string) ->
  unit

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

val set_send_appl_content :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'rr) service -> send_appl_content -> unit

(** Returns the name of the application to which belongs the service, if any. *)
val get_send_appl_content : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> send_appl_content

val xhr_with_cookies :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> string option option

val pre_wrap :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'rr) service ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'rr) service

val eliom_appl_answer_content_type : string

exception Wrong_session_table_for_CSRF_safe_coservice

val get_global_data : unit -> poly Eliom_lib_base.global_data
val get_request_data : unit -> request_data

module Syntax_helpers : sig

  (** Registers a client value datum for the next server section when
      executed in a global_data
      (cf. {!Eliom_service.Syntax_helpers.set_global}) or in the
      request_data when executed in a request. *)
  val client_value : int64 -> 'args -> 'a client_value

  (** All client values created between [set_global true] and
      [set_global false] are considered global client values
      (cf. <<a_manual chapter="clientserver-language" chapter="clientvalues"|the
      manual>>).  *)
  val set_global : bool -> unit

  (** Called at the end of each server or shared section. The argument
      identifies the compilation unit.

      Adds the list of recently registered
      {!Eliom_lib_base.client_value_datum}s into the queue of server
      section data of the compilation unit
      ({!Eliom_lib_base.compilation_unit_global_data}).

      Called in parallel with <<a_api
      subproject="client"|Eliom_client.Syntax_helpers.close_server_section>>.  *)
  val close_server_section : string -> unit

  (** Called at the end of every client or shared section. The first
      argument identifies the compilation unit. The second is the list
      of novel injections in that section.

      Adds a list of {!Eliom_lib_base.injection_datum}s into the queue
      of client section data of the compilation unit
      ({!Eliom_lib_base.compilation_unit_global_data}).

      Called in parallel with <<a_api
      subproject="client"|Eliom_client.Syntax_helpers.open_client_section>>.  *)
  val close_client_section : string -> (string * (unit -> poly)) list -> unit

  (** Convert any value to a {! Eliom_lib.escaped_value} for usage in
      the [args] argument to {! Eliom_service.Syntax_helpers.client_value}. *)
  val escaped_value : 'a -> escaped_value
end
