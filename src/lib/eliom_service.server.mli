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


(** Creation and manipulation of Eliom services. *)

(**

    {b See the Eliom manual for a detailed introduction to the concept of
    {% <<a_manual chapter="server-services"|Eliom services>>%}.
    }

  {% <<outline| <<header| **Table of contents** >> >>%}
*)

(**
   The main functions to create services are in modules
   {% <<a_api subproject="server"|module Eliom_service.Http>>%} (default),
   {% <<a_api subproject="server"|module Eliom_service.Ocaml>>%}
   (for services returning OCaml values) and
   {% <<a_api subproject="server"|module Eliom_service.App>>%}
   (for services belonging to an Eliom client-server app).

 *)

open Eliom_lib
open Eliom_parameter


(** {2 Type definitions for services} *)

(** {3 Services kind} *)

(** The type [service_kind] describe all four kind of services:
    - external (attached) services
    - (internal) attached services
    - (internal) attached coservices
    - (internal) non-attached coservices
*)

(** An internal attached service could either be a [`Service] or a [`AttachedCoservice]. *)
type internal_attached_service_kind =
  [ `Service
  | `AttachedCoservice ]

(** An internal service could either be an internal attached service or a [`NonattachedCoservice]. *)
type internal_service_kind =
  [ internal_attached_service_kind
  | `NonattachedCoservice ]

(** An attached service could either be an internal Eliom service or an
    abstraction for an [`External] service. *)
type service_kind =
  [ internal_service_kind
  | `External ]

(** {3 Attached or Non-attached} *)

(** The abstract type for attached service representation. *)
type a_s

(** The abstract type for non-attached service representation. *)
type na_s

type attached_kind = [ `Attached of a_s ]
type non_attached_kind = [ `Nonattached of na_s ]
type attached = [ attached_kind | non_attached_kind ]


(** {3 POST or GET parameters} *)

(** The kind of a service is [`Post] when there is at least one POST
    parameters. It is [`Get] otherwise. *)
type service_method = [ `Get | `Post | `Put | `Delete ]

(** {3 Common subtypes of [service_method] } *)

(** Restriction of [service_method] to GET services. *)
type get_service_kind = [`Get]

(** Restriction of [service_method] to POST services. *)
type post_service_kind = [`Post]

(** Restriction of [service_method] to PUT services. *)
type put_service_kind = [`Put]

(** Restriction of [service_method] to DELETE services. *)
type delete_service_kind = [`Delete]

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
    - [ 'get] is the type of GET parameters expected by the service.
    - [ 'post] is the type of POST parameters expected by the service.
    - [ 'meth] the HTTP method
    - [ 'attached] attached or non-attached
    - [ 'kind] describes the services's kind : service, coservice, external. It is a subtype of {!service_kind}.
    - [ 'tipo] the type paremeter of subtype {!suff} states the kind
      of parameters it uses: suffix or not.
    - [ 'gn] is the type of GET parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_content.Html5.D.get_form}).
    - [ 'pn] is the type of POST parameters names. See {!Eliom_parameter.param_name} and
            form generation functions (e. g. {!Eliom_content.Html5.D.post_form}).
    - [ 'reg] the type parameter of subtype {!registrable} tells if it is possible to
      register a handler on this service.
    - [ 'ret] is an information on what the service returns.
            See {!Eliom_registration.kind}.
*)
type ('get,'post,+'meth,+'attached,+'kind,+'tipo,'gn,'pn,+'reg,+'ret) service
  constraint 'meth = [< service_method ]
  constraint 'attached = [< attached]
  constraint 'kind = [< service_kind ]
  constraint 'tipo = [< suff ]
  constraint 'reg = [< registrable ]

(** Types of groups of services. *)

type http_service = [ `Http ]
type appl_service = [ `Appl ]
type 'a ocaml_service

(** The type [non_ocaml_service] is used as phantom type parameters for
    the {!Eliom_registration.kind}. It used to type functions that operates
    over service that do not returns OCaml values, like
    {!appl_self_redirect}. *)
type non_ocaml_service = [ appl_service | http_service ]

(** Helper for typing OCaml services.
    In some cases, you may need to write the return type of the
    service manually. Instead of writing the full type of the service,
    (which may be huge), add a type constraint for parameter [?rt] of service
    creation functions
    (like <<a_api subproject="server"|fun Eliom_service.Http.service>>),
    using the following value.

*)
type 'rt rt
val rt : 'rt rt

(***** Static dir and actions do not depend on the type of pages ******)

(** {2 Registration of services} *)


(** Default module for creating services *)
module Http : "sigs/eliom_service_with_external.mli"
  subst type returnB := [> http_service ]
  and type returnT := [< non_ocaml_service ]

(** Module for creating services returning applications *)
module App : "sigs/eliom_service.mli"
  subst type returnB := [> appl_service ]
  and type returnT := [< non_ocaml_service ]

(** Module for creating services that return OCaml values *)
module Ocaml : "sigs/eliom_service_with_external.mli"
  subst type returnB := 'rt ocaml_service
  and type returnT := 'rt ocaml_service

(** Module for creating services without specifying the return type *)
module Unsafe : "sigs/eliom_service_with_external.mli"
  subst type returnB := 'returnB
  and type returnT := 'returnT


(** {2 Static loading of Eliom modules} *)

(**
    This functionality allows one to register initialization functions for
    Eliom modules which will be executed when the corresponding module
    is loaded in [ocsigenserver.conf].
    If the module is loaded dynamically, you probably don't need this.
   But if the module is linked statically, some computations,
   like service registrations must be delayed.

*)

(** The function [register_eliom_module mod f] is used to register the
    initialization function [f] to be executed when then module [mod]
    is loaded by Ocsigen server. The module [mod] could either be a
    dynamically loaded module or linked statically into the server: in
    each case, the [f] function will be invoked when the module is
    initialized in the configuration file using [<eliommodule ...>
    ... </eliommodule>]. If [register_eliom_module] is called twice with the
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
  (string list, unit, [> `Get],[> attached_kind], [> `Service ],
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
  ((string list * 'a), unit, [> `Get], [> attached_kind], [> `Service ],
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
  (unit, unit, [> `Get], [> non_attached_kind], [> `NonattachedCoservice],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service

(** Same as {!void_coservice'} but forcing https. *)
val https_void_coservice' :
  (unit, unit, [> `Get], [> non_attached_kind], [> `NonattachedCoservice],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service

(** Same as {!void_coservice'} but keeps non attached GET parameters. *)
val void_hidden_coservice' :
  (unit, unit, [> `Get], [> non_attached_kind], [> `NonattachedCoservice],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], [> non_ocaml_service ])
  service

(** Same as {!void_hidden_coservice'} but forcing https. *)
val https_void_hidden_coservice' :
  (unit, unit, [> `Get], [> non_attached_kind], [> `NonattachedCoservice],
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
  service:('a, 'b, 'meth, [> attached_kind] as 'attached, 'kind,
	   [< suff ], 'e, 'f, 'g, 'return)
  service ->
  'a ->
  (unit, 'b, 'meth, 'attached, 'kind,
     [ `WithoutSuffix ], unit, 'f, [> `Unregistrable ], 'return) service

(** [attach_coservice' ~fallback ~service] attaches the non-attached
    coservice [service] on the URL of [fallback]. This allows to
    create a link to a non-attached coservice but with another URL
    than the current one. It is not possible to register something
    on the service returned by this function. *)
val attach_coservice' :
  fallback:(unit, unit, [< `Get ], [< attached_kind], [< `Service | `AttachedCoservice ],
	   [< suff ], unit, unit, 'rg1, 'return1) service ->
  service: ('get, 'post, 'meth, [< non_attached_kind], [< `NonattachedCoservice],
            [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'rg2, 'return) service ->
  ('get, 'post, 'meth, [> attached_kind], [> `AttachedCoservice ],
   'sf, 'gn, 'pn, [< registrable > `Unregistrable ], 'return) service

(** The function [add_non_localized_get_parameters ~params ~service]
    Adds non localized GET parameters [params] to [service]. See the
    Eliom manual for more information about {% <<a_manual
    chapter="server-params" fragment="nonlocalizedparameters"|non localized
    parameters>>%}. *)
val add_non_localized_get_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'meth, 'attached, 'kind, 'd, 'e, 'f, 'g, 'return) service ->
  ('a * 'p, 'b, 'meth, 'attached, 'kind, 'd, 'e * 'pn, 'f, 'g, 'return) service

(** Same as {!add_non_localized_get_parameters} but with POST
    parameters.*)
val add_non_localized_post_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'meth, 'attached, 'kind, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'b * 'p, 'meth, 'attached, 'kind, 'd, 'e, 'f * 'pn, 'g, 'return) service

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
  ('a, 'b, _, _, [> internal_service_kind], 'e, 'f, 'g, 'h, 'return) service -> unit


(**/**)

val get_get_or_post : ('a, 'b,[<service_method] as 'c,[< attached],'kind, 'd, 'e, 'f, 'g, 'h) service -> 'c

val get_info_ : ('a, 'b, 'meth,'attach,'kind, 'd, 'e, 'f, 'g, 'return) service -> 'attach
val get_kind_ : ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service -> service_kind
val get_or_post_ :('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service -> Ocsigen_http_frame.Http_header.http_method

val get_pre_applied_parameters_ : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  (string * string) list String.Table.t *
  (string * string) list
val get_get_params_type_ : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'd, 'e) Eliom_parameter.params_type
val get_post_params_type_ : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  ('b, [ `WithoutSuffix ], 'f) Eliom_parameter.params_type
val get_sub_path_ : a_s -> Url.path
val get_full_path_ : a_s -> Url.path
val get_prefix_ :   a_s -> string
val get_get_name_ : a_s -> Eliom_common.att_key_serv
val get_post_name_ : a_s -> Eliom_common.att_key_serv
val get_redirect_suffix_ : a_s -> bool
val get_na_name_ : na_s -> Eliom_common.na_key_serv
val get_na_keep_get_na_params_: na_s -> bool
val get_max_use_ : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service -> int option
val get_timeout_ : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service -> float option
val get_https : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service -> bool
val get_priority_ : a_s -> int
(* val reconstruct_absolute_Url.path : Url.path -> Url.path -> Url.path option -> string
val reconstruct_relative_Url.path : Url.path -> Url.path -> Url.path option -> string
*)

val keep_nl_params : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service ->
  [ `All | `Persistent | `None ]

val change_get_num :
  ('a, 'b, 'meth,'attch,'kind, 'd, 'e, 'f, 'g, 'return) service ->
  a_s ->
  Eliom_common.att_key_serv ->
  ('a, 'b, 'meth,[> attached_kind ], 'kind, 'd, 'e, 'f, 'i, 'return) service

val new_state : unit -> string

val untype_service_ : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'rr) service ->
  ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'return) service

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
  ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'rr) service -> send_appl_content -> unit

(** Returns the name of the application to which belongs the service, if any. *)
val get_send_appl_content : ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'h) service -> send_appl_content

val xhr_with_cookies :
  ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'h) service -> string option option

val pre_wrap :
  ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'rr) service ->
  ('a, 'b, 'meth, 'attached, 'c, 'd, 'e, 'f, 'g, 'rr) service

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
