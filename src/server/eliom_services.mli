(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_services.mli
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
    {% <<a_manual chapter="services"|Eliom services>>%}.

    {% <<outline>> %}
 *)

open Ocsigen_extensions

open Eliom_pervasives
open Eliom_parameters


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
    a suffix parameter, for examples {!Eliom_parameters.suffix} or
    {!Eliom_parameters.suffix_prod}. Otherwise it is
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
    - [ 'd] is a phantom type, subtype of {!suff} stating the kind
            of parameters it uses: suffix or not.
    - [ 'e] is the type of GET parameters names. See {!Eliom_parameters.param_name} and
            form generation functions (e. g. {!Eliom_output.Html5.get_form}).
    - [ 'f] is the type of POST parameters names. See {!Eliom_parameters.param_name} and
            form generation functions (e. g. {!Eliom_output.Html5.post_form}).
    - [ 'g] is a phantom type,  subtype of {!registrable},
            telling if it is possible to register a handler
            on this service.
    - [ 'h] is an information on what the service returns.
            See {!Eliom_output.kind}.
*)
type ('a,'b,+'c,+'d,+'e,+'f,+'g,+'h) service
constraint 'd = [< suff ]
constraint 'g = [< registrable ]


(***** Static dir and actions do not depend on the type of pages ******)

(** {2 Registration of named modules}

    This functionality allows to register initialization functions for
    Eliom modules which will be executed when the corresponding module
    is loaded in [ocsigenserver.conf].

*)

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

(** {2 Definitions of services}

    {e Warning: These functions must be called when the site
    information is available, that is, either during a request or
    during the initialisation phase of the site.  Otherwise, it will
    raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.  If you are
    using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_services.register_eliom_module}. Otherwise you will also
    get this exception.}
*)

(** {3 Main services} *)

(** The function [service ~path ~get_params ()] creates a {!service}
    associated to the path [path] and taking [get_params] as GET
    parameters.

    If the optional parameter [~https:true] is given, all links
    towards that service will use https. By default, links will keep
    the current protocol.

    The optional parameter [~priority] allows to change the priority
    order between service that shares the same path. The default
    priority is 0 ; if you want the service to be tried before
    (resp. after) other services, put a higher (resp. lower) priority.

    If the optional parameter [~keep_nl_params:`Persistent]
    (resp. [~keep_nl_params:`All]) is given, all links towards that
    service will keep persistent (resp. all) non localized GET
    parameters of the current service. The default is [`None]. See the
    eliom manual for more information about {% <<a_manual
    chapter="params" fragment="nonlocalizedparameters"|non localized
    parameters>>%}.
*)
val service :
  ?https:bool ->
  path:Url.path ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?priority:int ->
  get_params:('get, [< suff ] as 'tipo,'gn) params_type ->
  unit ->
  ('get,unit,
   [> `Attached of
       ([> `Internal of [> `Service ] ], [> `Get ]) a_s ],
   'tipo,'gn,
   unit, [> `Registrable ], 'return) service



(** The function [external_service ~prefix ~path ~get_params ()]
    creates a service for an external web site, that will use GET
    method and requires [get_params] as parameters. This allows to
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


(** The function [post_service ~fallback ~post_params ()] creates a
    service that takes [post_params] as POST parameters and share the
    same path and GET parameters than the service [fallback].

    POST parameters couldn't contain a suffix parameter.

    The service [fallback] should be an (internal) attached service
    without POST parameters ; it couldn't be a preapplied service.
    This argument enforces the creation of an equivalent service (
    i.e. a service with the same path and GET parameters ) to be
    served when POST parameters are missing. Thus, the user cannot put
    a bookmark on a page that does not exist.

    See {!service} for a description of optional [~https],
    [~keep_nl_params] and [~priority] parameters .
*)
val post_service :
  ?https:bool ->
  fallback: ('get, unit,
             [`Attached of
                 ([`Internal of
                     ([ `Service | `Coservice ] as 'kind) ], [`Get]) a_s ],
             [< suff] as 'tipo, 'gn, unit,
             [< `Registrable ], 'return1) service ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?priority:int ->
  post_params: ('post, [`WithoutSuffix], 'pn) params_type ->
  unit ->
  ('get, 'post, [> `Attached of
      ([> `Internal of 'kind ], [> `Post]) a_s ],
   'tipo, 'gn, 'pn, [> `Registrable ], 'return2) service


(** {3 Attached coservices} *)

(** The function [coservice ~fallback ~get_params] creates an {%
    <<a_manual chapter="service" fragment="coservices"|attached
    coservice>>%} at the same path than the service [fallback] and
    taking [get_params] as GET parameters.

    GET parameters of [coservice] couldn't contain a suffix
    parameter.

    The service [fallback] should be an (internal) attached service
    without any GET or POST parameters ; it could be a preapplied
    service.

    The optional [~name] parameter Coservices can be named if the [?name] optional parameter is present
    or anonymous (in that case, a coservice number will be generated).

    The optional [~timeout] parameter specifies a timeout (in
    seconds) after which the coservice will disappear. This amount of
    time is computed from the creation or from the last call to the
    service. The default is "no timeout". The optional [~max_use]
    parameter specifies that the service can be used only a fixed
    number of times. The default is "no limitation".

    If the optional [~csrf_safe] parameter is [true], it will create a
    {% <<a_manual chapter="services" fragment="csrf"|"CSRF-safe"
    service>>%}. In that case the [~name] parameter is ignored. The
    default is [false].

    The [~csrf_scope] and [~csrf_secure], if present, should
    respectively correspond to the [~scope] and [~secure] parameters
    that will be given to the associated [register]
    function. Otherwise the registration will fail with
    {Eliom_services.Wrong_session_table_for_CSRF_safe_coservice}. See
    {!Eliom_output.Html5.register},
    {!Eliom_output.Eliom_appl.register} or any other
    {!Eliom_output}[.*.register] functions for a description of those
    parameters.

    See {!service} for a description of the optional [~https] and
    [~keep_nl_params] parameters .
*)
val coservice :
  ?name: string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [< Eliom_common.user_scope] ->
  ?csrf_secure: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  fallback:
    (unit, unit, [ `Attached of ([ `Internal of [ `Service ] ], [`Get]) a_s ],
     [ `WithoutSuffix ] as 'tipo,
     unit, unit, [< registrable ], 'return1) service ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:
    ('get,[`WithoutSuffix],'gn) params_type ->
  unit ->
  ('get,unit,[> `Attached of
      ([> `Internal of [> `Coservice] ], [> `Get]) a_s ],
   'tipo, 'gn, unit,
   [> `Registrable ], 'return2) service

(** The function [post_coservice ~fallback ~get_params] creates an {%
    <<a_manual chapter="service" fragment="coservices"|attached
    coservice>>%} with the same path and GET parameters than the
    service [fallback] and taking [post_params] as POST
    parameters.

    POST parameters couldn't contain a suffix parameter.

    The service [fallback] should be an (internal) attached service or
    coservice without any POST parameters ; it could be a preapplied
    service.

    See {!coservice} for a description of optional parameters. *)
val post_coservice :
  ?name: string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [< Eliom_common.user_scope] ->
  ?csrf_secure: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  fallback: ('get, unit, [ `Attached of
                             ([`Internal of [<`Service | `Coservice] ],
                              [`Get]) a_s ],
             [< suff ] as 'tipo,
             'gn, unit, [< `Registrable ], 'return1) service ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  post_params: ('post, [`WithoutSuffix], 'pn) params_type ->
  unit ->
  ('get, 'post,
   [> `Attached of
      ([> `Internal of [> `Coservice] ], [> `Post]) a_s ],
   'tipo, 'gn, 'pn, [> `Registrable ], 'return2) service

(** {3 Non attached coservices} *)

(** The function [coservice' ~get_param] creates a {% <<a_manual
    chapter="services" fragment="non-attached_coservices"|non-attached
    coservice>>%} taking [get_params] as extra GET parameters.

    GET parameters of [coservice'] couldn't contain a suffix
    parameter.

    See {!service} for a description of the optional [~https] and
    [~keep_nl_params] parameters ; see {!coservice} for others
    optional parameters.
*)
val coservice' :
  ?name:string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [< Eliom_common.user_scope] ->
  ?csrf_secure: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:
    ('get, [`WithoutSuffix], 'gn) params_type ->
  unit ->
  ('get, unit, [> `Nonattached of [> `Get] na_s ],
   [`WithoutSuffix], 'gn, unit, [> `Registrable ], 'return) service


(** The function [post_coservice' ~get_param] creates a {% <<a_manual
    chapter="services" fragment="non-attached_coservices"|non-attached
    coservice>>%} taking [post_params] as POST parameters.

    POST parameters couldn't contain a suffix parameter.

    If the optional parameter [~keep_get_na_params] is [false], GET
    non-attached parameters of the current page won't be kept in the
    URL (if any) when you create a POST form to this coservice. The
    default is true.

    See {!service} for a description of the optional [~https] and
    [~keep_nl_params] parameters ; see {!coservice} for others
    optional parameters.
*)
val post_coservice' :
  ?name:string ->
  ?csrf_safe: bool ->
  ?csrf_scope: [< Eliom_common.user_scope] ->
  ?csrf_secure: bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  ?keep_get_na_params:bool ->
  post_params: ('post, [`WithoutSuffix], 'pn) params_type ->
  unit ->
  (unit, 'post,
   [> `Nonattached of [> `Post ] na_s ],
   [ `WithoutSuffix ], unit, 'pn, [> `Registrable ], 'return) service



(** {2 Predefined services} *)

(** {3 Static files} *)

(** The predefined service [static_dir] allows to create links to
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

(** Like [static_dir], but allows to put GET parameters *)
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


(** The service [void_coservice'] is a predefined non-attached action
    with special behaviour: it has no parameter at all, even
    non-attached parameters.  Use it if you want to make a link to the
    current page without non-attached parameters.  It is almost
    equivalent to a POST non-attached service without POST parameters,
    on which you register an action that does nothing, but you can use
    it with <a> links, not only forms.  It does not keep non attached
    GET parameters.  *)
val void_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service

(** Same as {!void_coservice'} but forcing https. *)
val https_void_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service

(** Same as {!void_coservice'} but keeps non attached GET parameters. *)
val void_hidden_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
  service

(** Same as {!void_hidden_coservice'} but forcing https. *)
val https_void_hidden_coservice' :
  (unit, unit, [> `Nonattached of 'a na_s ],
   [ `WithoutSuffix ],
   unit, unit, [> `Unregistrable ], 'return)
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

(** The function [add_non_localized_get_parameters ~params ~service]
    Adds non localized GET parameters [params] to [service]. See the
    Eliom manual for more information about {% <<a_manual
    chapter="params" fragment="nonlocalizedparameters"|non localized
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
    {!Eliom_output.Html5.register},
    {!Eliom_output.Eliom_appl.register} or any other
    {!Eliom_output}[.*.register] functions. See the documentation of
    those functions for a description of the [~scope] and [~secure]
    optionnal parameters. *)
val unregister :
  ?scope:[< Eliom_common.scope ] ->
  ?secure:bool ->
  ('a, 'b, [< `Attached of ([> `Internal of 'c ], [< `Get | `Post ]) a_s
	   | `Nonattached of 'd na_s ], 'e, 'f, 'g, 'h, 'return) service ->
  unit


(** {2 Eliom application services} *)

(** The function [onload evt] will register the function [evt] to be
    executed on client side once the [Eliom_appl] page generated by
    the current service handler is loaded. This function should not be
    called outside of a service handler.

    Use it with Eliom's syntax extension for client side code, for
    example:

    [onload {{ Firebug.console.debug(Js.string "Hello world!") }}].
*)
val onload : Dom_html.event XML.caml_event_handler -> unit

(** The function [onunload evt] will register the function [evt] to be
    executed on client side when leaving the [Eliom_appl] page
    generated by the current service handler. This function should not
    be called outside of a service handler.

    Use it with Eliom's syntax extension for client side code, for
    example:

    [onunload {{ Firebug.console.debug(Js.string "Goodbye!") }}].*)
val onunload : Dom_html.event XML.caml_event_handler -> unit








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

val set_send_appl_content :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'rr) service -> send_appl_content -> unit

(** Returns the name of the application to which belongs the service, if any. *)
val get_send_appl_content : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> send_appl_content

val xhr_with_cookies :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) service -> string option option

val get_onload : Eliom_common.server_params -> Dom_html.event XML.caml_event_handler list
val get_onunload : Eliom_common.server_params -> Dom_html.event XML.caml_event_handler list

val pre_wrap :
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'rr) service ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'rr) service

val eliom_appl_answer_content_type : string

exception Wrong_session_table_for_CSRF_safe_coservice
