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

    See {!create} for the main service creation function.x *)

include Eliom_service_sigs.S

val create :
   ?name:string
  -> ?csrf_safe:bool
  -> ?csrf_scope:[< Eliom_common.user_scope]
  -> ?csrf_secure:bool
  -> ?max_use:int
  -> ?timeout:float
  -> ?https:bool
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> ?priority:int
  -> meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp_) meth
  -> path:('att, 'co, 'gp_) path_option
  -> unit
  -> ('gp, 'pp, 'm, 'att, 'co, non_ext, reg, 'tipo, 'gn, 'pn, non_ocaml) t
(** {b Service definition}

    The function [create ~id ~path ()] creates a service
    ({!Eliom_service_sigs.S.t}) identified as per [path] and accepting
    parameters as per [meth]. See {!Eliom_service_sigs.S.path_option}
    and {!Eliom_service_sigs.TYPES.meth} for the respective arguments.

    If [path = Path p], the service appears on path [p]. Otherwise
    ([No_path]), the service doesn't have its own path. Rather, the
    service responds on any path as long as an internal
    automatically-generated parameter is provided.

    In addition to [~path] and [~meth], [create] accepts a series of
    optional arguments described below.

    If [~https:true] is provided, all links towards that service
    will use HTTPS. By default, links will keep the current
    protocol.

    The optional argument [~priority] allows one to change the
    priority order between services that share the same path. The
    default priority is 0. If you want the service to be tried
    before (resp. after) other services, put a higher (resp. lower)
    priority.

    The remaining arguments are ignored for services identified by a
    path (constructor [Path]).

    The optional [~timeout] argument specifies a timeout (in
    seconds) after which the coservice will disappear. This amount
    of time is computed from the creation or from the last call to
    the service. The default is "no timeout". The optional
    [~max_use] argument specifies that the service can be used only
    a fixed number of times. The default is "no limitation".

    If the optional argument [~keep_nl_params:`Persistent]
    (resp. [~keep_nl_params:`All]) is given, all links towards that
    service will keep persistent (resp. all) non localized GET
    arguments of the current service. The default is [`None]. See
    the eliom manual for more information about {% <<a_manual
    chapter="server-params" fragment="nonlocalizedparameters"|non localized
    parameters>>%}.

    The optional [~name] argument provides a name for the service;
    otherwise, it will be anonymous (with an auto-generated internal
    name).

    If the optional [~csrf_safe] argument is [true], we create a
    {% <<a_manual chapter="server-security" fragment="csrf"|"CSRF-safe"
    service>>%}. In that case the [~name] argument is ignored. The
    default is [false].

    The [~csrf_scope] and [~csrf_secure], if present, should
    respectively correspond to the [~scope] and [~secure] arguments
    that will be given to the associated [register]
    function. Otherwise the registration will fail with
    {Eliom_service.Wrong_session_table_for_CSRF_safe_coservice}. See
    {!Eliom_registration.Html.register},
    {!Eliom_registration.App.register} or any other
    {!Eliom_registration}[.*.register] functions for a description
    of these arguments.

    {e Warning: [create] must be called when the site information is
    available, that is, either during a request or during the
    initialisation phase of the site.  Otherwise, it will raise the
    exception {!Eliom_common.Eliom_site_information_not_available}.
    If you are using static linking, you must delay the call to this
    function until the configuration file is read, using
    {!Eliom_service.register_eliom_module}. Otherwise you will also
    get this exception.}  *)

val create_attached_get :
   ?name:string
  -> ?csrf_safe:bool
  -> ?csrf_scope:[< Eliom_common.user_scope]
  -> ?csrf_secure:bool
  -> ?max_use:int
  -> ?timeout:float
  -> ?https:bool
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> fallback:
       ( unit
         , unit
         , get
         , att
         , non_co
         , non_ext
         , _
         , [`WithoutSuffix]
         , unit
         , unit
         , non_ocaml )
         t
  -> get_params:('gp, [`WithoutSuffix], 'gn) Eliom_parameter.params_type
  -> unit
  -> ( 'gp
       , unit
       , get
       , att
       , co
       , non_ext
       , reg
       , [`WithoutSuffix]
       , 'gn
       , unit
       , non_ocaml )
       t
(** [create_attached_get ~fallback ~get_params ()] attaches a new service on
    the path of [fallback]. The new service implements the GET method
    and accepts [get_params], in addition to an
    automatically-generated parameter that is used to identify the
    service and does not need to be provided by the
    programmer. [fallback] remains available. For a description of the
    optional parameters see {!create}. *)

val create_attached_post :
   ?name:string
  -> ?csrf_safe:bool
  -> ?csrf_scope:[< Eliom_common.user_scope]
  -> ?csrf_secure:bool
  -> ?max_use:int
  -> ?timeout:float
  -> ?https:bool
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> fallback:
       ('gp, unit, get, att, non_co, non_ext, _, 'suff, 'gn, unit, non_ocaml) t
  -> post_params:('pp, [`WithoutSuffix], 'pn) Eliom_parameter.params_type
  -> unit
  -> ('gp, 'pp, post, att, co, non_ext, reg, 'suff, 'gn, 'pn, non_ocaml) t
(** [create_attached_post ~fallback ~post_params ()] attaches a new service on
    the path of [fallback]. The new service implements the POST method
    and accepts the GET parameters of [fallback], in addition to the
    POST parameters [post_params]. An automatically-generated
    parameter is used to identify the service and does not need to be
    provided by the programmer. [fallback] remains available. For a
    description of the optional parameters see {!create}. *)

val attach :
   fallback:(unit, unit, get, att, _, non_ext, _, _, unit, unit, 'return1) t
  -> service:
       ( 'get
         , 'post
         , 'meth
         , non_att
         , co
         , non_ext
         , _
         , ([< `WithoutSuffix] as 'sf)
         , 'gn
         , 'pn
         , 'return )
         t
  -> unit
  -> ('get, 'post, 'meth, att, co, non_ext, non_reg, 'sf, 'gn, 'pn, 'return) t
(** [attach ~fallback ~service ()] attaches the preexisting pathless
    service [service] on the URL of [fallback]. This allows creating a
    link to a pathless service but with another URL than the current
    one. It is not possible to register something on the service
    returned by this function. *)

(** {2 Static loading of Eliom modules}

    This functionality allows one to register initialization functions
    for Eliom modules which will be executed when the corresponding
    module is loaded in [ocsigenserver.conf].  If the module is loaded
    dynamically, you probably don't need this.  But if the module is
    linked statically, some computations, like service registrations
    must be delayed. *)

val register_eliom_module : string -> (unit -> unit) -> unit
(** The function [register_eliom_module mod f] is used to register the
    initialization function [f] to be executed when then module [mod]
    is loaded by Ocsigen server. The module [mod] could either be a
    dynamically loaded module or linked statically into the server: in
    each case, the [f] function will be invoked when the module is
    initialized in the configuration file using [<eliommodule ...>
    ... </eliommodule>]. If [register_eliom_module] is called twice
    with the same module name, the second initialization function will
    replace the previous one. *)

val unregister :
   ?scope:[< Eliom_common.scope]
  -> ?secure:bool
  -> (_, _, _, _, _, non_ext, _, _, _, _, _) t
  -> unit
(** The function [unregister service] unregister the service handler
    previously associated to [service] with
    {!Eliom_registration.Html.register},
    {!Eliom_registration.App.register} or any other
    {!Eliom_registration}[.*.register] functions. See the
    documentation of those functions for a description of the [~scope]
    and [~secure] optional parameters. *)

val is_external : (_, _, _, _, _, _, _, _, _, _, _) t -> bool
(** Returns whether it is an external service or not. *)

(**/**)

val pre_applied_parameters :
   (_, _, _, _, _, _, _, _, _, _, _) t
  -> (string * string) list Eliom_lib.String.Table.t * (string * string) list

val new_state : unit -> string

val untype :
   ('a, 'b, 'meth, 'attached, 'co, 'ext, 'd, 'e, 'f, 'g, 'rr) t
  -> ('a, 'b, 'meth, 'attached, 'co, 'ext, 'd, 'e, 'f, 'g, 'return) t

val set_delayed_get_or_na_registration_function :
   Eliom_common.tables
  -> int
  -> (sp:Eliom_common.server_params -> string)
  -> unit

val set_delayed_post_registration_function :
   Eliom_common.tables
  -> int
  -> (sp:Eliom_common.server_params -> Eliom_common.att_key_serv -> string)
  -> unit

val set_send_appl_content :
   (_, _, _, _, _, _, _, _, _, _, _) t
  -> send_appl_content
  -> unit

exception Wrong_session_table_for_CSRF_safe_coservice

val eliom_appl_answer_content_type : string

val create_ocaml :
   ?name:string
  -> ?csrf_safe:bool
  -> ?csrf_scope:[< Eliom_common.user_scope]
  -> ?csrf_secure:bool
  -> ?max_use:int
  -> ?timeout:float
  -> ?https:bool
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> ?priority:int
  -> meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp_) meth
  -> path:('att, 'co, 'gp_) path_option
  -> unit
  -> ('gp, 'pp, 'm, 'att, 'co, non_ext, reg, 'tipo, 'gn, 'pn, 'ret ocaml) t

val create_unsafe :
   ?name:string
  -> ?csrf_safe:bool
  -> ?csrf_scope:[< Eliom_common.user_scope]
  -> ?csrf_secure:bool
  -> ?max_use:int
  -> ?timeout:float
  -> ?https:bool
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> ?priority:int
  -> meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp_) meth
  -> path:('att, 'co, 'gp_) path_option
  -> unit
  -> ('gp, 'pp, 'm, 'att, 'co, non_ext, reg, 'tipo, 'gn, 'pn, 'ret) t

val create_attached_get_unsafe :
   ?name:string
  -> ?csrf_safe:bool
  -> ?csrf_scope:[< Eliom_common.user_scope]
  -> ?csrf_secure:bool
  -> ?max_use:int
  -> ?timeout:float
  -> ?https:bool
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> fallback:(unit, unit, get, att, non_co, non_ext, _, _, unit, unit, _) t
  -> get_params:('gp, 'tipo, 'gn) Eliom_parameter.params_type
  -> unit
  -> ('gp, unit, get, att, co, non_ext, reg, 'tipo, 'gn, unit, _) t

val create_attached_post_unsafe :
   ?name:string
  -> ?csrf_safe:bool
  -> ?csrf_scope:[< Eliom_common.user_scope]
  -> ?csrf_secure:bool
  -> ?max_use:int
  -> ?timeout:float
  -> ?https:bool
  -> ?keep_nl_params:[`All | `Persistent | `None]
  -> fallback:('gp, unit, get, att, non_co, non_ext, _, 'tipo, 'gn, unit, _) t
  -> post_params:('pp, [`WithoutSuffix], 'pn) Eliom_parameter.params_type
  -> unit
  -> ('gp, 'pp, post, att, co, non_ext, reg, 'tipo, 'gn, 'pn, _) t
