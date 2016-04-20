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

module type S = sig

  include module type of Eliom_service_types

  type att
  type non_att

  type 'a attached_info =
    | Attached : att -> att attached_info
    | Nonattached : non_att -> non_att attached_info

  (** Type of services.
      - ['get] is the type of GET parameters expected by the service.
      - ['post] is the type of POST parameters expected by the service.
      - ['meth] the HTTP method
      - ['attached] attached or non-attached
      - ['co] co-service or regular service
      - ['ext] external or internal
      - ['reg]: possible to register a handler on this service
      - ['tipo] the type paremeter of subtype {!suff} states the kind
        of parameters it uses: suffix or not.
      - ['gn] is the type of GET parameters names. See
        {!Eliom_parameter.param_name} and form generation functions
        (e. g. {!Eliom_content.Html5.D.get_form}).
      - ['pn] is the type of POST parameters names. See
        {!Eliom_parameter.param_name} and form generation functions
        (e. g. {!Eliom_content.Html5.D.post_form}).
      - [ 'ret] is an information on what the service returns.  See
        {!Eliom_registration.kind}. *)
  type ('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg,
        +'tipo, 'gn, 'pn, +'ret) t
    constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]

  module Id : sig

    type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) service =
      ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) t

    type (_, _, _, _, _) t =
      | Path :
          Eliom_lib.Url.path
        -> (att, non_co, _, _, _) t
      | Fallback :
          (unit, unit, 'mf, att, non_co, non_ext, reg,
           [ `WithoutSuffix ], unit, unit, 'ret) service
        -> (att, co, 'mf, 'ret, unit) t
      | Global :
          (non_att, co, _, _, unit) t

    val untype :
      ('a, 'c, 'm, 'r, 'g) t -> ('a, 'c, 'm, 'rr, 'g) t

  end

  (** {2 Definitions of services}

      {e Warning: These functions must be called when the site
      information is available, that is, either during a request or
      during the initialisation phase of the site.  Otherwise, it will
      raise the exception
      {!Eliom_common.Eliom_site_information_not_available}.  If you
      are using static linking, you must delay the call to this
      function until the configuration file is read, using
      {!Eliom_service.register_eliom_module}. Otherwise you will also
      get this exception.}  *)

  (** The function [create ~id ~meth ~ret ()] creates a {!service}
      identified as per [id] and accepting parameters as per [m]. The
      parameter [~ret] is used to constrain the type parameter ['ret] of
      the service.

      If the optional parameter [~https:true] is given, all links
      towards that service will use https. By default, links will keep
      the current protocol.

      The optional parameter [~priority] allows one to change the
      priority order between service that shares the same path. The
      default priority is 0 ; if you want the service to be tried
      before (resp. after) other services, put a higher (resp. lower)
      priority.

      If the optional parameter [~keep_nl_params:`Persistent]
      (resp. [~keep_nl_params:`All]) is given, all links towards that
      service will keep persistent (resp. all) non localized GET
      parameters of the current service. The default is [`None]. See
      the eliom manual for more information about {% <<a_manual
      chapter="params" fragment="nonlocalizedparameters"|non localized
      parameters>>%}.  *)
  val create :
    ?name:string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?priority:int ->
    ret:'ret Ret.t ->
    meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'mf, 'gp_) Meth.t ->
    id:('att, 'co, 'mf, 'ret, 'gp_) Id.t ->
    unit ->
    ('gp, 'pp, 'm, 'att, 'co, non_ext, reg, 'tipo, 'gn, 'pn, 'ret) t

  (** {2 External services} *)

  (** The function [create_external ~prefix ~path ~get_params ()]
      creates a service for an external web site, that will use GET
      method and requires [get_params] as parameters. This allows one
      to creates links or forms towards other Web sites using Eliom's
      syntax.

      The parameter labelled [~path] is the URL path. Each element of
      the list will be URL-encoded.

      The parameter labelled [~prefix] contains all what you want to
      put before the path. It usually starets with "http://" plus the
      name of the server. The prefix is not URL encoded.

      The whole URL is constructed from the prefix, the path and GET
      parameters. Hence, an empty prefix can be used to make a link to
      another site of the same server.

      See {!val:service} for a description of the optional
      [~keep_nl_params] and [~ret] parameters.  *)
  val create_external :
    prefix: string ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ret:'ret Ret.t ->
    meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'mf, _) Meth.t ->
    unit ->
    ('gp, 'pp, 'm, att, non_co, ext, non_reg,
     'tipo, 'gn, 'pn, 'ret) t

  (** {2 Predefined services} *)

  (** {3 Reload actions} *)

  (** The service [reload_action] is a predefined non-attached action
      with special behaviour: it has no parameter at all, even
      non-attached parameters.  Use it if you want to make a link to
      the current page without non-attached parameters.  It is almost
      equivalent to a POST non-attached service without POST
      parameters, on which you register an action that does nothing,
      but you can use it with <a> links, not only forms.  It does not
      keep non attached GET parameters.  *)
  val reload_action :
    (unit, unit, get, non_att, co, non_ext, non_reg,
     [ `WithoutSuffix ], unit, unit, non_ocaml) t

  (** Same as {!reload_action} but forcing HTTPS. *)
  val reload_action_https :
    (unit, unit, get, non_att, co, non_ext, non_reg,
     [ `WithoutSuffix ], unit, unit, non_ocaml) t

  (** Same as {!reload_action} but keeps non-attached GET
      parameters. *)
  val reload_action_hidden :
    (unit, unit, get, non_att, co, non_ext, non_reg,
     [ `WithoutSuffix ], unit, unit, non_ocaml) t

  (** Same as {!reload_action_hidden} but forcing HTTPS. *)
  val reload_action_https_hidden :
    (unit, unit, get, non_att, co, non_ext, non_reg,
     [ `WithoutSuffix ], unit, unit, non_ocaml) t

  (** {3 Static files} *)

  (** The predefined service [static_dir] allows one to create links
      to static files. This service takes the name of a static file as
      a parameter (a string list, slash separated). The actual
      directory in filesystem where static pages will be found must be
      set up in the configuration file with the staticmod
      extension. *)
  val static_dir :
    unit ->
    (string list, unit, get, att, non_co, non_ext, non_reg,
     [ `WithSuffix ], [ `One of string list ] Eliom_parameter.param_name,
     unit, non_ocaml) t

  (** Same as {!static_dir} but forcing https link. *)
  val https_static_dir :
    unit ->
    (string list, unit, get, att, non_co, non_ext, non_reg,
     [ `WithSuffix ], [ `One of string list ] Eliom_parameter.param_name,
     unit, non_ocaml) t

  (** Like [static_dir], but allows one to put GET parameters *)
  val static_dir_with_params :
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:('a, [`WithoutSuffix], 'an) Eliom_parameter.params_type ->
    unit ->
    ((string list * 'a), unit, get, att, non_co, non_ext, non_reg,
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name *'an,
     unit, non_ocaml) t

  (** Same as {!static_dir_with_params} but forcing https link. *)
  val https_static_dir_with_params :
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:('a, [`WithoutSuffix], 'an) Eliom_parameter.params_type ->
    unit ->
    ((string list * 'a), unit, get, att, non_co, non_ext, non_reg,
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name *'an,
     unit, non_ocaml) t

  (** {2 Miscellaneous} *)

  (** The function [preapply ~service paramaters] creates a new
      service by preapplying [service] to the GET [parameters]. It is
      not possible to register a handler on an preapplied service;
      preapplied services may be used in links or as fallbacks. *)
  val preapply :
    service:
      ('a, 'b, 'meth, att, 'co, 'ext, 'reg, _, 'e, 'f, 'return) t ->
    'a ->
    (unit, 'b, 'meth, att, 'co, 'ext, non_reg,
     [ `WithoutSuffix ], unit, 'f, 'return) t

  (** [attach_global_to_fallback ~fallback ~service] attaches the
      global service [service] on the URL of [fallback]. This allows
      creating a link to a global service but with another URL than
      the current one. It is not possible to register something on the
      service returned by this function. *)
  val attach_global_to_fallback :
    fallback:
      (unit, unit, get, att, _, non_ext, _,
       _, unit, unit, 'return1) t ->
    service:
      ('get, 'post, 'meth, non_att, co, non_ext, _,
       [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'return) t ->
    ('get, 'post, 'meth, att, co, non_ext, non_reg,
     'sf, 'gn, 'pn, 'return) t

  (** The function [add_non_localized_get_parameters ~params ~service]
      Adds non localized GET parameters [params] to [service]. See the
      Eliom manual for more information about {% <<a_manual
      chapter="server-params" fragment="nonlocalizedparameters"|non
      localized parameters>>%}. *)
  val add_non_localized_get_parameters :
    params:
      ('p, [ `WithoutSuffix ], 'pn)
      Eliom_parameter.non_localized_params ->
    service:
      ('a, 'b, 'meth, 'attach, 'co, 'ext, 'reg,
       'd, 'e, 'f, 'return) t ->
    ('a * 'p, 'b, 'meth, 'attach, 'co, 'ext, 'reg,
     'd, 'e * 'pn, 'f, 'return) t

  (** Same as {!add_non_localized_get_parameters} but with POST
      parameters.*)
  val add_non_localized_post_parameters :
    params:
      ('p, [ `WithoutSuffix ], 'pn)
      Eliom_parameter.non_localized_params ->
    service:
      ('a, 'b, 'meth, 'attach, 'co, 'ext, 'g,
       'd, 'e, 'f, 'return) t ->
    ('a, 'b * 'p, 'meth, 'attach, 'co, 'ext, 'g,
     'd, 'e, 'f * 'pn, 'return) t

  (**/**)

  val which_meth :
    (_, _, 'm, _, _, _, _, _, _, _, _) t -> 'm Meth.which

  val info :
    (_, _, _, 'att, _, _, _, _, _, _, _) t -> 'att attached_info

  val is_external : (_, _, _, _, _, _, _, _, _, _, _) t -> bool

  val get_params_type :
    ('a, _, _, _, _, _, _, 'b, 'c,  _, _) t ->
    ('a, 'b, 'c) Eliom_parameter.params_type

  val post_params_type :
    (_, 'a, _, _, _, _, _, _, _, 'b, _) t ->
    ('a, [ `WithoutSuffix ], 'b) Eliom_parameter.params_type

  val sub_path : att -> Eliom_lib.Url.path

  val full_path : att -> Eliom_lib.Url.path

  val prefix : att -> string

  val get_name : att -> Eliom_common.att_key_serv

  val post_name : att -> Eliom_common.att_key_serv

  val redirect_suffix : att -> bool

  val na_name : non_att -> Eliom_common.na_key_serv

  val na_keep_get_na_params: non_att -> bool

  val max_use :
    (_, _, _, _, _, _, _, _, _, _, _) t -> int option

  val timeout :
    (_, _, _, _, _, _, _, _, _, _, _) t -> float option

  val https :
    (_, _, _, _, _, _, _, _, _, _, _) t -> bool

  val priority : att -> int

  val client_fun :
    ('a, 'b, _, _, _, _, _, _, _, _, _) t ->
    (unit -> ('a -> 'b -> unit Lwt.t) option) Eliom_client_value.t

  val has_client_fun :
    (_, _, _, _, _, _, _, _, _, _, _) t -> bool

  val has_client_fun_lazy :
    (_, _, _, _, _, _, _, _, _, _, _) t ->
    (unit -> bool) Eliom_client_value.t

  val keep_nl_params :
    (_, _, _, _, _, _, _, _, _, _, _) t -> [ `All | `Persistent | `None ]

  val change_get_num :
    ('a, 'b, 'meth, att, 'co, 'ext, _, 'd, 'e, 'f, 'return) t ->
    att ->
    Eliom_common.att_key_serv ->
    ('a, 'b, 'meth, att, 'co, 'ext, _, 'd, 'e, 'f, 'return) t

  (* Not implemented on client side: TODO should not be called in
     Eliom_uri *)
  val register_delayed_get_or_na_coservice :
    sp:Eliom_common.server_params ->
    (int * [< Eliom_common.user_scope ] * bool option) ->
    string

  val register_delayed_post_coservice :
    sp:Eliom_common.server_params ->
    (int * [< Eliom_common.user_scope ] * bool option) ->
    Eliom_common.att_key_serv -> string

  (** Whether the service is capable to send application content or
      not. (application content has type
      Eliom_service.eliom_appl_answer: content of the application
      container, or xhr redirection ...).  A link towards a service
      with send_appl_content = XNever will always answer a regular
      http frame (this will stop the application if used in a regular
      link or form, but not with XHR).  XAlways means "for all
      applications" (like redirections/actions).  XSame_appl means
      "only for this application".  If there is a client side
      application, and the service has XAlways or XSame_appl when it
      is the same application, then the link (or form or change_page)
      will expect application content.  *)
  type send_appl_content =
    | XNever
    | XAlways
    | XSame_appl of string * string option
  (* used by eliommod_mkform *)

  (** Returns the name of the application to which belongs the
      service, if any. *)
  val send_appl_content :
    (_, _, _, _, _, _, _, _, _, _, _) t -> send_appl_content

  val xhr_with_cookies :
    (_, _, _, _, _, _, _, _, _, _, _) t -> string option option

  val set_client_fun :
    ?app:string ->
    service:('a, 'b, _, _, _, _, _, _, _, _, _) t ->
    ('a -> 'b -> unit Lwt.t) Eliom_client_value.t ->
    unit

  val internal_set_client_fun :
    service :('a, 'b, _, _, _, _, _, _, _, _, _) t ->
    (unit -> ('a -> 'b -> unit Lwt.t) option) Eliom_client_value.t ->
    unit

end
