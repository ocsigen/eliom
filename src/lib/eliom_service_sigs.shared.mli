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

module type TYPES = sig

  (** {2 Auxilliary service-related types} *)

  type get = Get_method
  type put = Put_method
  type post = Post_method
  type delete = Delete_method

  type co = Co
  type non_co = Non_co

  type ext = Ext
  type non_ext = Non_ext

  type http = Http_ret

  type 'a ocaml  = Ocaml of 'a
  type non_ocaml = Non_ocaml

  type reg = Reg
  type non_reg = Non_reg

  type ('get, 'tipo, 'gn) params =
    ('get, 'tipo, 'gn) Eliom_parameter.params_type
    constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]

  (** {2 Method specification} *)

  (** {b Method specification datatype}

      An Eliom service (see {!Eliom_service_sigs.S.t}) can respond to
      one of the following HTTP methods:

      - GET ([Get g])
      - POST ([Post (g, p)])
      - PUT ([Put g])
      - DELETE ([Delete g])

      In all cases, the service parameters need to be provided (see
      {!Eliom_parameter_sigs.S}). POST ([Post (g, p)]) services accept
      both GET ([g]) and POST ([p]) parameters. For the other methods,
      only GET ([g]) parameters apply.

      The type parameters are used to impose various type constraints,
      and are not necessarily of interest to the programmer. Their
      technical meaning is as follows.

      - 0-th param : method
      - params 1-4 : GET and POST parameter types and names
      - param 5    : suffix parameters permitted or not
      - param 6    : non-unit only for the [Post (g, p)] case when [g] is
                     not unit ; used to force unit GET parameters when
                     needed *)
  type ('m, _, _, _, _, _, _) meth =

    | Get : ('gp, 'tipo, 'gn) params ->

      (get, 'gp, 'gn, unit, unit, 'tipo, unit) meth

    | Post : ('gp, 'tipo, 'gn) params *
             ('pp, [`WithoutSuffix], 'pn) params ->

      (post, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp) meth

    | Put : ('gp, 'tipo, 'gn) params ->

      (put,
       'gp, 'gn,
       Eliom_parameter.raw_post_data, Eliom_parameter.no_param_name,
       'tipo, unit) meth

    | Delete : ('gp, 'tipo, 'gn) params ->

      (delete,
       'gp, 'gn,
       Eliom_parameter.raw_post_data, Eliom_parameter.no_param_name,
       'tipo, unit) meth

  (** Like {!meth} but without the parameters *)
  type 'm which_meth =
    | Get'    : get    which_meth
    | Post'   : post   which_meth
    | Put'    : put    which_meth
    | Delete' : delete which_meth

end

module type S = sig

  (** {2 Services}

      See {!Eliom_service.create} (on the server) for how to create
      services. *)

  include TYPES

  (** {3 Auxilliary types} *)

  type att
  type non_att

  type 'a attached_info =
    | Attached : att -> att attached_info
    | Nonattached : non_att -> non_att attached_info

  (** {b Type of services}

      For a service
      [('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg,
        'tipo, 'gn, 'pn, 'ret) t]:

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
        (e. g. {!Eliom_content.Html.D.get_form} ).
      - ['pn] is the type of POST parameters names. See
        {!Eliom_parameter.param_name} and form generation functions
        (e. g. {!Eliom_content.Html.D.post_form} ).
      - [ 'ret] is an information on what the service returns.  See
        {!Eliom_registration.kind}. *)
  type ('get, 'post, 'meth, 'attached, 'co, 'ext, 'reg,
        +'tipo, 'gn, 'pn, 'ret) t
    constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]

  (** {b Optional service path} *)
  type (_, _, _) path_option =
    | Path    : Eliom_lib.Url.path -> (att, non_co, _) path_option
    | No_path : (non_att, co, unit) path_option

  (** {3 Predefined services}

      {4 Reload actions} *)

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

  (** Like {!reload_action}, but forces HTTPS *)
  val reload_action_https :
    (unit, unit, get, non_att, co, non_ext, non_reg,
     [ `WithoutSuffix ], unit, unit, non_ocaml) t

  (** Like {!reload_action}, but keeps non-attached GET
      parameters. *)
  val reload_action_hidden :
    (unit, unit, get, non_att, co, non_ext, non_reg,
     [ `WithoutSuffix ], unit, unit, non_ocaml) t

  (** Like {!reload_action_hidden}, but forces HTTPS *)
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

  (** Like {!static_dir}, but forces HTTPS link *)
  val https_static_dir :
    unit ->
    (string list, unit, get, att, non_co, non_ext, non_reg,
     [ `WithSuffix ], [ `One of string list ] Eliom_parameter.param_name,
     unit, non_ocaml) t

  (** Like {!static_dir}, but allows one to put GET parameters *)
  val static_dir_with_params :
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:('a, [`WithoutSuffix], 'an) Eliom_parameter.params_type ->
    unit ->
    ((string list * 'a), unit, get, att, non_co, non_ext, non_reg,
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name *'an,
     unit, non_ocaml) t

  (** Like {!static_dir_with_params}, but forces HTTPS link *)
  val https_static_dir_with_params :
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:('a, [`WithoutSuffix], 'an) Eliom_parameter.params_type ->
    unit ->
    ((string list * 'a), unit, get, att, non_co, non_ext, non_reg,
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name *'an,
     unit, non_ocaml) t

  (** {3 Miscellaneous} *)

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

  (** Like {!add_non_localized_get_parameters} but with POST
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

  (** [extern ~prefix ~path ~meth ()] creates an external service,
      i.e., a service implemented by a remote server (not necessarily
      running Ocsigen/Eliom). *)
  val extern :
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    prefix:string ->
    path:Eliom_lib.Url.path ->
    meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, _) meth ->
    unit ->
    ('gp, 'pp, 'm, att, non_co, ext, non_reg, 'tipo, 'gn, 'pn, non_ocaml) t

  (**/**)

  val which_meth :
    (_, _, 'm, _, _, _, _, _, _, _, _) t -> 'm which_meth

  val which_meth_untyped :
    (_, _, _, _, _, _, _, _, _, _, _) t -> [`Get | `Post | `Put | `Delete]

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
    ('a -> 'b -> unit Lwt.t) Eliom_client_value.t option

  val has_client_fun :
    (_, _, _, _, _, _, _, _, _, _, _) t -> bool

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

  val internal_set_client_fun :
    service : ('a, 'b, _, _, _, _, _, _, _, _, _) t ->
    ('a -> 'b -> unit Lwt.t) Eliom_client_value.t ->
    unit

end
