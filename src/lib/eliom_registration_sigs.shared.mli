(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2016 Vasilis Papavasileiou
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

module type PARAM = sig

  type page
  type options
  type result
  type frame

  val send :
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    page ->
    frame Lwt.t

  val send_appl_content : Eliom_service.send_appl_content
  (** Whether the service is capable to send application content when
      required. This field is usually [Eliom_service.XNever]. This
      value is recorded inside each service just after
      registration.  *)

  val result_of_http_result : frame -> result

end

module type PARAM_CLIENT = sig

  type page
  type options

  val send : ?options:options -> page -> unit Lwt.t

end

module type PARAM_POLY = sig

  type _ page
  type options
  type _ return
  type frame

  val send :
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    _ page ->
    frame Lwt.t

  (** See {!Eliom_reg_sigs.PARAM.send_appl_content}. *)
  val send_appl_content : Eliom_service.send_appl_content

end

(** Signature for application creation. *)
module type APP_PARAM = sig

  (** Name of the application. Applications must have distinct
      names. *)
  val application_name : string

  (** If a path is provided, we export through it a service for
      accessing the global data. Reading this data makes global
      injections work in client apps. *)
  val global_data_path : string list option

end

module type S = sig

  type page
  type options
  type return = Eliom_service.non_ocaml
  type result

  (** {2 Service registration } *)

  (** The function [register ~service handler] associates the
      [service] to the function [handler]. The [handler] function take
      two parameters, the GET and POST parameters of the current HTTP
      request, and should returns the corresponding page.

      The optional parameter [~scope] is {!Eliom_common.global_scope}
      by default, see the Eliom manual for detailled description {%
      <<a_manual chapter="server-services" fragment="scope"|of
      different scope>>%}.

      The optional parameter [~options] is specific to each output
      module, see the type description for more information.

      The optional parameters [?charset], [?code], [?content_type] and
      [?headers] can be used to modify the HTTP answer sent by
      Eliom. Use this with care.

      The optional parameter [~secure_session] has no effect for scope
      {!Eliom_common.global_scope}. With other scopes, the parameter
      is used to force the session service table in which the
      [handler] will be registered. By default, the service is
      registred in the unsecure session if the current request's
      protocol is [http], or in the secure session if the protocol is
      [https]. If set to [false] (resp. [true]) the [handler] will be
      stored in the unsecure (resp. secure) session. See the Eliom
      manual for an introduction to {% <<a_manual
      chapter="server-state"|secure state>>%}.

      The optional parameter [~error_handler] is used to specialize
      the error page when actual parameters aren't compatible with the
      expected type. The default error handler is [ fun l -> raise
      (]{!Eliom_common.Eliom_Typing_Error}[ l) ]. *)
  val register :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    service:
      ('get, 'post, _, _, _, Eliom_service.non_ext, Eliom_service.reg, _,
       _, _, return)
        Eliom_service.t ->
    ?error_handler:((string * exn) list -> page Lwt.t) ->
    ('get -> 'post -> page Lwt.t) ->
    unit

  (** The function [send page] build the HTTP frame corresponding to
      [page]. This may be used for example in an service handler
      registered with {!Eliom_registration.Any.register} or when
      building a custom output module.  *)
  val send :
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    page ->
    result Lwt.t

end

module type S_with_create = sig

  include S

  (** Same as {!Eliom_service.create} followed by {!register}. *)
  val create :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    ?https:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [<Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    meth:
      ('m , 'gp , 'gn , 'pp, 'pn, 'tipo, 'gp_) Eliom_service.meth ->
    path:('att, 'co, 'gp_) Eliom_service.path_option ->
    ?error_handler:((string * exn) list -> page Lwt.t) ->
    ('gp -> 'pp -> page Lwt.t) ->
    ('gp, 'pp, 'm, 'att, 'co, Eliom_service.non_ext, Eliom_service.reg,
     'tipo,
     'gn, 'pn, return)
      Eliom_service.t

  (** Same as {!Eliom_service.attach_get} followed by {!register}. *)
  val attach_get :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    ?https:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [<Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    fallback:
      (unit, unit,
       Eliom_service.get, Eliom_service.att,
       Eliom_service.non_co, Eliom_service.non_ext, _,
       [`WithoutSuffix], unit, unit, return)
        Eliom_service.t ->
    get_params:('gp, [`WithoutSuffix], 'gn) Eliom_parameter.params_type ->
    ?error_handler:((string * exn) list -> page Lwt.t) ->
    ('gp -> unit -> page Lwt.t) ->
    ('gp, unit,
     Eliom_service.get, Eliom_service.att, Eliom_service.co,
     Eliom_service.non_ext, Eliom_service.reg,
     [`WithoutSuffix], 'gn, unit, return)
      Eliom_service.t

  (** Same as {!Eliom_service.attach_post} followed by {!register}. *)
  val attach_post :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    ?https:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [<Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    fallback:
      ('gp, unit,
       Eliom_service.get, Eliom_service.att,
       Eliom_service.non_co, Eliom_service.non_ext, _,
       [`WithoutSuffix], 'gn, unit, return)
        Eliom_service.t ->
    post_params:('pp, [`WithoutSuffix], 'pn) Eliom_parameter.params_type ->
    ?error_handler:((string * exn) list -> page Lwt.t) ->
    ('gp -> 'pp -> page Lwt.t) ->
    ('gp, 'pp,
     Eliom_service.post, Eliom_service.att, Eliom_service.co,
     Eliom_service.non_ext, Eliom_service.reg,
     [`WithoutSuffix], 'gn, 'pn, return)
      Eliom_service.t

end

module type S_poly = sig

  type _ page
  type options
  type _ return

  (** See {!S.register}. *)
  val register :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    service:
      ('get, 'post, _, _, _, Eliom_service.non_ext, Eliom_service.reg, _,
       _, _, 'a return)
        Eliom_service.t ->
    ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
    ('get -> 'post -> 'a page Lwt.t) ->
    unit

end

module type S_poly_with_create = sig

  include S_poly

  (** See {!S.create}. *)
  val create :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    ?https:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [<Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    meth:
      ('m , 'gp , 'gn , 'pp, 'pn, 'tipo, 'gp_) Eliom_service.meth ->
    path:
      ('att, 'co, 'gp_) Eliom_service.path_option ->
    ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
    ('gp -> 'pp -> 'a page Lwt.t) ->
    ('gp, 'pp, 'm, 'att, 'co, Eliom_service.non_ext, Eliom_service.reg,
     'tipo,
     'gn, 'pn, 'a return)
      Eliom_service.t

  (** See {!S.attach_get}. *)
  val attach_get :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    ?https:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [<Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    fallback:
      (unit, unit,
       Eliom_service.get, Eliom_service.att,
       Eliom_service.non_co, Eliom_service.non_ext, _,
       [`WithoutSuffix], unit, unit, 'a return)
        Eliom_service.t ->
    get_params:
      ('gp, [`WithoutSuffix], 'gn) Eliom_parameter.params_type ->
    ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
    ('gp -> unit -> 'a page Lwt.t) ->
    ('gp, unit,
     Eliom_service.get, Eliom_service.att, Eliom_service.co,
     Eliom_service.non_ext, Eliom_service.reg,
     [`WithoutSuffix], 'gn, unit, 'a return)
      Eliom_service.t

  (** See {!S.attach_post}. *)
  val attach_post :
    ?app:string ->
    ?scope:[<Eliom_common.scope] ->
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?secure_session:bool ->
    ?https:bool ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [<Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    fallback:
      ('gp, unit,
       Eliom_service.get, Eliom_service.att,
       Eliom_service.non_co, Eliom_service.non_ext, _,
       [`WithoutSuffix], 'gn, unit, 'a return)
        Eliom_service.t ->
    post_params:('pp, [`WithoutSuffix], 'pn) Eliom_parameter.params_type ->
    ?error_handler:((string * exn) list -> 'a page Lwt.t) ->
    ('gp -> 'pp -> 'a page Lwt.t) ->
    ('gp, 'pp,
     Eliom_service.post, Eliom_service.att, Eliom_service.co,
     Eliom_service.non_ext, Eliom_service.reg,
     [`WithoutSuffix], 'gn, 'pn, 'a return)
      Eliom_service.t

end

module type S_poly_with_send = sig

  include S_poly

  type 'a result

  val send :
    ?options      : options ->
    ?charset      : string ->
    ?code         : int ->
    ?content_type : string ->
    ?headers      : Http_headers.t ->
    'a page ->
    'a result Lwt.t

end

module type S_poly_with_create_with_send = sig

  include S_poly_with_create

  type 'a result

  val send :
    ?options      : options ->
    ?charset      : string ->
    ?code         : int ->
    ?content_type : string ->
    ?headers      : Http_headers.t ->
    'a page ->
    'a result Lwt.t

end
