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

open! Service

module type PARAM = sig
  type page
  type options
  type result
  type frame

  val send :
     ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> page
    -> frame Lwt.t

  val send_appl_content : Service.send_appl_content
  (** Whether the service is capable of sending application content
      when required. This field is usually
      [Service.XNever]. This value is recorded inside each
      service just after registration.  *)

  val result_of_http_result : frame -> result
end

module type PARAM_POLY = sig
  type _ page
  type options
  type _ return
  type frame

  val send :
     ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> _ page
    -> frame Lwt.t

  val send_appl_content : Service.send_appl_content
  (** See {!Registration_sigs.PARAM.send_appl_content}. *)
end

(** Signature for application creation. *)
module type APP_PARAM = sig
  val application_name : string
  (** Name of the application. Applications must have distinct
      names. *)

  val global_data_path : string list option
  (** If a path is provided, we export through it a service for
      accessing the global data. Reading this data makes global
      injections work in client apps. *)
end

module type S = sig
  type page
  type options
  type return = Service.non_ocaml
  type result

  (** {2 Service registration } *)

  val register :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> service:
         ( 'get
           , 'post
           , _
           , _
           , _
           , Service.non_ext
           , Service.reg
           , _
           , _
           , _
           , return )
           Service.t
    -> ?error_handler:((string * exn) list -> page Lwt.t)
    -> ('get -> 'post -> page Lwt.t)
    -> unit
  (** The function [register ~service handler] associates the
      [service] to the function [handler]. The [handler] function takes
      two parameters, the GET and POST parameters of the current HTTP
      request, and should return the corresponding page.

      The optional parameter [~scope] is {!Common.global_scope}
      by default. See the Eliom manual for detailed description {{!page-"server-services".service_scope}of
      different scopes}.

      The optional parameter [~options] is specific to each output
      module. See the type description for more information.

      The optional parameters [?charset], [?code], [?content_type] and
      [?headers] can be used to modify the HTTP answer sent by
      Eliom. Use this with care.

      The optional parameter [~secure_session] has no effect for scope
      {!Common.global_scope}. With other scopes, the parameter
      is used to force the session service table in which the
      [handler] will be registered. By default, the service is
      registered in the non-secure session if the current request's
      protocol is [http], or in the secure session if the protocol is
      [https]. If set to [false] (resp. [true]) the [handler] will be
      stored in the non-secure (resp. secure) session. See the Eliom
      manual for an introduction to {{!page-"server-state"}secure state}.

      The optional parameter [~error_handler] is used to specialize
      the error page when actual parameters aren't compatible with the
      expected type. The default error handler is [ fun l -> raise
      (]{!Common.Eliom_Typing_Error}[ l) ]. *)

  val send :
     ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> page
    -> result Lwt.t
  (** The function [send page] builds the HTTP frame corresponding to
      [page]. This may be used for example in a service handler
      registered with {!Registration.Any.register}, or when
      building a custom output module.  *)
end

module type S_with_create = sig
  include S

  val create :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> ?https:bool
    -> ?name:string
    -> ?csrf_safe:bool
    -> ?csrf_scope:[< Common.user_scope]
    -> ?csrf_secure:bool
    -> ?max_use:int
    -> ?timeout:float
    -> meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp_) Service.meth
    -> path:('att, 'co, 'gp_) Service.path_option
    -> ?error_handler:((string * exn) list -> page Lwt.t)
    -> ('gp -> 'pp -> page Lwt.t)
    -> ( 'gp
         , 'pp
         , 'm
         , 'att
         , 'co
         , Service.non_ext
         , Service.reg
         , 'tipo
         , 'gn
         , 'pn
         , return )
         Service.t
  (** Create a service and register it at the same time.
      It calls {!Service.create} and then performs
      {!Registration_sigs.S.register}.
      Returns the service. *)

  val create_attached_get :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> ?https:bool
    -> ?name:string
    -> ?csrf_safe:bool
    -> ?csrf_scope:[< Common.user_scope]
    -> ?csrf_secure:bool
    -> ?max_use:int
    -> ?timeout:float
    -> fallback:
         ( unit
           , unit
           , Service.get
           , Service.att
           , Service.non_co
           , Service.non_ext
           , _
           , [`WithoutSuffix]
           , unit
           , unit
           , return )
           Service.t
    -> get_params:('gp, [`WithoutSuffix], 'gn) Parameter.params_type
    -> ?error_handler:((string * exn) list -> page Lwt.t)
    -> ('gp -> unit -> page Lwt.t)
    -> ( 'gp
         , unit
         , Service.get
         , Service.att
         , Service.co
         , Service.non_ext
         , Service.reg
         , [`WithoutSuffix]
         , 'gn
         , unit
         , return )
         Service.t
  (** Create an attached service and register it at the same time.
      It calls {!Service.create_attached_get} and then performs
      {!Registration_sigs.S.register}.
      Returns the new service. *)

  val create_attached_post :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> ?https:bool
    -> ?name:string
    -> ?csrf_safe:bool
    -> ?csrf_scope:[< Common.user_scope]
    -> ?csrf_secure:bool
    -> ?max_use:int
    -> ?timeout:float
    -> fallback:
         ( 'gp
           , unit
           , Service.get
           , Service.att
           , Service.non_co
           , Service.non_ext
           , _
           , [`WithoutSuffix]
           , 'gn
           , unit
           , return )
           Service.t
    -> post_params:('pp, [`WithoutSuffix], 'pn) Parameter.params_type
    -> ?error_handler:((string * exn) list -> page Lwt.t)
    -> ('gp -> 'pp -> page Lwt.t)
    -> ( 'gp
         , 'pp
         , Service.post
         , Service.att
         , Service.co
         , Service.non_ext
         , Service.reg
         , [`WithoutSuffix]
         , 'gn
         , 'pn
         , return )
         Service.t
  (** Create an attached POST service and register it at the same time.
      It calls {!Service.create_attached_post} and then performs
      {!Registration_sigs.S.register}.
      Returns the new service. *)
end

module type S_poly = sig
  type _ page
  type options
  type _ return

  val register :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> service:
         ( 'get
           , 'post
           , _
           , _
           , _
           , Service.non_ext
           , Service.reg
           , _
           , _
           , _
           , 'a return )
           Service.t
    -> ?error_handler:((string * exn) list -> 'a page Lwt.t)
    -> ('get -> 'post -> 'a page Lwt.t)
    -> unit
  (** See {!S.register}. *)
end

module type S_poly_with_create = sig
  include S_poly

  val create :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> ?https:bool
    -> ?name:string
    -> ?csrf_safe:bool
    -> ?csrf_scope:[< Common.user_scope]
    -> ?csrf_secure:bool
    -> ?max_use:int
    -> ?timeout:float
    -> meth:('m, 'gp, 'gn, 'pp, 'pn, 'tipo, 'gp_) Service.meth
    -> path:('att, 'co, 'gp_) Service.path_option
    -> ?error_handler:((string * exn) list -> 'a page Lwt.t)
    -> ('gp -> 'pp -> 'a page Lwt.t)
    -> ( 'gp
         , 'pp
         , 'm
         , 'att
         , 'co
         , Service.non_ext
         , Service.reg
         , 'tipo
         , 'gn
         , 'pn
         , 'a return )
         Service.t
  (** See {!S_with_create.create}. *)

  val create_attached_get :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> ?https:bool
    -> ?name:string
    -> ?csrf_safe:bool
    -> ?csrf_scope:[< Common.user_scope]
    -> ?csrf_secure:bool
    -> ?max_use:int
    -> ?timeout:float
    -> fallback:
         ( unit
           , unit
           , Service.get
           , Service.att
           , Service.non_co
           , Service.non_ext
           , _
           , [`WithoutSuffix]
           , unit
           , unit
           , 'a return )
           Service.t
    -> get_params:('gp, [`WithoutSuffix], 'gn) Parameter.params_type
    -> ?error_handler:((string * exn) list -> 'a page Lwt.t)
    -> ('gp -> unit -> 'a page Lwt.t)
    -> ( 'gp
         , unit
         , Service.get
         , Service.att
         , Service.co
         , Service.non_ext
         , Service.reg
         , [`WithoutSuffix]
         , 'gn
         , unit
         , 'a return )
         Service.t
  (** See {!S_with_create.create_attached_get}. *)

  val create_attached_post :
     ?app:string
    -> ?scope:[< Common.scope]
    -> ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> ?secure_session:bool
    -> ?https:bool
    -> ?name:string
    -> ?csrf_safe:bool
    -> ?csrf_scope:[< Common.user_scope]
    -> ?csrf_secure:bool
    -> ?max_use:int
    -> ?timeout:float
    -> fallback:
         ( 'gp
           , unit
           , Service.get
           , Service.att
           , Service.non_co
           , Service.non_ext
           , _
           , [`WithoutSuffix]
           , 'gn
           , unit
           , 'a return )
           Service.t
    -> post_params:('pp, [`WithoutSuffix], 'pn) Parameter.params_type
    -> ?error_handler:((string * exn) list -> 'a page Lwt.t)
    -> ('gp -> 'pp -> 'a page Lwt.t)
    -> ( 'gp
         , 'pp
         , Service.post
         , Service.att
         , Service.co
         , Service.non_ext
         , Service.reg
         , [`WithoutSuffix]
         , 'gn
         , 'pn
         , 'a return )
         Service.t
  (** See {!S_with_create.create_attached_post}. *)
end

module type S_poly_with_send = sig
  include S_poly

  type 'a result

  val send :
     ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> 'a page
    -> 'a result Lwt.t
end

module type S_poly_with_create_with_send = sig
  include S_poly_with_create

  type 'a result

  val send :
     ?options:options
    -> ?charset:string
    -> ?code:int
    -> ?content_type:string
    -> ?headers:Cohttp.Header.t
    -> 'a page
    -> 'a result Lwt.t
end
