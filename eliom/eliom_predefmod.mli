(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_predefmod
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** This modules contains predefined modules for generating forms and
   registering handlers, for several types of pages:
   XHTML pages typed with polymorphic variants,
   untyped (text) pages, actions, redirections, files ...
 *)

open XHTML.M
open Xhtmltypes
open Eliom_services
open Eliom_parameters
open Eliom_mkforms
open Eliom_mkreg


(** The signature of such modules. *)
module type ELIOMSIG = sig
  include Eliom_mkreg.ELIOMREGSIG
  include Eliom_mkforms.ELIOMFORMSIG
end


(** {2 Module for registering Xhtml pages typed with polymorphic variants using {!XHTML.M}} *)


module type XHTMLFORMSSIG = Eliom_predefmod_client.XHTMLFORMSSIG

module Xhtmlforms : XHTMLFORMSSIG




(** {3 Forms and registration functions} *)

      (** Eliom forms and service registration functions for XHTML *)
module Xhtml : sig

  include Eliom_mkreg.ELIOMREGSIG with type page = xhtml elt 
                                  and type options = XHTML.M.doctypes
                                  and type return = Eliom_services.http

  include XHTMLFORMSSIG

end

module Xhtmlreg : Eliom_mkreg.ELIOMREGSIG with type page = xhtml elt 
                                  and type options = XHTML.M.doctypes
                                  and type return = Eliom_services.http

module Xhtmlreg_ : 
  functor(Xhtml_content : 
            Ocsigen_http_frame.HTTP_CONTENT with type t = [ `Html ] XHTML.M.elt
                                            and type options = XHTML.M.doctypes
         ) -> Eliom_mkreg.REGCREATE with type page =  Xhtml_content.t
                                    and type options = XHTML.M.doctypes
                                    and type return = Eliom_services.http



      (** Eliom forms and service registration functions for XHTML, with
          compact markup (i.e., without pretty-printing). *)
module Xhtmlcompact : sig

  include Eliom_mkreg.ELIOMREGSIG with type page = xhtml elt
                                  and type options = XHTML.M.doctypes
                                  and type return = Eliom_services.http

  include XHTMLFORMSSIG

end



(** {3 Eliom client/server applications} *)

type appl_service_params =
    {
      ap_doctype: XHTML.M.doctypes;
      ap_title: string;
      ap_container : 'a.
        ((([< XHTML.M.common ] as 'a) XHTML.M.attrib list) option *
           (Xhtmltypes.body_content elt -> Xhtmltypes.body_content elt list))
        option;
      ap_body_attributes : 
        'a. (([< XHTML.M.common ] as 'a) XHTML.M.attrib list) option;
      ap_headers : [ `Meta | `Link | `Style | `Object | `Script ] elt list
    }

val default_appl_params : appl_service_params

module type APPL_PARAMS = sig

  (** Name of the application. 
      The name of the client side program must be the this name plus
      ".uue" suffix.
      Two distincts applications must have distincts names.
  *)
  val application_name : string

  val params : appl_service_params
end

(** The option is a boolean.
    If you set it to [true] for a service, it will send the page
    without launching the client side program if it is not already launched.
    Use this if some of your pages are not using the client side program,
    and you want to make them load faster (for example the main page).
*)

module Eliom_appl (Appl_params : APPL_PARAMS) : sig
  include Eliom_mkreg.ELIOMREGSIG 
    with type page = Xhtmltypes.body_content elt list
    and type options = bool
    and type return = Eliom_services.appl_service

  include XHTMLFORMSSIG

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  val application_name : string
end


module Xhtmlcompactreg : Eliom_mkreg.ELIOMREGSIG with type page = xhtml elt 
                                  and type options = XHTML.M.doctypes
                                  and type return = Eliom_services.http

(** {3 Module to register subpages of type [block]} *)

module Blocks : sig

  include Eliom_mkreg.ELIOMREGSIG with type page = body_content elt list
                                  and type options = unit
                                  and type return = Eliom_services.http
  include XHTMLFORMSSIG

end
  (** Use this module for example for XMLHttpRequests for block tags (e.g. <div>) *)


(** {3 Functor to create modules to register subpages for other subtypes of XHTML} *)

module SubXhtml : functor (T : sig type content end) ->
  sig

    include Eliom_mkreg.ELIOMREGSIG with type page = T.content elt list
    include XHTMLFORMSSIG

  end




(** {2 Untyped pages} *)

(** {3 Module to create forms and register untyped HTML pages} *)
module HtmlText : ELIOMSIG with
type page = string
and type form_content_elt = string
and type form_content_elt_list = string
and type form_elt = string
and type a_content_elt = string
and type a_content_elt_list = string
and type a_elt = string
and type a_elt_list = string
and type div_content_elt = string
and type div_content_elt_list = string
and type uri = string
and type link_elt = string
and type script_elt = string
and type textarea_elt = string
and type select_elt = string
and type input_elt = string
and type pcdata_elt = string
and type a_attrib_t = string
and type form_attrib_t = string
and type input_attrib_t = string
and type textarea_attrib_t = string
and type select_attrib_t = string
and type link_attrib_t = string
and type script_attrib_t = string
and type input_type_t = string

(** {3 Module to register untyped CSS pages} *)
module CssText : Eliom_mkreg.ELIOMREGSIG with type page = string

(** {3 Module to register untyped text pages} *)
module Text : Eliom_mkreg.ELIOMREGSIG with type page = string * string
(** The first string is the content, the second is the content type,
 for example "text/html" *)

(** {2 Other kinds of services} *)

(** Actions do not generate any page. They do something,
    then the page corresponding to the URL (without POST parameters
    or non-attached parameters or coservice parameters) is sent to the browser.

    If you want to give information to the handler that will be called
    to reload the page, put it in the polymorphic table returned by 
    {!Eliom_sessions.get_request_cache}.

    If you give the optional parameter
    [~options:`NoReload] to the registration function, no page will be sent.
 *)
module Action : Eliom_mkreg.ELIOMREGSIG 
  with
    type page = unit
  and type options = [ `Reload | `NoReload ]
  and type return = Eliom_services.http


(** Like actions, but the page is not reloaded. Just do something and do
   not generate any page. To be used carefully. Probably not usefull at all.
   (Same as {!Eliom_predefmod.Action} with [`NoReload] option).
 *)
module Unit : Eliom_mkreg.ELIOMREGSIG with
  type page = unit

(** Allows to create redirections towards another service.
   A 301 or 307 code is sent to the browser to ask it to redo the request to
   another URL.

   To choose if you want permanent or temporary redirection, use
   the [options] parameter of registration functions.
   For example: [register ~options:`Temporary ...].
*)
module Redirection : Eliom_mkreg.ELIOMREGSIG with
  type page =
  (unit, unit, Eliom_services.get_service_kind,
   [ `WithoutSuffix ],
   unit, unit, Eliom_services.registrable, Eliom_services.http)
    Eliom_services.service
  and type options = [ `Temporary | `Permanent ]
  and type return = Eliom_services.http

(** Allows to create redirections towards other URLs.
   A 301 or 307 code is sent to the browser to ask it to redo the request to
   another URL.

   Warning: The URL given must be an absolute URI.

   To choose if you want permanent or temporary redirection, use
   the [options] parameter of registration functions.
   For example: [register ~options:`Temporary ...].
 *)
module String_redirection : Eliom_mkreg.ELIOMREGSIG with
  type page = XHTML.M.uri
  and type options = [ `Temporary | `Permanent ]
  and type return = Eliom_services.http
(*VVV Would be better to define the type uri elsewhere *)

(** Allows to send files. The content is the name of the file to send. *)
module Files : Eliom_mkreg.ELIOMREGSIG with
  type page = string


(** Allows to create services that choose dynamically what they want
   to send. The content is created using one {!Eliom_mkreg.ELIOMREGSIG1.send}
   function, for ex [Xhtml.send] or [Files.send].
   .
 *)
module Any : Eliom_mkreg.ELIOMREGSIG with
  type page = Ocsigen_http_frame.result

(** Allows to send raw data using Ocsigen's streams.
    The content is a pair containing:

    - a list of functions returning a stream and the
    function to close it,
    - the  content type string to send.

    Streams are opened by calling the functions in the list, and closed
    automatically by a call to the closing function.
    If something goes wrong, the current stream is closed,
    and the following are not opened.
 *)
module Streamlist : Eliom_mkreg.ELIOMREGSIG with
  type page = (((unit -> string Ocsigen_stream.t Lwt.t) list) *
                 string)



(** Allows to register services that send caml values.
    Note that this kind of services are most of the time
    POST coservices, and GET (co)services are probably useless here.
*)
module Caml : sig

  type options = unit

  val register :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?sp: Eliom_sessions.server_params ->
    service:('get, 'post,
             [< internal_service_kind ],
             [< suff ], 'gn, 'pn, [ `Registrable ], 
             'return Eliom_parameters.caml) service ->
    ?error_handler:(Eliom_sessions.server_params ->
                      (string * exn) list -> 'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> 'post -> 'return Lwt.t) ->
    unit

  val register_for_session :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?session_name:string ->
    ?secure:bool ->
    sp:Eliom_sessions.server_params ->
    service:('get, 'post, [< internal_service_kind ],
             [< suff ], 'gn, 'pn, [ `Registrable ],
             'return Eliom_parameters.caml) service ->
    ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                      'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> 'post -> 'return Lwt.t) -> 
    unit

  val register_new_service :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?sp: Eliom_sessions.server_params ->
    ?https:bool ->
    path:Ocsigen_lib.url_path ->
    get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
    ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                      'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> unit -> 'return Lwt.t) ->
    ('get, unit,
     [> `Attached of
        ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
     'tipo, 'gn, unit,
     [> `Registrable ], 'return Eliom_parameters.caml) service

  val register_new_coservice :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?sp: Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_session_name: string ->
    ?csrf_secure_session: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    fallback:(unit, unit,
              [ `Attached of ([ `Internal of [ `Service ] ], [`Get]) a_s ],
              [ `WithoutSuffix ] as 'tipo,
              unit, unit, [< registrable ], 'return Eliom_parameters.caml)
      service ->
    get_params:
      ('get, [`WithoutSuffix], 'gn) params_type ->
    ?error_handler:(Eliom_sessions.server_params ->
                      (string * exn) list -> 'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> unit -> 'return Lwt.t) ->
    ('get, unit,
     [> `Attached of
        ([> `Internal of [> `Coservice ] ], [> `Get]) a_s ],
     'tipo, 'gn, unit,
     [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_coservice' :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?sp: Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_session_name: string ->
    ?csrf_secure_session: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    get_params:
      ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
    ?error_handler:(Eliom_sessions.server_params ->
                      (string * exn) list -> 'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> unit -> 'return Lwt.t) ->
    ('get, unit,
     [> `Nonattached of [> `Get] na_s ],
     'tipo, 'gn, unit, [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_coservice_for_session :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?session_name:string ->
    ?secure:bool ->
    sp:Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    fallback:(unit, unit,
              [ `Attached of ([ `Internal of [ `Service ] ], [`Get]) a_s ],
              [ `WithoutSuffix ] as 'tipo,
              unit, unit, [< registrable ], 'return Eliom_parameters.caml)
      service ->
    get_params:
      ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
    ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                      'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> unit -> 'return Lwt.t) ->
    ('get, unit,
     [> `Attached of
        ([> `Internal of [> `Coservice ] ], [> `Get]) a_s ],
     'tipo, 'gn, unit,
     [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_coservice_for_session' :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?session_name:string ->
    ?secure:bool ->
    sp:Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    get_params:
      ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
    ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                      'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> unit -> 'return Lwt.t) ->
    ('get, unit, [> `Nonattached of [> `Get] na_s ],
     'tipo, 'gn, unit,
     [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_post_service :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?sp: Eliom_sessions.server_params ->
    ?https:bool ->
    fallback:('get, unit,
              [ `Attached of
                  ([ `Internal of
                       ([ `Service | `Coservice ] as 'kind) ], [`Get]) a_s ],
              [< suff ] as 'tipo, 'gn,
              unit, [< `Registrable ], 'return Eliom_parameters.caml)
      service ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                      'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> 'post -> 'return Lwt.t) ->
    ('get, 'post, [> `Attached of
                     ([> `Internal of 'kind ], [> `Post]) a_s ],
     'tipo, 'gn, 'pn, [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_post_coservice :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?sp: Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_session_name: string ->
    ?csrf_secure_session: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    fallback:('get, unit ,
              [ `Attached of
                  ([ `Internal of [< `Service | `Coservice ] ], [`Get]) a_s ],
              [< suff ] as 'tipo,
              'gn, unit, [< `Registrable ], 'return Eliom_parameters.caml)
      service ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                      'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> 'post -> 'return Lwt.t) ->
    ('get, 'post,
     [> `Attached of
        ([> `Internal of [> `Coservice ] ], [> `Post]) a_s ],
     'tipo, 'gn, 'pn, [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_post_coservice' :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?sp: Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_session_name: string ->
    ?csrf_secure_session: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?keep_get_na_params:bool ->
    ?https:bool ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:(Eliom_sessions.server_params -> (string * exn) list ->
                      'return Lwt.t) ->
    (Eliom_sessions.server_params -> unit -> 'post -> 'return Lwt.t) ->
    (unit, 'post, [> `Nonattached of [> `Post] na_s ],
     [ `WithoutSuffix ], unit, 'pn,
     [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_post_coservice_for_session :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?session_name:string ->
    ?secure:bool ->
    sp:Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    fallback:('get, unit,
              [ `Attached of ([ `Internal of
                                  [< `Service | `Coservice ] ], [`Get]) a_s ],
              [< suff ] as 'tipo,
              'gn, unit, [ `Registrable ], 'return Eliom_parameters.caml)
      service ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:(Eliom_sessions.server_params ->
                      (string * exn) list -> 'return Lwt.t) ->
    (Eliom_sessions.server_params -> 'get -> 'post -> 'return Lwt.t) ->
    ('get, 'post,
     [> `Attached of
        ([> `Internal of [> `Coservice ] ], [> `Post]) a_s ],
     'tipo, 'gn, 'pn, [> `Registrable ], 'return Eliom_parameters.caml)
      service

  val register_new_post_coservice_for_session' :
    ?options:options ->
    ?cookies:Eliom_services.cookie list ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    ?session_name:string ->
    ?secure:bool ->
    sp:Eliom_sessions.server_params ->
    ?name: string ->
    ?csrf_safe: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?keep_get_na_params:bool ->
    ?https:bool ->
    post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
    ?error_handler:(Eliom_sessions.server_params ->
                      (string * exn) list -> 'return Lwt.t) ->
    (Eliom_sessions.server_params -> unit -> 'post -> 'return Lwt.t) ->
    (unit, 'post, [> `Nonattached of [> `Post] na_s ],
     [ `WithoutSuffix ], unit, 'pn,
     [> `Registrable ], 'return Eliom_parameters.caml)
      service


end






module Xhtml5 : sig

  include Eliom_mkreg.ELIOMREGSIG 
  with type page = Xhtml5types.xhtml XHTML5.M.elt 
  and type options = XHTML5.M.doctypes
  and type return = Eliom_services.http

(*  include XHTMLFORMSSIG *)

end

module Xhtml5compact : sig

  include Eliom_mkreg.ELIOMREGSIG 
  with type page = Xhtml5types.xhtml XHTML5.M.elt
  and type options = XHTML5.M.doctypes
  and type return = Eliom_services.http

(*  include XHTMLFORMSSIG *)

end
