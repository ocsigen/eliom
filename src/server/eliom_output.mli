(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_output
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

(** Eliom services registration for various kinds of
    page content: Eliom application, valid {!HTML5} or {!XHTML},
    actions, redirections, static files, … *)

(** See the Eliom manual for
    more information on {% <<a_manual chapter="services" | services
    >>%} and {% <<a_manual chapter="outputs" | predefined outputs
    >>%}. *)

(** {% <<outline>> %}*)

open Eliom_lib
open Eliom_content_core

(** {2 Type definitions} *)

(** The type [kind] is an abstract type for the HTTP frame returned by
    a service. The type parameters are phantom types describing the
    content of the frame:

    - The second parameter is the same as the last type parameters of
    the corresponding {!type:Eliom_services.service}. Currently, one of the
    following types:
    {ul {- {!Eliom_output.appl_service}}
        {- {!Eliom_output.http_service}}
        {- {!Eliom_parameters.caml}} }
    - The first parameter is a refinement of the second
    parameter. Currently, one of the following types:
    {ul {- {!application_content}}
        {- {!browser_content}}
        {- {!block_content}}
        {- {!unknown_content}}
        {- {!caml_content}}}

*)
type ('a, 'b) kind

(** {3 Return types for {!type:Eliom_services.service} } *)

(** {4 Classical content} *)

(** The type [http_service] is used as a phantom type parameters for
    {!Eliom_services.service} and {!Eliom_output.kind}. It means the
    returned content is classical HTTP content described by the
    content type header. See {!Eliom_output.kind} for a list of others
    return types. *)
type http_service = [ `Http ]

(** The type [browser_content] is a refinement of {!http_service} to
    be used as a phantom type parameters for {!Eliom_output.kind}. It
    means the returned content must be interpreted in the browser as
    stated by the content-type header. This is most common return type
    for an eliom service, see for example {!Html5},
    {!Xhtml}, {!CssText}, {!Files},
    {!Redirection}, ….*)
type browser_content = [ `Browser ]

(** The type [block_content] is a refinement of {!http_service} to be
    used as a phantom type parameters for {!Eliom_output.kind}. It
    means the returned content is a subtree of an XML value. See for
    example {!Blocks5} or {!Make_TypedXML_Registration}. *)
type block_content

(** The type [unknown_content] is a refinement of {!http_service} to
    be used as a phantom type parameters for {!Eliom_output.kind} when
    the content-type can't be determined staticaly. See {!Text} or
    {!Any}. *)
type unknown_content

(** {4 Application content} *)

(** The type [appl_service] is used as a phantom type parameters for
    {!Eliom_services.service} and {!Eliom_output.kind}. It means the
    service is part of an Eliom application. See {!Eliom_output.kind}
    for a list of others return types. *)
type appl_service = [ `Appl ]

(** The type [application_content] is a refinement of {!appl_service}
    to be used as a phantom type parameters for {!Eliom_output.kind}. The
    parameter ['a] is phantom type that is unique for a given
    application. *)
type 'a application_content = [ `Appl of 'a ]

(**/**)
type 'a application_name
(**/**)

(** {4 OCaml content} *)

(** The type [caml_content] is an synomyn for {!Eliom_parameters.caml}
    to be used as a phantom type parameters for {!Eliom_output.kind}. See
    {!Caml}. *)
type 'a caml_content

(** The type [non_caml_service] is used as phantom type parameters for
    the {!Eliom_output.kind}. It used to type functions that operates
    over service that do not returns OCaml values, like
    {!appl_self_redirect}. *)
type non_caml_service = [ appl_service | http_service ]

(** {3 Module signature} *)

(** Abstract signature for service registration functions. For
    concrete instance see {!Html5}, {!Xhtml}, {!CssText}, {!Files},
    {!Redirection}, … *)
module type Registration = sig
  type page
  type options
  type return
  type result
  include "sigs/eliom_reg_simpl.mli"
end

(** {2 Using HTML5 with services } *)

(** Eliom service registration for services that returns HTML5
    page. This is a subset of the {!Html5} module and an instance of
    the {!Registration} abstract signature. *)
module Html5_registration : "sigs/eliom_html5_reg.mli"


(** Eliom service registration for HTML5 page. This
    an instance the {!modtype:Registration} abstract signatures. *)
module Html5 : sig
  include "sigs/eliom_html5_reg.mli"
end

(** {2 Using XHTML with services } *)

(** Eliom service registration for XHTML page. This
    an instance the {!Registration} abstract signatures. *)
module Xhtml : sig
  include "sigs/eliom_xhtml_reg.mli"
end

(** Eliom service registration for services that returns XHTML
    page. This is a subset of the {!Xhtml} module and an instance of
    the {!Registration} abstract signature. *)
module Xhtml_registration : "sigs/eliom_xhtml_reg.mli"

(** {2 Eliom client/server applications} *)

(** Signature for application creation. *)
module type APPL_PARAMS = sig

  (** Name of the application.
      Two distincts applications must have distincts names.
  *)
  val application_name : string

end

(** Type for the options of an Eliom application service.

    If you set [do_not_launch] to [true] when creating an application
    service, it will send the page without launching the client side
    program. However, if the program is already lanched, the client
    side process won't be stopped. Use this if some of your pages are
    not using the client side program and you want to make them load
    faster.
*)
type appl_service_options =
    {
      do_not_launch : bool;
    (** Do not launch the client side program if it is not already
        launched.  Default: [false]. *)
    }

(** The default options record for an eliom service. See
    {!appl_service_options}. *)
val default_appl_service_options : appl_service_options

module type ELIOM_APPL = sig

  (** The function [application_name ()] returns a [<script>] node
      that represents the javascript part of the application. If you
      do not include this script in the [<head>] node of your page, it
      will be automatically added at the end of the [<head>] node. *)
  val application_script : ?async:bool -> unit -> [> `Script ] HTML5.elt

  (** Unique identifier for this application. Currently, it is just
      the application name as defined by {!Appl_params.application_name}.

      {e Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application on a
      client.}  *)
  val application_name : string


  (** The type [appl] is an abstract type for identifying an
      application. It usually used a phantom parameter for
      {!application_content}. *)
  type appl

  include "sigs/eliom_reg.mli"
    subst type page    := HTML5_types.html HTML5.elt
      and type options := appl_service_options
      and type return  := appl_service
      and type result  := (appl application_content, appl_service) kind

  (**/**)
  val typed_name : appl application_name

end

(** Functor for application creation. The resulting module is an
    instance of the {!modtype:Registration} abstract signature. *)
module Eliom_appl (Appl_params : APPL_PARAMS) : ELIOM_APPL

module type TMPL_PARAMS = sig
  type t
  val name: string
  val make_page: t -> HTML5_types.html HTML5.elt Lwt.t
  val update: t -> Dom_html.event XML.caml_event_handler
end

module Eliom_tmpl (Appl : ELIOM_APPL) (Tmpl_param : TMPL_PARAMS): sig

  include "sigs/eliom_reg.mli"
  subst type page    := Tmpl_param.t
    and type options := appl_service_options
    and type return  := appl_service
    and type result  := (Appl.appl application_content, appl_service) kind

end

(** {3 Services returning only fragment of HTML (or others TyXML tree)} *)

(** Eliom service registration and forms creation for fragment of
    HTML5 page. This is an instance of the {!Registration} abstract
    signature.

    For Eliom application, prefers {!Caml} services to send page
    fragment.
*)
module Flow5 : "sigs/eliom_reg.mli"
  subst type page    := HTML5_types.flow5 HTML5.elt list
  and type options := unit
  and type return  := http_service
  and type result  := (block_content, http_service) kind

(** Deprecated alias for {!Flow5}. *)
module Blocks5 : "sigs/eliom_reg.mli"
  subst type page    := HTML5_types.flow5 HTML5.elt list
  and type options := unit
  and type return  := http_service
  and type result  := (block_content, http_service) kind

(** Eliom service registration and forms creation for fragment of
    XHTML page. This is an instance of the {!Registration} abstract
    signature. *)
module Blocks : "sigs/eliom_reg.mli"
  subst type page    := XHTML_types.body_content XHTML.F.elt list
  and type options := unit
  and type return  := http_service
  and type result  := (block_content, http_service) kind

(** Eliom service registration for services that returns fragment of
    TyXML's tree. The returned module is an instance of the
    {!Registration} abstract signature. *)
module Make_TypedXML_Registration
  (XML: XML_sigs.Iterable)
  (TypedXML: XML_sigs.TypedXML with module XML := XML)
  (E : sig type content end) :

  "sigs/eliom_reg.mli"
  subst type page    := E.content TypedXML.elt list
  and type options := unit
  and type return  := http_service
  and type result  := (block_content, http_service) kind

(** {2 Untyped pages} *)

(** Eliom service registration and forms creation for untyped HTML
    page. This an instance the {!Registration}
    abstract signature. The page content is a [string] that must
    contains valid HTML and the content type is always [text/html]. *)
module Html_text : sig

  include "sigs/eliom_reg.mli"
    subst type page    := string
    and type options := unit
    and type return  := http_service
    and type result  := (browser_content, http_service) kind

end

(** Eliom service registration for services that returns CSS. The page
    content is a [string] that must contains valid CSS and the content
    type is always [text/css]. The option is the optionnal
    "Cache-policy: max-age" header value to be sent.

    This an instance of the {!Registration} abstract signature.
*)
module CssText : "sigs/eliom_reg.mli"
  subst type page  := string
  and type options := int
  and type return  := http_service
  and type result  := (browser_content, http_service) kind

(** {2 Other kinds of services} *)

(** Eliom service registration for services that only execute
    actions. See the Eliom manual for more information about {%
    <<a_manual chapter="outputs" fragment="actions"|Actions
    outputs>>%}.

    If you give the optional parameter [~options:`NoReload] to the
    registration function, the action will executed and a [204 No
    Content] will be sent to the server.

    This an instance of the {!Registration} abstract signature.
*)
module Action : "sigs/eliom_reg.mli"
 subst type page    := unit
   and type options := [ `Reload | `NoReload ]
   and type return  := http_service
   and type result  := (browser_content, http_service) kind

(** Similar to {!Actions} with [`NoReload] option. *)
module Unit : "sigs/eliom_reg.mli"
  subst type page    := unit
  and type options := unit
  and type return  := http_service
  and type result  := (browser_content, http_service) kind

(** Eliom service registration for services that returns a redirections
    towards another service. See the Eliom manual for more
    information about {% <<a_manual chapter="outputs"
    fragment="redirections"|Redirections outputs>>%}.

    The default returned HTTP code is [302 Found]. You could use the
    optional parameter [~options] to change this value:

    - [`MovedPermanently] to return [301 Moved Permanently].
    - [`Found] to return [302 Found].
    - [`SeeOther] to return [303 See Other].
    - [`NotNodifed] to return [304 Not Modified].
    - [`UseProxy] to return [305 Use Proxy].
    - [`TemporaryRedirect] to return [301 Temporary Redirect].

*)
module Redirection : "sigs/eliom_reg_alpha_return.mli"
  subst type page :=
      (unit, unit, Eliom_services.get_service_kind,
       [ `WithoutSuffix ],
       unit, unit, Eliom_services.registrable, 'b)
      Eliom_services.service
  and type options := [ `MovedPermanently
		      | `Found
		      | `SeeOther
		      | `NotNodifed
		      | `UseProxy
		      | `TemporaryRedirect ]
  and type return  := 'b
  and type result  := ('a, 'b) kind

(** Eliom service registration for services that returns a redirections
    towards a string-URL. See the Eliom manual for more information
    about {% <<a_manual chapter="outputs"
    fragment="redirections"|Redirections outputs>>%}. The URL given
    must be an absolute URI.

    The default returned HTTP code is [302 Found]. You could use the
    optional parameter [~options] to change this value, see
    {!Redirections} for a detailled description.

    This an instance of the {!Registration} abstract signature.
*)
module String_redirection : "sigs/eliom_reg.mli"
  subst type page    := Url.uri
  and type options := [ `MovedPermanently
		      | `Found
		      | `SeeOther
		      | `NotNodifed
		      | `UseProxy
		      | `TemporaryRedirect ]
  and type return  := http_service
  and type result  := (browser_content, http_service) kind

(** Eliom service registration for services that returns file
    contents. The page is the name of the file to send. See the Eliom
    manual for more information on {% <<a_manual chapter="outputs"
    fragment="eliomfiles"|how to send files with Eliom>>%}. *)
module Files : sig

  (** The function [check_file file] is true if [Files.send file]
      would effectively return the file (i.e. the file is present and
      readable.) The option is the optionnal "Cache-policy: max-age"
      header value to be sent.
  *)

  val check_file : string -> bool

  include "sigs/eliom_reg.mli"
    subst type page    := string
      and type options := int
      and type return  := http_service
      and type result  := (browser_content, http_service) kind

end

(** Eliom service registration for services that send marshalled caml values.

    This an instance of the {!Registration} abstract signature.
*)
module Caml : "sigs/eliom_reg_simpl.mli"
  subst type page    := 'return
    and type options := unit
    and type return  := 'return Eliom_parameters.caml
    and type result  := ('return caml_content, 'return Eliom_parameters.caml) kind

(** Eliom service registration for services that choose dynamically
    what they want to send. The content is created using for example
    {!Html5.send} or {!String.send} functions. See the Eliom manual
    for more information about {% <<a_manual chapter="outputs"
    fragment="any"|services that choose dynamically what they want to
    send>>%} *)
module Any : "sigs/eliom_reg_alpha_return.mli"
  subst type page  := ('a, 'b) kind
  and type options := unit
  and type return  := 'b
  and type result  := ('a, 'b) kind

(** The function [appl_self_redirect send page] is an helper function
    required for defining {!Any} service usable inside an Eliom
    application ({!Eliom_appl}). It allows to cast an Eliom senders
    that do not returns {!application_content} (like {!Files.send},
    {!String.send}, ...) into a senders returns
    {!application_content}.

    When the service is called from an Eliom application, this is
    implemented with half-redirection (a redirection that leaves
    the application). Hence, the service may be called two times in a
    row and you should not use this function for service that use POST
    parameters.
*)
val appl_self_redirect :
  ('page -> ([< 'a application_content | browser_content ], [< non_caml_service ]) kind Lwt.t) -> 'page ->
  ('appl application_content, appl_service) kind Lwt.t


(** Eliom service registration for services that returns "byte"-string
    contents. The page content is a couple [(raw_content,
    content_type)]. See also {!Streamlist} for another kind of service
    that returns "byte" contents. The option is the optionnal
    "Cache-policy: max-age" header value to be sent.

    This an instance of the {!Registration} abstract signature.
*)
module String : "sigs/eliom_reg.mli"
  subst type page  := string * string
  and type options := int
  and type return  := http_service
  and type result  := (unknown_content, http_service) kind

(** Deprecated alias for {!String}. *)
module Text : "sigs/eliom_reg.mli"
  subst type page  := string * string
  and type options := int
  and type return  := http_service
  and type result  := (unknown_content, http_service) kind

(** Eliom service registration for services that returns "byte"
    contents with {% <<a_api project="ocsigenserver" text="Ocsigen's
    streams"| module Ocsigen_stream>>%}. The page content is a couple
    [(stream_creator_list, content_type)]. See also {!String} for
    another kind of service that returns "byte" contents.

    Streams are created by calling the functions in the list and {%
    <<a_api project="ocsigenserver"| val Ocsigen_stream.finalize>>%}
    is called at the end of the stream. If something goes wrong while
    processing a stream, the current stream is closed and the
    following streams are not created.
*)
module Streamlist : "sigs/eliom_reg.mli"
  subst type page    := (((unit -> string Ocsigen_stream.t Lwt.t) list) * string)
  and type options := unit
  and type return  := http_service
  and type result  := (unknown_content, http_service) kind

(** {2 Customizing registration} *)

(** The [Customize] functor allows to specialize service registration
    functions by customizing the page type. See the {% <<a_manual
    project="tutorial" chapter="interaction"| Eliom tutorial>>%} for
    example. *)
module Customize :
    functor (R : Registration) ->
      functor (T : sig type page val translate : page -> R.page Lwt.t end) -> Registration
  with type options := R.options
  and type return  := R.return
  and type page    := T.page
  and type result  := R.result

(** {2 Using your own error pages} *)

(** The [set_exn_handler handler] allows to redefines error pages:
    [404] or any exception during page generation.

    Note that you should not catch every exception here since some Eliom
    mechanisms are done using exceptions, like redirections.
    Do not catch exception defined in Eliom except {!Eliom_common.Eliom_404},
    {!Eliom_common.Eliom_Wrong_parameter} {!Eliom_common.Eliom_Typing_Error}.

    {e Warning: This functions must be called when the site
    information is available, that is, either during a request or
    during the initialisation phase of the site.  Otherwise, it will
    raise the exception
    {!Eliom_common.Eliom_site_information_not_available}.  If you are
    using static linking, you must delay the call to this function
    until the configuration file is read, using
    {!Eliom_services.register_eliom_module}. Otherwise you will also
    get this exception.}
*)
val set_exn_handler : (exn -> (browser_content, http_service) kind Lwt.t) -> unit

(** {2 Unsafe cast of contents} *)

val cast_unknown_content_kind :
  (unknown_content, http_service) kind -> ('a, http_service) kind
(** If you know that the content you generated using [Text.send] or
    [Streamlist.send] is the same as some other kind, you can cast
    it with [cast_unknown_content_kind] for use with [Any] module.*)

val cast_http_result : Ocsigen_http_frame.result -> ('a, 'b) kind
(** [cast_http_result] should only be used to register new output modules *)
