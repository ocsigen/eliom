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

(** Client-side service registration.

    The interface is meant to be compatible with server-side
    <<a_api subproject="server" | module Eliom_registration >>.

    See {% <<a_manual chapter="clientserver-services"|the manual chapter on client-side services>> %}
    for details. *)

type 'a kind
type browser_content = [`Browser]
type 'a application_content = [`Appl of 'a]

module Html :
  Eliom_registration_sigs.S
  with type page = Html_types.html Eliom_content.Html.elt
   and type options = unit
   and type return = Eliom_service.non_ocaml
   and type result = browser_content kind

module Action :
  Eliom_registration_sigs.S
  with type page = unit
   and type options = [`Reload | `NoReload]
   and type return = Eliom_service.non_ocaml
   and type result = browser_content kind

module Unit :
  Eliom_registration_sigs.S
  with type page = unit
   and type options = unit
   and type return = Eliom_service.non_ocaml
   and type result = browser_content kind

type appl_service_options = {do_not_launch : bool}
(** Has no effect on client; for compatibility with server *)

module App (_ : Eliom_registration_sigs.APP_PARAM) : sig
  val application_name : string

  type app_id

  include
    Eliom_registration_sigs.S
    with type page = Html_types.html Eliom_content.Html.elt
     and type options = appl_service_options
     and type return = Eliom_service.non_ocaml
     and type result = app_id application_content kind
end

type _ redirection =
  | Redirection :
      ( unit
        , unit
        , Eliom_service.get
        , _
        , _
        , _
        , _
        , [`WithoutSuffix]
        , unit
        , unit
        , 'a )
        Eliom_service.t
      -> 'a redirection

(* [page] et al. are not really polymorphic. The type variables are
    necessary for maintaining type-level compatibility with server
    (for injections) *)
module Redirection :
  Eliom_registration_sigs.S_poly_with_send
  with type 'a page = Eliom_service.non_ocaml redirection
   and type options =
    [ `MovedPermanently
    | `Found
    | `SeeOther
    | `NotNodifed
    | `UseProxy
    | `TemporaryRedirect ]
   and type 'a return = Eliom_service.non_ocaml
   and type 'a result = browser_content kind

module Any :
  Eliom_registration_sigs.S_poly_with_send
  with type 'a page = 'a kind
   and type options = unit
   and type 'a return = Eliom_service.non_ocaml
   and type 'a result = 'a kind

val appl_self_redirect :
   ('page -> [< 'a application_content | browser_content] kind Lwt.t)
  -> 'page
  -> 'appl application_content kind Lwt.t
(** For compatibility with server-side [appl_self_redirect] *)

(**/**)

module type Base = sig
  type return = Eliom_service.non_ocaml
end

module Block5 : Base
module Html_text : Base
module CssText : Base
module Text : Base
module String : Base
module String_redirection : Base
module Streamlist : Base

module Ocaml : sig
  type 'a return = 'a Eliom_service.ocaml
end

val section : Lwt_log_core.section
