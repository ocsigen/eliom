(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_form
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

[%%client.start]

val set_error_handler : (unit -> bool Lwt.t) -> unit

[%%shared.start]

open Js_of_ocaml

module type Html = sig
  include
    Html_sigs.T
    with type 'a Xml.W.t = 'a
     and type 'a Xml.W.tlist = 'a list
     and type Xml.mouse_event_handler =
      (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t

  type ('a, 'b, 'c) lazy_star =
    ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

  val lazy_form :
    ( [< Html_types.form_attrib]
      , [< Html_types.form_content_fun]
      , [> Html_types.form] )
      lazy_star

  val uri_of_fun : (unit -> string) -> Xml.uri

  val attrib_of_service :
     string
    -> ([`A | `Form_get | `Form_post]
       * (bool * string list) option
       * string option
       * Eliom_lib.poly)
         option
         Eliom_lazy.request
    -> Html_types.form_attrib attrib

  val to_elt : 'a elt -> Eliom_content_core.Xml.elt
end

type 'a param

module Make_links (H : Html) :
  Eliom_form_sigs.LINKS
  with type +'a elt := 'a H.elt
   and type +'a attrib := 'a H.attrib
   and type uri := H.uri

module Make (H : Html) :
  Eliom_form_sigs.S
  with type +'a elt := 'a H.elt
   and type +'a attrib := 'a H.attrib
   and type uri := H.uri
   and type 'a param = 'a param
