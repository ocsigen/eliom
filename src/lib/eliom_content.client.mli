(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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

open Js_of_ocaml

(** This module provides the creation of valid XML content, i.e. XML, SVG,
    and (X)HTML5.

    {b Please read
    {% <<a_manual chapter="clientserver-html"|Eliom's manual>>%}
    for more information on HTML generation. }
    You can also have a look at the server API of
    {% <<a_api subproject="server" | module Eliom_content >> %}
    for an explication of the modules [F] and [D].

  *)

(** Low-level XML manipulation. *)

module Xml : Xml_sigs.NoWrap
  with type uri = string
   and type event_handler = Dom_html.event Js.t -> unit
   and type mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
   and type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit
   and type touch_event_handler = Dom_html.touchEvent Js.t -> unit
   and type elt = Eliom_content_xml.Xml.elt
   and type attrib = Eliom_content_xml.Xml.attrib

(** Building valid SVG . *)
module Svg = Eliom_content_svg

(** Building valid (X)HTML5. *)
module Html = Eliom_content_html

(**/**)

(** With [set_form_error_handler f], [f] becomes the action to be
    called when we are unable to call a client-side service due to
    invalid form data.

    If the handler returns [true], nothing happens.

    If the handler returns [false], we proceed to call the server-side
    service.

    The default handler throws an exception (via [Lwt.fail_with]). *)
val set_form_error_handler : (unit -> bool Lwt.t) -> unit
