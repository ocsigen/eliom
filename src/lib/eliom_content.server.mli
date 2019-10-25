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

(** This module allows creating valid HTML content, or other XML formats. *)


(**
    XML tree manipulation within Eliom is based on the TyXML library
    but Eliom is using a custom representation for XML values (see
    {!Xml}). Then, [Eliom_content] redefines the two high level
    interfaces ({!Svg}, {!Html}) that are provided by
    TyXML for valid XML tree creation and printing.

    - If you want to generate typed HTML, use {!Eliom_content.Html},
    - If you want to write untyped html, use {!Eliom_content.Html_text},
    - If you want to generate typed svg, use {!Eliom_content.Svg}.

    Modules {!Eliom_content.Html}, {!Eliom_content.Svg} contain two
    sub-modules: {!Eliom_content.Html.F}, {!Eliom_content.Html.D}
    corresponding to tow different semantics.
    They also contain a module {!Eliom_content.Html.C} that allows to
    inject client-side content into server-side content.

    {5 Functional semantics}

    The [F] modules provides functions to create elements with {e f}unctional
    semantics: they are standard OCaml values.

    Use this module:
    - if your application does not have a client-side part
    (server-side generated Web site)
    - or if the client-side is not written with Eliom,
    - or if you do not need to use this node from the client-side program
    (no injection [%n] on this node)
    and want to avoid the extra attributes added by module [D].

    If you use a [F]-node [n] in an injection ([%n]),
    it is considered as any OCaml value, NOT precisely the copy you (possibly)
    inserted in the page. For example, [To_dom.of_element %n] will not refer
    to the element in the page, but create a new DOM node.


    {5 DOM semantics}

    The [D] module provides functions to create elements with {e D}OM semantics:
    Firstly, they behave like DOM nodes, e.g. they can only be added once to the
    DOM tree even when appended several times.
    Secondly, those values have an identifier,
    which means they can be referred to
    on client side (by [%variable]) or used with the functions in
    {% <<a_api subproject="client"|module Eliom_content.Html.To_dom>> %} and
    {% <<a_api subproject="client"|module Eliom_content.Html.Manip>> %}.

    In case of doubt, always use [D]-nodes when you are writing a
    client-server Eliom app. You can also mix F-nodes and D-nodes.

   {5 Client-side value injection}

   The [C] modules provides functions to inject client-side elements and attributes
   into server-side content.

   {b Please read
   {% <<a_manual chapter="clientserver-html"|Eliom's manual>>%}
   to learn how to generate HTML. }

  *)

open Js_of_ocaml

(** Low-level XML manipulation. *)
module Xml : sig

  (** {2 Base functions}
      See {% <<a_api project="tyxml" | module Xml_sigs.Iterable >> %}. *)

  include Xml_sigs.Iterable
    with type 'a wrap = 'a
     and type 'a list_wrap = 'a list
     and type event_handler =
           (Dom_html.event Js.t -> unit) Eliom_client_value.t
     and type mouse_event_handler =
           (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
     and type keyboard_event_handler =
           (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t
     and type touch_event_handler =
                (Dom_html.touchEvent Js.t -> unit) Eliom_client_value.t
     and type uri = Eliom_content_xml.Xml.uri

  (** {2 Unique nodes } *)

  (** Unique nodes are XML nodes that are manipulated 'by reference'
      when sent to the client part of an Eliom-application: the
      created element is allocated only one time in each instance of
      an application. See {% <<a_manual chapter="clientserver-html"
      fragment="unique" |the eliom manual>>%} for more
      details. *)

  (** {2 Event handlers } *)

  (** Values of type [caml_event_handler] represents event handler
      build with the [{{ ... }}] syntax (see the Eliom manual for more
      information on {% <<a_manual chapter="clientserver-html"
      fragment="syntax"|syntax extension>>%}). Such values are
      expected by functions like {!Eliom_content.Html.a_onclick}. *)
  type caml_event_handler

(*
  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : ?reset:bool -> elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Concrete on client-side only. *)
  type node_id
  val get_node_id : elt -> node_id
  val make_event_handler_table : elt -> Eliom_runtime.RawXML.event_handler_table
  val make_client_attrib_table : elt -> Eliom_runtime.RawXML.client_attrib_table

  val caml_event_handler :
    (Dom_html.event Js.t -> unit) Eliom_client_value.t ->
    caml_event_handler

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * Eliom_lib.poly
                                           (* attrib client_value *)
  val racontent : attrib -> racontent

  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  (**/**)

  (** [Eliom_content.Xml.wrap page v] is like [Eliom_wrap.wrap v] but
      it makes sure that all [elt]s in [v] which are included in
      [page] are sent with empty content. This is safe because such
      elements will be taken from the DOM on the client either
      ways. *)
  val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value
*)
end

module Xml_shared : Xml_sigs.T
  with type 'a W.t = 'a Eliom_shared.React.S.t
   and type 'a W.tlist = 'a Eliom_shared.ReactiveData.RList.t
   and type event_handler =
         (Dom_html.event Js.t -> unit) Eliom_client_value.t
   and type mouse_event_handler =
         (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
   and type keyboard_event_handler =
         (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t
   and type touch_event_handler =
         (Dom_html.touchEvent Js.t -> unit) Eliom_client_value.t
   and type uri = Eliom_content_xml.Xml.uri

(** Building and pretty-printing valid SVG tree.
Information about Svg api can be found at {% <<a_api project="tyxml" | module Svg_sigs.T >> %}*)
module Svg = Eliom_content_svg

(** Building and printing valid HTML5 tree.
    Information about Html api can be found at
    {% <<a_api project="tyxml" | module Html_sigs.T >> %} .*)
module Html = Eliom_content_html
