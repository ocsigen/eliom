(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

(** Cross browser dom manipulation functions *)

open Js_of_ocaml

class type ['element] get_tag =
  object
    method getElementsByTagName :
      Js.js_string Js.t -> 'element Dom.nodeList Js.t Js.meth
  end

val get_body : 'element #get_tag Js.t -> 'element Js.t
val get_head : 'element #get_tag Js.t -> 'element Js.t

(** [select_nodes root] finds the nodes below [root]
    in the page annotated to be:
    * eliom links
    * eliom forms
    * process unique nodes
    * nodes with closures ( events )
    * nodes with attributes *)

val select_nodes
  :  Dom_html.element Js.t
  -> Dom_html.anchorElement Dom.nodeList Js.t
     * Dom_html.formElement Dom.nodeList Js.t
     * Dom_html.element Dom.nodeList Js.t
     * Dom_html.element Dom.nodeList Js.t
     * Dom_html.element Dom.nodeList Js.t

val select_request_nodes
  :  Dom_html.element Js.t
  -> Dom_html.element Dom.nodeList Js.t
(** [select_request_nodes root] finds the nodes below [root]
    in the page annotated to be:
    * request unique nodes *)

val ancessor : #Dom.node Js.t -> #Dom.node Js.t -> bool
(** [ancessor n1 n2] is true if [n1] is an ancessor of [n2] *)

val createEvent : Js.js_string Js.t -> #Dom_html.event Js.t

val copy_element
  :  Dom.element Js.t
  -> (Js.js_string Js.t -> bool)
  -> Dom_html.element Js.t
(** [copy_element e] creates recursively a fresh html from any xml
    element avoiding browser bugs *)

val html_document
  :  Dom.element Dom.document Js.t
  -> (Js.js_string Js.t -> bool)
  -> Dom_html.element Js.t
(** Assuming [d] has a body and head element, [html_document d] will
    return the same document as html *)

val preload_css : Dom_html.element Js.t -> unit Lwt.t
(** [preload_css e] downloads every css included in every link
    elements that is a descendant of [e] and replace it and its linked
    css by inline [<style>] elements *)

val iter_nodeList : 'a Dom.nodeList Js.t -> ('a Js.t -> unit) -> unit

val iter_attrList
  :  Dom.attr Dom.namedNodeMap Js.t
  -> (Dom.attr Js.t -> unit)
  -> unit

(** Window scrolling. *)

type position =
  {html_top : int; html_left : int; body_top : int; body_left : int}
[@@deriving json]

val top_position : position
val getDocumentScroll : unit -> position
val setDocumentScroll : position -> unit

(* Test if the "pageshow" and "pagehide" event exists. *)
val test_pageshow_pagehide : unit -> bool
val onhashchange : (Js.js_string Js.t -> unit) -> unit

(**/**)

val touch_base : unit -> unit
val add_formdata_hack_onclick_handler : unit -> unit
val section : Lwt_log_core.section
