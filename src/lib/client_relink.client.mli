(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Copyright (C) 2011 Jérôme Vouillon, Grégoire Henry, Pierre Chambart
 * Copyright (C) 2012 Benedikt Becker
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

(** Relinking of a page received from the server: registration of the
    unique nodes, and binding of Eliom's links, forms, event handlers
    and client attributes. Internal module. *)

open Js_of_ocaml

val relink_request_nodes : Dom_html.element Js.t -> unit
(** Registers the request nodes below the given root, or replaces them
    with the nodes already known under the same id. *)

val relink_page_but_client_values :
   Dom_html.element Js.t
  -> Mod_dom.selected_nodes
(** Relinks the links, forms and process nodes below the given root.
    The selected nodes are returned, so that the closure and attribute
    nodes can be relinked once the client values are initialised. *)

val relink_closure_nodes :
   Dom_html.element Js.t
  -> Runtime.RawXML.event_handler_table
  -> Dom_html.element Dom.nodeList Js.t
  -> unit
  -> unit
(** [relink_closure_nodes root table nodes] binds the event handlers of
    [nodes], taken from [table]. It returns a function running all the
    [onload] handlers found, which stops at the first one returning
    [false]. *)

val relink_attribs :
   Dom_html.element Js.t
  -> Runtime.RawXML.client_attrib_table
  -> Dom_html.element Dom.nodeList Js.t
  -> unit
(** [relink_attribs root table nodes] sets the client attributes of
    [nodes], taken from [table]. *)
