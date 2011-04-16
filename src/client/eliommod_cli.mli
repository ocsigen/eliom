(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

(**/**)

open Eliom_pervasives

(*external get_closure_arg : unit -> 'a = "caml_get_closure_arg"*)
val register_closure : int -> ('a -> 'b) -> unit
val nodes : (int, Dom_html.element Js.t) Hashtbl.t
(*val set_node_id : Js.Node.t -> int -> unit*)

val unwrap : 'a Eliom_types.data_key -> 'a

val unwrap_node : Dom.node Js.t Eliom_types.data_key -> Dom_html.element Js.t

val rebuild_xml : int64 -> Eliom_types.elt -> Dom_html.element Js.t
val relink_dom : int64 -> Dom_html.element Js.t -> XML.M.ref_tree -> unit
val relink_dom_list : int64 -> #Dom.node Dom.nodeList Js.t -> (int * XML.M.ref_tree) list -> unit
val fill_page_data_table : (int64 * int) * Eliom_types.poly list -> unit


