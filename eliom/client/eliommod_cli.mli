(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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



(*external get_closure_arg : unit -> 'a = "caml_get_closure_arg"*)
val register_closure : int -> ('a -> 'b) -> unit
(*val nodes : (int, Js.Node.t) Hashtbl.t
val set_node_id : Js.Node.t -> int -> unit*)

val unwrap : 'a Eliom_client_types.data_key -> 'a

val unwrap_node : Dom_html.element Js.t Eliom_client_types.data_key -> Dom_html.element Js.t


(**/**)
val relink_dom : int64 -> Dom_html.element Js.t -> XML.ref_tree -> unit
val relink_dom_list : int64 -> #Dom.node Dom.nodeList Js.t -> (int * XML.ref_tree) list -> unit
val fill_page_data_table : (int64 * int) * Eliom_client_types.poly list -> unit


