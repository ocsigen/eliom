(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_obrowser.ml
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
type ref_tree

val unwrap : 'a Eliom_client_types.data_key -> 'a

val unwrap_sp : Eliom_client_types.server_params Eliom_client_types.data_key ->
  Eliom_client_types.server_params

val unwrap_node : Eliom_client_types.server_params Eliom_client_types.data_key -> Dom.node Js.t


(**/**)
val relink_dom_list : int -> Js.Node.t list -> (int * ref_tree) list -> unit
val fill_global_data_table : (int * int) * unit list -> unit

