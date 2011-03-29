(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Pierre Chambart
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

(** This module contains functions to track the XML nodes sent to a
    the browser side of a client process. Each node is associated to
    an id used to reference the node on client side. *)

open Eliom_pervasives

(** The functions in this module can only be called inside the
    function answering to a client application request *)

val make_ref_tree_list : XML.elt list -> (int * XML.ref_tree) list
val make_ref_tree : XML.elt -> XML.ref_tree
(** Those functions returns the structures of an XML tree containing
    only the nodes with an id *)

val make_node_id : XML.elt -> int
(** [make_node_id node] generate an id for a node and returns it.  It
    also mark the node and its sons as 'node to send to the client' *)

val contents_to_send : unit -> Eliom_types.elt list
(** [contents_to_send ()] returns the nodes that were marked by
    make_node_id and not already sent. Those nodes are marked as sent *)

val mark_sent : XML.elt -> unit
(** [mark_sent node] is used to tell that [node] has already been sent
    to the client by another mean without calling [contents_to_send]:
    The part of a page that is sent as plain XML ( in text ) in the
    answer of the request. *)

(**/**)

val client_process_node_table_size : unit -> int
(** [client_process_node_table_size ()] returns the size of the table
    used to store the id associated to each XML node on the client
    process. *)
