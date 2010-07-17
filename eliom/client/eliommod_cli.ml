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

exception Failed_service of int

let unmarshal x = Marshal.from_string (Js.to_bytestring x) 0

external register_closure
  : int -> (_ -> _) -> unit
  = "caml_register_closure"

let register_closure id f = register_closure id (fun x -> f (unmarshal x))

(* == Page application data *)
let page_appl_data_table : ((int64 * int), unit) Hashtbl.t = 
  Hashtbl.create 50

(* Loading page Eliom application data *)
let fill_page_data_table ((reqnum, size), l) =
  ignore
    (List.fold_left
       (fun b v -> 
          let n = b-1 in
          Hashtbl.replace page_appl_data_table (reqnum, n) v;
          n
       )
       size
       l)





(* == Relinking DOM nodes *)
let nodes : ((int64 * int), Dom_html.element Js.t) Hashtbl.t =
  Hashtbl.create 200

let set_node_id node id =
  Hashtbl.replace nodes id node

let retrieve_node id =
  Hashtbl.find nodes id



(* Relinking DOM nodes *)
let rec relink_dom timeofday root = 
  fun (XML.Ref_tree (id, subs)) ->
    begin match id with
      | Some id ->
	set_node_id root (timeofday, id)
      | None ->
	()
    end ;
(*VVV How to avoid the Unsafe.coerce? *)
    let children = (Js.Unsafe.coerce root##childNodes : 
                      Dom_html.element Dom.nodeList Js.t)
    in
    relink_dom_list timeofday children subs
and relink_dom_list timeofday 
    (dom_nodes : Dom_html.element Dom.nodeList Js.t) subs =
  List.iter
    (fun (n, sub) ->
      relink_dom timeofday (dom_nodes##item (n)) sub
    )
    subs





(* == unwraping server data *)

let unwrap (key : 'a Eliom_client_types.data_key) : 'a = 
  Obj.magic (Hashtbl.find page_appl_data_table 
               (Eliom_client_types.of_data_key_ key))

let unwrap_sp = unwrap

let unwrap_node k =
  retrieve_node (Eliom_client_types.of_data_key_ k)


