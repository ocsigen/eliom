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

(* == Global application data *)
let global_appl_data_table : ((int64 * int), unit) Hashtbl.t = 
  Hashtbl.create 50

(* Loading global Eliom application data *)
let fill_global_data_table ((reqnum, size), l) =
  ignore
    (List.fold_left
       (fun b v -> 
          let n = b-1 in
          Hashtbl.replace global_appl_data_table (reqnum, n) v;
          n
       )
       size
       l)

let ((timeofday, _), _) as global_data : (int64 * int) * (unit list) =
  unmarshal (Js.Unsafe.variable "eliom_global_data")

let _ = fill_global_data_table global_data


(* == Relinking DOM nodes *)
let nodes : ((int64 * int), Dom.node Js.t) Hashtbl.t = Hashtbl.create 200

let set_node_id node id =
  Hashtbl.replace nodes id node

let retrieve_node id =
  Hashtbl.find nodes id

type ref_tree = Ref_tree of int option * (int * ref_tree) list

(* Relinking DOM nodes *)
let rec relink_dom timeofday root = fun (Ref_tree (id, subs)) ->
  begin match id with
    | Some id ->
	set_node_id root (timeofday, id)
    | None ->
	()
  end ;
  let children = root##childNodes in
  relink_dom_list timeofday children subs
and relink_dom_list timeofday dom_nodes subs =
  List.iter
    (fun (n, sub) ->
       relink_dom timeofday (dom_nodes##item (n)) sub
    )
    subs

let _ =
Dom_html.window##onload <- Dom_html.handler (fun _ ->
  relink_dom
    timeofday
    (Dom_html.document##body :> Dom.node Js.t)
    (unmarshal (Js.Unsafe.variable "eliom_id_tree") : ref_tree);
  Js._false)


(* == unwraping server data *)

let unwrap (key : 'a Eliom_client_types.data_key) : 'a = 
  Obj.magic (Hashtbl.find global_appl_data_table 
               (Eliom_client_types.of_data_key_ key))


let unwrap_sp = unwrap

let unwrap_node k = 
  retrieve_node (Eliom_client_types.of_data_key_ k)

