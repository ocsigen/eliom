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

open Eliom_pervasives

exception Failed_service of int

let unmarshal x = Marshal.from_string (Js.to_bytestring x) 0

external register_closure
  : int -> (_ -> _) -> unit
  = "caml_register_closure"

let register_closure id f = register_closure id (fun x -> f (unmarshal x))

(* == Page application data *)
let page_appl_data_table : ((int64 * int), Eliom_types.poly) Hashtbl.t =
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
let nodes : (int, Dom_html.element Js.t) Hashtbl.t =
  Hashtbl.create 200

let set_node_id node (_,id) =
  Hashtbl.replace nodes id node

let retrieve_node (_,id) =
  Hashtbl.find nodes id

let rebuild_attrib a =
  let open Eliom_types in
  match a with
  | AFloat (name, float) -> XML.float_attrib name float
  | AInt (name, int) -> XML.int_attrib name int
  | AStr (name, string) -> XML.string_attrib name string
  | AStrL (Space, name, string_list) -> XML.space_sep_attrib name string_list
  | AStrL (Comma, name, string_list) -> XML.comma_sep_attrib name string_list

let rec rebuild_xml timeofday (root,id) : Dom_html.element Js.t =
  let open Eliom_types in
  let node =
    match root with
    | Ref i -> ( retrieve_node (timeofday,i) :> Dom.node Js.t )
    | Empty -> XML.empty ()
    | Comment s -> XML.comment s
    | EncodedPCDATA s -> XML.encodedpcdata s
    | PCDATA s -> XML.pcdata s
    | Entity s -> XML.entity s
    | Leaf (s,a) -> XML.leaf ~a:(List.map rebuild_attrib a) s
    | Node (s,a,subs) ->
	XML.node ~a:(List.map rebuild_attrib a) s
	  (List.map (rebuild_xml timeofday :> Eliom_types.elt ->  Dom.node Js.t) subs)
  in
  let node = Js.Unsafe.coerce (node : Dom.node Js.t) in
  (match id with
  | None -> ()
  | Some id ->
      set_node_id node (timeofday, id));
  node

(* Relinking DOM nodes *)
let rec relink_dom timeofday root (XML.Ref_tree (id, subs)) =
    begin match id with
      | Some id ->
	set_node_id root (timeofday, id)
      | None ->
	()
    end ;
    relink_dom_list timeofday (root##childNodes) subs
and relink_dom_list timeofday dom_nodes subs =
  let j = ref (-1) in
  let k = ref (-1) in
  List.iter
    (fun (i, sub) ->
       while !j < i do
         incr k;
         let node =
           Js.Optdef.get (dom_nodes##item (!k)) (fun () -> assert false) in
         if node##nodeType = Dom.ELEMENT then incr j;
         if i = !j then
           relink_dom timeofday (Js.Unsafe.coerce node) sub
       done)
    subs

let relink_dom_list =
  (relink_dom_list :  _ ->  Dom.node Dom.nodeList Js.t -> _
                   :> _ -> #Dom.node Dom.nodeList Js.t -> _)

(* == unwraping server data *)

let unwrap (key : 'a Eliom_types.data_key) : 'a =
  Obj.magic (Hashtbl.find page_appl_data_table
               (Eliom_types.of_data_key_ key))

(* let unwrap_sp = unwrap *)

let unwrap_node k =
  retrieve_node (Eliom_types.of_data_key_ k)

let internal_node_unwrap (k,unwrapper) = retrieve_node (0L,k)

let () = Eliom_unwrap.register_unwrapper Eliom_common.node_unwrap_id internal_node_unwrap
