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

(* Relinking DOM nodes *)

let to_html : Dom.node Js.t -> Dom_html.element Js.t = Js.Unsafe.coerce

let register_dom_event_handler node (name, evt) =
  HTML5.Dom.register_event_handler
    (Js.Unsafe.coerce node : Dom_html.element Js.t) name evt

let rec relink_dom node (id, attribs, childrens_ref_tree) =
  List.iter (register_dom_event_handler node) attribs;
  begin match id with
  | None -> ()
  | Some id ->
      try
	let pnode = HTML5.Dom.retrieve_node id in
	let parent = Js.Opt.get (node##parentNode) (fun _ -> assert false) in
	Dom.replaceChild parent node pnode
      with Not_found ->
	HTML5.Dom.register_node id (Js.Unsafe.coerce node : Dom_html.element Js.t)
  end;
  let childrens =
    List.filter
      (fun node -> node##nodeType = Dom.ELEMENT)
      (Dom.list_of_nodeList (node##childNodes)) in
  relink_dom_list childrens childrens_ref_tree

and relink_dom_list nodes ref_trees =
  match nodes, ref_trees with
  | node :: nodes, Eliom_types.Ref_node ref_tree :: ref_trees ->
      relink_dom node ref_tree;
      relink_dom_list nodes ref_trees
  | nodes, Eliom_types.Ref_empty i :: ref_trees ->
      relink_dom_list (List.chop i nodes) ref_trees
  | _, [] -> ()
  | [], _ -> assert false (* GRGR FIXME *)

let register_nodes ref_tree contents =
  begin match ref_tree with
  | Eliom_types.First_page (headers, body) ->
      relink_dom_list [(contents :> Dom.node Js.t)] [body];
      (* GRGR FIXME TODO headers ?? *)
    | Eliom_types.Change_page (h,b) ->
      assert false (* GRGR FIXME TODO *)
  end
