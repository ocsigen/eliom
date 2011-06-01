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

open Eliom_pervasives

let unique elt = HTML5.M.tot (XML.make_unique (HTML5.M.toelt elt))

(** Ref tree *)

let cons_attrib att acc = match XML.racontent att with
  | XML.RACamlEvent (n,oc) -> (n, oc) :: acc
  | _ -> acc

let rec make_ref_tree elt =
  let id = XML.get_unique_id elt in
  let attribs, childrens = match XML.content elt with
    | XML.Empty | XML.EncodedPCDATA _ | XML.PCDATA _
    | XML.Entity _ | XML.Comment _  -> [], []
    | XML.Leaf (_, attribs) ->
	List.fold_right cons_attrib attribs [],
	[]
    | XML.Node (_, attribs, elts) ->
	List.fold_right cons_attrib attribs [],
	make_ref_tree_list elts
  in
  match id, attribs, childrens with
  | None, [], [] -> Eliom_types.Ref_empty 0
  | _ -> Eliom_types.Ref_node (id, attribs, childrens)

and make_ref_tree_list l =
  let aggregate elt acc =
    let elt = make_ref_tree elt in
    if elt = Eliom_types.Ref_empty 0 then
      match acc with
      | [] -> []
      | Eliom_types.Ref_empty i :: acc ->
	  Eliom_types.Ref_empty (succ i) :: acc
      | acc -> Eliom_types.Ref_empty 1 :: acc
    else elt :: acc in
  List.fold_right aggregate l []

and make_attrib_list l =
  let aggregate a acc = match XML.racontent a with
    | XML.RACamlEvent (n, (id, args)) ->
	(n, (id, args)) :: acc
    | _ -> acc in
  List.fold_right aggregate l []

let rec print_ref_tree fmt tree = match tree with
  | Eliom_types.Ref_empty i -> Format.fprintf fmt "Empty(%d)" i
  | Eliom_types.Ref_node (elt_ref, attribs, childrens) ->
      Format.fprintf fmt "Node(@[<hov>%a,@,%a,@,%a@])"
	print_elt_ref elt_ref
	print_attribs attribs
	print_ref_tree_list childrens

and print_ref_tree_list fmt childrens =
  Format.fprintf fmt "[@[<hov>";
  List.iter (Format.fprintf fmt "%a ;@," print_ref_tree) childrens;
  Format.fprintf fmt "@]]"

and print_attribs fmt attribs =
  Format.fprintf fmt "[@[<hov>";
  List.iter (Format.fprintf fmt "%a ;@," print_attrib) attribs;
  Format.fprintf fmt "@]]"

and print_attrib fmt (name, (id, _)) =
  Format.fprintf fmt "%s = %Ld (args...)" name id

and print_elt_ref fmt = function
  | Some id -> Format.fprintf fmt "Some(%s)" id
  | None -> Format.fprintf fmt "None"
