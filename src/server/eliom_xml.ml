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

type elt =
    { node : XML.elt Weak.t; (* always size 1 *)
      mutable sent : bool;
      mutable id : int option; }

let create_weak n =
  let w = Weak.create 1 in
  Weak.set w 0 (Some n);
  w

module T =
struct
  (* We do not want the hashtbl to keep reference to the node key *)
  module Int =
  struct
    type t = int
    let hash n = n
    let equal (x:int) (y:int) = x = y
  end
  module H = Hashtbl.Make(Int)

  type 'a t = 'a H.t
  let mem t n = H.mem t (XML.ref_node n)
  let find t n = H.find t (XML.ref_node n)
  let find_key t k = H.find t k
  let create = H.create
  let add t n v = H.add t (XML.ref_node n) v
  let replace t n v = H.replace t (XML.ref_node n) v
  let remove_key t k = H.remove t k
  let iter_key t f = H.iter t f
  let fold_key t f = H.fold t f
  let length t = H.length t

end

let node_reference_table_key : elt T.t Polytables.key = Polytables.make_key ()
let node_id_key : (int ref) Polytables.key = Polytables.make_key ()

let get_table () =
  let sp = Eliom_common.get_sp () in
  let cpi = Lazy.force sp.Eliom_common.sp_client_process_info in
  let table = cpi.Eliom_common.cpi_references in
  try
    Polytables.get ~table ~key:node_reference_table_key
  with
    | Not_found ->
      let t = T.create 0 in
      Polytables.set ~table ~key:node_reference_table_key ~value:t;
      t

let new_id () =
  let sp = Eliom_common.get_sp () in
  let cpi = Lazy.force sp.Eliom_common.sp_client_process_info in
  let table = cpi.Eliom_common.cpi_references in
  let r =
    try
      Polytables.get ~table ~key:node_id_key
    with
      | Not_found ->
	let r = ref 0 in
	Polytables.set ~table ~key:node_id_key ~value:r;
	r
  in
  incr r;
  !r

let is_element node =
  match node.XML.elt with
    | XML.Empty
    | XML.Comment _
    | XML.EncodedPCDATA _
    | XML.PCDATA _
    | XML.Entity _ -> false
    | XML.Leaf _
    | XML.Node _ -> true

let rec add_node_ t node =
  if not (T.mem t node)
  then
    begin
      T.add t node 
	{ node = create_weak node;
	  sent = false;
	  id =
	    if is_element node
	    then Some (new_id ())
	    else None
	(* the ref_tree won't be sparse if there is no None *)
	};
      match node.XML.elt with
        | XML.Empty
        | XML.Comment _
        | XML.EncodedPCDATA _
        | XML.PCDATA _
        | XML.Entity _
        | XML.Leaf _ -> ()
        | XML.Node (_,_,l) -> List.iter (add_node_ t) l
    end

let rec mark_sent_ t node =
  add_node_ t node;
  let elt = T.find t node in
  if elt.sent
  then ()
  else
    begin
      elt.sent <- true;
      match node.XML.elt with
        | XML.Empty
        | XML.Comment _
        | XML.EncodedPCDATA _
        | XML.PCDATA _
        | XML.Entity _
        | XML.Leaf _ -> ()
        | XML.Node (_,_,l) -> List.iter (mark_sent_ t) l
    end

let find_unsent_roots_ t =
  let root = T.create 0 in
  let add k elt =
    match Weak.get elt.node 0 with
      | None -> T.remove_key t k
      | Some node ->
	if elt.sent = false
	then
	  begin
            if not (T.mem root node)
            then T.add root node true;
            match node.XML.elt with
              | XML.Empty
              | XML.Comment _
              | XML.EncodedPCDATA _
              | XML.PCDATA _
              | XML.Entity _
              | XML.Leaf _ -> ()
              | XML.Node (_,_,l) -> List.iter (fun n -> T.replace root n false) l
	  end
  in
  T.iter_key add t;
  T.fold_key (fun k v l ->
    if v
    then
      try
	match Weak.get (T.find_key t k).node 0 with
	  | None -> T.remove_key t k; l
	  | Some n -> n::l
      with Not_found -> l
    else l) root []

let add_node node = add_node_ (get_table ()) node
let mark_sent node = mark_sent_ (get_table ()) node
let find_unsent_roots () = find_unsent_roots_ (get_table ())

let make_node_id_ t node =
  if not (is_element node) then failwith "can't assign id to non element xml node";
  add_node_ t node;
  let node = T.find t node in
  let id =
    match node.id with
      | None ->
	let id = new_id () in
	node.id <- Some id;
	id
      | Some id -> id in
  id

let make_node_id node = make_node_id_ (get_table ()) node

let map_separator = function
  | XML.Space -> Eliom_types.Space
  | XML.Comma -> Eliom_types.Comma

let map_attrib a = match XML.acontent a with
  | XML.AFloat (string, float) -> Eliom_types.AFloat (string, float)
  | XML.AInt (string, int) -> Eliom_types.AInt (string, int)
  | XML.AStr (string1, string2) -> Eliom_types.AStr (string1, string2)
  | XML.AStrL (separator, string, string_list) ->
    Eliom_types.AStrL (map_separator separator, string, string_list)

let rec map_node t n =
  if (T.find t n).sent
  then (Eliom_types.Ref (make_node_id_ t n), None)
    (* we don't need to attach id to reference node *)
  else
    let node = match n.XML.elt with
      | XML.Empty -> Eliom_types.Empty
      | XML.Comment s -> Eliom_types.Comment s
      | XML.EncodedPCDATA s -> Eliom_types.EncodedPCDATA s
      | XML.PCDATA s -> Eliom_types.PCDATA s
      | XML.Entity s -> Eliom_types.Entity s
      | XML.Leaf (e,a) -> Eliom_types.Leaf (e,List.map map_attrib a)
      | XML.Node (e,a,l) -> Eliom_types.Node (e,List.map map_attrib a,List.map (map_node t) l)
    in
    (node, (T.find t n).id)

let contents_to_send () =
  let t = get_table () in
  let roots = find_unsent_roots_ t in
  let contents = List.map (map_node t) roots in
  List.iter (mark_sent_ t) roots;
  contents

let rec make_ref_tree_list_ t l =
  let rec map i = function
    | e :: es ->
        begin match make_ref_tree_ t e with
          | XML.Ref_tree (None, []) -> map i es
          | v -> (i, v) :: map (succ i) es
        end
    | [] -> []
  in map 0 l
and make_ref_tree_ t root =
  let children =
    match root.XML.elt with
        XML.Node (n_, _, elts) -> make_ref_tree_list_ t elts
      | XML.Empty | XML.EncodedPCDATA _
      | XML.PCDATA _ | XML.Entity _
      | XML.Leaf (_, _) | XML.Comment _  ->
          []
  in
  let id =
    if is_element root
    then (T.find t root).id
    else None
  in
  XML.Ref_tree (id, children)

let make_ref_tree_list nodes = make_ref_tree_list_ (get_table ()) nodes
let make_ref_tree node = make_ref_tree_ (get_table ()) node

let client_process_node_table_size () =
  let t = get_table () in
  T.length t

let internal_wrap node =
  let id = make_node_id node in
  ( id , Eliom_common.make_unwrapper Eliom_common.node_unwrap_id )

let node_mark () = Obj.repr (Eliom_common.make_wrapper internal_wrap)

let () = XML.make_mark := node_mark

let () = Ocsigen_messages.debug2 ("eliom_xml: wrapper loaded");
