(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Copyright (C) 2011 Jérôme Vouillon, Grégoire Henry, Pierre Chambart
 * Copyright (C) 2012 Benedikt Becker
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

open Js_of_ocaml
open Lib

let section = Client_core.section

(* == Relink

   Traverse the Dom representation of the page in order to register
   "unique" nodes (or substitute previously known global nodes) and to
   bind Eliom's event handlers.
*)

let get_element_cookies_info elt =
  Js.Opt.to_option
    (Js.Opt.map
       elt##(getAttribute (Js.string Runtime.RawXML.ce_call_service_attrib))
       (fun s -> of_json ~typ:Runtime.RawXML.cookie_info_json (Js.to_string s)))

let get_element_template elt =
  Js.Opt.to_option
    (Js.Opt.map
       elt##(getAttribute (Js.string Runtime.RawXML.ce_template_attrib))
       (fun s -> Js.to_string s))

let a_handler =
  Dom_html.full_handler (fun node ev ->
    let node =
      Js.Opt.get (Dom_html.CoerceTo.a node) (fun () ->
        raise_error ~section "not an anchor element")
    in
    (* We prevent default behaviour
          only if raw_a_handler has taken the change page itself *)
    (*VVV Better: use preventdefault rather than returning false *)
    Js.bool
      (Client_core.raw_a_handler node
         (get_element_cookies_info node)
         (get_element_template node)
         ev))

let form_handler :
  (Dom_html.element Js.t, #Dom_html.event Js.t) Dom_html.event_listener
  =
  Dom_html.full_handler (fun node ev ->
    let form =
      Js.Opt.get (Dom_html.CoerceTo.form node) (fun () ->
        raise_error ~section "not a form element")
    in
    let kind =
      if String.lowercase_ascii (Js.to_string form##._method) = "get"
      then `Form_get
      else `Form_post
    and f _ = Lwt.return_false in
    Js.bool
      (Client_core.raw_form_handler form kind
         (get_element_cookies_info form)
         (get_element_template node)
         ev f))

let relink_process_node (node : Dom_html.element Js.t) =
  let id =
    Js.Opt.get
      node##(getAttribute (Js.string Runtime.RawXML.node_id_attrib))
      (fun () -> raise_error ~section "unique node without id attribute")
  in
  Js.Optdef.case
    (Client_core.find_process_node id)
    (fun () ->
       Logs.debug ~src:section (fun fmt ->
         fmt "Relink process node: did not find %s. Will add it."
           (Js.to_string id));
       Client_core.register_process_node id (node :> Dom.node Js.t))
    (fun pnode ->
       Logs.debug ~src:section (fun fmt ->
         fmt "Relink process node: found %s" (Js.to_string id));
       Js.Opt.iter node##.parentNode (fun parent ->
         Dom.replaceChild parent pnode node);
       let id = Js.to_bytestring id in
       if not (String.starts_with ~prefix:"global_" id)
       then (
         let children = Dom.list_of_nodeList pnode##.childNodes in
         List.iter (fun c -> ignore pnode##(removeChild c)) children;
         let children = Dom.list_of_nodeList node##.childNodes in
         List.iter (fun c -> ignore pnode##(appendChild c)) children))

let relink_request_node (node : Dom_html.element Js.t) =
  let id =
    Js.Opt.get
      node##(getAttribute (Js.string Runtime.RawXML.node_id_attrib))
      (fun () -> raise_error ~section "unique node without id attribute")
  in
  Js.Optdef.case
    (Client_core.find_request_node id)
    (fun () ->
       Logs.debug ~src:section (fun fmt ->
         fmt "Relink request node: did not find %s. Will add it."
           (Js.to_string id));
       Client_core.register_request_node id (node :> Dom.node Js.t))
    (fun pnode ->
       Logs.debug ~src:section (fun fmt ->
         fmt "Relink request node: found %s" (Js.to_string id));
       Js.Opt.iter node##.parentNode (fun parent ->
         Dom.replaceChild parent pnode node))

let relink_request_nodes root =
  Logs.debug ~src:section (fun fmt -> fmt "Relink request nodes");
  Config.debug_time "relink_request_nodes";
  Mod_dom.iter_nodeList (Mod_dom.select_request_nodes root) relink_request_node;
  Config.debug_time_end "relink_request_nodes"

(* Relinks a-elements, form-elements, and process nodes. The list of
   closure nodes is returned for application on [relink_closure_node]
   after the client values are initialized.
*)
let relink_page_but_client_values (root : Dom_html.element Js.t) =
  Logs.debug ~src:section (fun fmt -> fmt "Relink page");
  let nodes = Mod_dom.select_nodes root in
  Mod_dom.iter_nodeList nodes.links (fun node -> node##.onclick := a_handler);
  Mod_dom.iter_nodeList nodes.forms (fun node ->
    node##.onsubmit := form_handler);
  Mod_dom.iter_nodeList nodes.process_nodes relink_process_node;
  nodes

(* == Rebuild event handlers

   Event handlers inside the DOM tree are rebuilt from the closure map
   sent with the request. The actual functions will be taken from the
   client values.

   It returns a single handler ([unit -> unit]) which captures all
   onload event handlers found in the tree, and cancels the execution
   when on raises [False] (cf. [raw_event_handler]).
*)

let is_closure_attrib, get_closure_name, get_closure_id =
  let v_prefix = Runtime.RawXML.closure_attr_prefix in
  let v_len = String.length v_prefix in
  let v_prefix_js = Js.string v_prefix in
  let n_prefix = Runtime.RawXML.closure_name_prefix in
  let n_len = String.length n_prefix in
  let n_prefix_js = Js.string n_prefix in
  ( (fun attr ->
      attr##.value##(substring 0 v_len) = v_prefix_js
      && attr##.name##(substring 0 n_len) = n_prefix_js)
  , (fun attr -> attr##.name##(substring_toEnd n_len))
  , fun attr -> attr##.value##(substring_toEnd v_len) )

let relink_closure_node root onload table (node : Dom_html.element Js.t) =
  Logs.debug ~src:section (fun fmt -> fmt "Relink closure node");
  let aux attr =
    if is_closure_attrib attr
    then
      let cid = Js.to_bytestring (get_closure_id attr) in
      let name = get_closure_name attr in
      try
        let cv = Runtime.RawXML.ClosureMap.find cid table in
        let closure = Client_core.raw_event_handler cv in
        if name = Js.string "onload"
        then (
          if
            Mod_dom.ancestor root node
            (* if not inside a unique node replaced by an older one *)
          then onload := closure :: !onload)
        else
          Js.Unsafe.set node name
            (Dom_html.handler (fun ev -> Js.bool (closure ev)))
      with Not_found ->
        Logs.err ~src:section (fun fmt ->
          fmt "relink_closure_node: client value %s not found" cid)
  in
  Mod_dom.iter_attrList node##.attributes aux

let relink_closure_nodes
      (root : Dom_html.element Js.t)
      event_handlers
      closure_nodeList
  =
  Logs.debug ~src:section (fun fmt ->
    fmt "Relink %i closure nodes" closure_nodeList##.length);
  let onload = ref [] in
  Mod_dom.iter_nodeList closure_nodeList (fun node ->
    relink_closure_node root onload event_handlers node);
  fun () ->
    let ev = Mod_dom.createEvent (Js.string "load") in
    ignore (List.for_all (fun f -> f ev) (List.rev !onload))

let is_attrib_attrib, get_attrib_id =
  let v_prefix = Runtime.RawXML.client_attr_prefix in
  let v_len = String.length v_prefix in
  let v_prefix_js = Js.string v_prefix in
  let n_prefix = Runtime.RawXML.client_name_prefix in
  let n_len = String.length n_prefix in
  let n_prefix_js = Js.string n_prefix in
  ( (fun attr ->
      attr##.value##(substring 0 v_len) = v_prefix_js
      && attr##.name##(substring 0 n_len) = n_prefix_js)
  , fun attr -> attr##.value##(substring_toEnd v_len) )

let relink_attrib _root table (node : Dom_html.element Js.t) =
  Logs.debug ~src:section (fun fmt -> fmt "Relink attribute");
  let aux attr =
    if is_attrib_attrib attr
    then
      let cid = Js.to_bytestring (get_attrib_id attr) in
      try
        let value = Runtime.RawXML.ClosureMap.find cid table in
        let rattrib : Content_core.Xml.attrib =
          Lib.from_poly (Lib.to_poly value)
        in
        Client_core.rebuild_rattrib node rattrib
      with Not_found ->
        raise_error ~section "relink_attrib: client value %s not found" cid
  in
  Mod_dom.iter_attrList node##.attributes aux

let relink_attribs (root : Dom_html.element Js.t) attribs attrib_nodeList =
  Logs.debug ~src:section (fun fmt ->
    fmt "Relink %i attributes" attrib_nodeList##.length);
  Mod_dom.iter_nodeList attrib_nodeList (fun node ->
    relink_attrib root attribs node)
