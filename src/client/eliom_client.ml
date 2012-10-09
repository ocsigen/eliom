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

open Eliom_lib
open Eliom_content_core

module JsTable = Eliommod_jstable

(* == Auxiliaries *)

let create_buffer () =
  let elts = ref [] in
  let add x =
    elts := x :: !elts
  in
  let flush () =
    let res = List.rev !elts in
    elts := [];
    res
  in
  add, flush

(* == Callbacks for onload and onunload *)

let run_callbacks handlers =
  List.iter (fun f -> f ()) handlers

let onload, flush_onload = create_buffer ()

let onunload, flush_onunload = create_buffer ()

(* == Closure *)

module Client_closure : sig
  val register : closure_id:int64 -> closure:(_ -> _) -> unit
  val find : closure_id:int64 -> (poly -> poly)
end = struct

  let key closure_id = Js.string (Int64.to_string closure_id)

  let client_closures = JsTable.create ()

  let register ~closure_id ~closure =
    trace "Register client closure %Ld" closure_id;
    JsTable.add client_closures (key closure_id)
      (fun args ->
         to_poly (closure (from_poly args)))

  let find ~closure_id =
    Js.Optdef.get
      (JsTable.find client_closures (key closure_id))
      (fun () ->
         raise Not_found)
end

module Client_value : sig
  val find : closure_id:int64 -> instance_id:int -> _
  val initialize : client_value_datum -> unit
end = struct

  let table = JsTable.create ()

  let closure_key closure_id =
    Js.string (Int64.to_string closure_id)

  let instance_key instance_id =
    Js.string (string_of_int instance_id)

  let find ~closure_id ~instance_id =
    trace "Get client value %Ld/%d" closure_id instance_id;
    let value =
      let instances =
        Js.Optdef.get
          (JsTable.find
             table
             (closure_key closure_id))
          (fun () ->
             raise Not_found)
      in
      Js.Optdef.get
        (JsTable.find
           instances
           (instance_key instance_id))
        (fun () ->
           raise Not_found)
    in
    from_poly value

  let initialize {closure_id; instance_id; args} =
    trace "Initialize client value %Ld/%d" closure_id instance_id;
    let closure = Client_closure.find ~closure_id in
    let value = closure args in
    Eliom_unwrap.late_unwrap_value
      (Eliom_unwrap.id_of_int Eliom_lib_base.client_value_unwrap_id_int)
      (fun (cv, _) ->
         Client_value_server_repr.closure_id cv = closure_id &&
            Client_value_server_repr.instance_id cv = instance_id)
      value;
    let instances =
      Js.Optdef.get
        (JsTable.find table (closure_key closure_id))
        (fun () ->
           let instances = JsTable.create () in
           JsTable.add table (closure_key closure_id) instances;
           instances)
    in
    JsTable.add instances (instance_key instance_id) value

end

module Injection : sig
  val get : name:string -> _
  val initialize : injection_datum -> unit
end = struct

  let table = JsTable.create ()

  let get ~name =
    trace "Get injection %s" name;
    from_poly
      (Js.Optdef.get
         (JsTable.find table (Js.string name))
         (fun () ->
            error "Did not find injection %S" name))

  let initialize { Eliom_lib_base.injection_id; injection_value } =
    trace "Initialize injection %s" injection_id;
    (* BBB One should assert that injection_value doesn't contain any
       value marked for late unwrapping. How to do this efficiently? *)
    JsTable.add table (Js.string injection_id) injection_value

end

(* == Populating client values and injections by global data *)

let global_data = ref String_map.empty

let do_next_server_section_data ~compilation_unit_id =
  try
    let data = String_map.find compilation_unit_id !global_data in
    List.iter Client_value.initialize
      (Queue.take data.server_sections_data)
  with Not_found -> (* Client-only compilation unit *)
    ()

let do_next_client_section_data ~compilation_unit_id =
  try
    let data = String_map.find compilation_unit_id !global_data in
    List.iter Injection.initialize
      (Queue.take data.client_sections_data)
  with Not_found -> (* Client-only compilation unit *)
    ()

(* == Initialize the client values sent with a request *)

let do_request_data request_data =
  trace "Do request data (%d)" (List.length request_data);
  (* On a request, i.e. after running the toplevel definitions, global_data
     must contain at most empty sections_data lists, which stem from server-
     only eliom files. *)
  String_map.iter
    (fun _ { server_sections_data; client_sections_data } ->
       Queue.iter (fun data -> assert (data = []))
         server_sections_data;
       Queue.iter (fun data -> assert (data = []))
         client_sections_data)
    !global_data;
  List.iter Client_value.initialize request_data

(*******************************************************************************)

let register_unwrapped_elt, force_unwrapped_elts =
  let suspended_nodes = ref [] in
  (fun elt ->
     suspended_nodes := elt :: !suspended_nodes),
  (fun () ->
     trace "Force unwrapped elements";
     List.iter Xml.force_lazy !suspended_nodes;
     suspended_nodes := [])

(* == Process nodes (a.k.a. nodes with a unique Dom instance on each client process) *)

let (register_process_node, find_process_node) =
  let process_nodes : Dom.node Js.t JsTable.t = JsTable.create () in
  let find id =
    trace "Find process node %s" (Js.to_string id);
    Js.Optdef.bind
      (JsTable.find process_nodes id)
      (fun node ->
        if Js.to_bytestring (node##nodeName##toLowerCase()) == "script" then
          (* We don't wan't to reexecute global script. *)
          Js.def (Dom_html.document##createTextNode (Js.string "") :> Dom.node Js.t)
        else
          Js.def node)
  in
  let register id node =
    trace "Register process node %s" (Js.to_string id);
    JsTable.add process_nodes id node in
  (register, find)

let registered_process_node id = Js.Optdef.test (find_process_node id)

let getElementById id =
  Js.Optdef.case (find_process_node (Js.string id))
    (fun () -> debug "getElementById %s: Not_found" id; raise Not_found)
    (fun pnode -> pnode)

(* == Request nodes (a.k.a. nodes with a unique Dom instance in the current request) *)

let register_request_node, find_request_node, reset_request_nodes =
  let request_nodes : Dom.node Js.t JsTable.t ref = ref (JsTable.create ()) in
  let find id = JsTable.find !request_nodes id in
  let register id node =
    trace "Register request node %s" (Js.to_string id);
    JsTable.add !request_nodes id node in
  let reset () =
    trace "Reset request nodes";
    (* Unwrapped elements must be forced before reseting the request node table. *)
    force_unwrapped_elts ();
    request_nodes := JsTable.create () in
  (register, find, reset)

(* == Current uri.

   This reference is used in [change_page_uri] and popstate event
   handler to mimic browser's behaviour with fragment: we do not make
   any request to the server, if only the fragment part of url
   changes.

*)

let current_uri =
  ref (fst (Url.split_fragment (Js.to_string Dom_html.window##location##href)))

(* == Postpone events until the page is fully loaded. e.g. after the
      end of the page's onload event handlers. *)

let in_onload, broadcast_load_end, wait_load_end, set_loading_phase =
  let loading_phase = ref true in
  let load_end = Lwt_condition.create () in
  let set () = loading_phase := true in
  let in_onload () = !loading_phase in
  let broadcast_load_end () =
    loading_phase := false;
    Lwt_condition.broadcast load_end () in
  let wait_load_end () =
    if !loading_phase
    then Lwt_condition.wait load_end
    else Lwt.return () in
  in_onload, broadcast_load_end, wait_load_end, set

(* == Helper's functions for Eliom's event handler.

   Allow to convert Xml.event_handler to javascript closure and to
   register them in Dom node.

*)

(* forward declaration... *)
let change_page_uri_ = ref (fun ?cookies_info ?tmpl href -> assert false)
let change_page_get_form_ = ref (fun ?cookies_info ?tmpl form href -> assert false)
let change_page_post_form_ = ref (fun ?cookies_info ?tmpl form href -> assert false)

let middleClick ev =
   match Dom_html.taggedEvent ev with
   | Dom_html.MouseEvent ev ->
       Dom_html.buttonPressed ev = Dom_html.Middle_button
       || Js.to_bool ev##ctrlKey
       || Js.to_bool ev##shiftKey
       || Js.to_bool ev##altKey
       || Js.to_bool ev##metaKey
   | _ -> false

let raw_a_handler node cookies_info tmpl ev =
  let href = (Js.Unsafe.coerce node : Dom_html.anchorElement Js.t)##href in
  let https = Url.get_ssl (Js.to_string href) in
  (middleClick ev)
  || (https = Some true && not Eliom_request_info.ssl_)
  || (https = Some false && Eliom_request_info.ssl_)
  || (!change_page_uri_ ?cookies_info ?tmpl (Js.to_string href); false)

let raw_form_handler form kind cookies_info tmpl ev =
  let action = Js.to_string form##action in
  let https = Url.get_ssl action in
  let change_page_form = match kind with
    | `Form_get -> !change_page_get_form_
    | `Form_post -> !change_page_post_form_ in
  (https = Some true && not Eliom_request_info.ssl_)
  || (https = Some false && Eliom_request_info.ssl_)
  || (change_page_form ?cookies_info ?tmpl form action; false)

let raw_event_handler cv =
  let closure_id = Client_value_server_repr.closure_id cv in
  let instance_id = Client_value_server_repr.instance_id cv in
  try
    let value = Client_value.find ~closure_id ~instance_id in
    let handler = (Eliom_lib.from_poly value : #Dom_html.event Js.t -> unit) in
    fun ev -> try handler ev; true with False -> false
  with Not_found ->
    error "Client value %Ld/%d not found as event handler" closure_id instance_id

let reify_caml_event node ce : #Dom_html.event Js.t -> bool = match ce with
  | Xml.CE_call_service None -> (fun _ -> true)
  | Xml.CE_call_service (Some (`A, cookies_info, tmpl)) ->
      (fun ev ->
        let node = Js.Opt.get (Dom_html.CoerceTo.a node) (fun () -> error "not an anchor element") in
        raw_a_handler node cookies_info tmpl ev)
  | Xml.CE_call_service (Some ((`Form_get | `Form_post) as kind, cookies_info, tmpl)) ->
      (fun ev ->
        let form = Js.Opt.get (Dom_html.CoerceTo.form node) (fun () -> error "not a form element") in
        raw_form_handler form kind cookies_info tmpl ev)
  | Xml.CE_client_closure f ->
      (fun ev -> try f ev; true with False -> false)
  | Xml.CE_registered_closure (_, cv) ->
      raw_event_handler cv

let reify_event node ev = match ev with
  | Xml.Raw ev -> Js.Unsafe.variable ev
  | Xml.Caml ce -> reify_caml_event node ce

let register_event_handler, flush_load_script =
  let add, flush = create_buffer () in
  let register node (name, ev) =
    let f = reify_caml_event node ev in
    if name = "onload" then
      add f
    else
      Js.Unsafe.set node (Js.bytestring name)
        (Dom_html.handler (fun ev -> Js.bool (f ev))) in
  let flush () =
    let fs = flush () in
    let ev = Eliommod_dom.createEvent (Js.string "load") in
    ignore (List.for_all (fun f -> f ev) fs)
  in
  register, flush

(* == Associate data to state of the History API.

   We store an 'id' in the state, and store data in an association
   table in the session storage. This allows to avoid "replaceState"
   that has not a coherent behaviour between Chromium and Firefox
   (2012/03).

   Storing the scroll position in the state is not required with
   Chrome or Firefox: they automatically store and restore the
   correct scrolling while browsing the history. However this
   behaviour in not required by the HTML5 specification (only
   suggested). *)

type state =
    (* TODO store cookies_info in state... *)
    { template : Js.js_string Js.t;
      position : Eliommod_dom.position;
    }

let random_int () = (truncate (Js.to_float (Js.math##random()) *. 1000000000.))
let current_state_id = ref (random_int ())

let state_key i = (Js.string "state_history")##concat(Js.string (string_of_int i))
let get_state i : state =
  Js.Opt.case
    (Js.Optdef.case ( Dom_html.window##sessionStorage )
       (fun () ->
         (* We use this only when the history API is
            available. Sessionstorage seems to be available
            everywhere the history API exists. *)
         error "sessionStorage not available")
       (fun s -> s##getItem(state_key i)))
    (fun () -> error "State id not found %d in sessionStorage" i)
    (fun s -> Json.unsafe_input s)
let set_state i (v:state) =
  Js.Optdef.case ( Dom_html.window##sessionStorage )
    (fun () -> () )
    (fun s -> s##setItem(state_key i, Json.output v))
let update_state () =
  set_state !current_state_id
    { template =
        ( match Eliom_request_info.get_request_template () with
          | Some tmpl -> Js.bytestring tmpl
          | None -> Js.string  "" );
      position = Eliommod_dom.getDocumentScroll () }

(* == Leaving page *)

let leave_page () =
  update_state ();
  run_callbacks (flush_onunload ())

(* TODO: Registering a global "onunload" event handler breaks the
   'bfcache' mechanism of Firefox and Safari. We may try to use
   "pagehide" whenever this event exists. See:

   https://developer.mozilla.org/En/Using_Firefox_1.5_caching

   http://www.webkit.org/blog/516/webkit-page-cache-ii-the-unload-event/

   and the function [Eliommod_dom.test_pageshow_pagehide]. *)

(* == Low-level: call service. *)

let create_request_
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =

  (* TODO: allow get_get_or_post service to return also the service
     with the correct subtype. Then do use Eliom_uri.make_string_uri
     and Eliom_uri.make_post_uri_components instead of Eliom_uri.make_string_uri_
     and Eliom_uri.make_post_uri_components__ *)

  match Eliom_service.get_get_or_post service with
  | `Get ->
      let uri =
        Eliom_uri.make_string_uri_
          ?absolute ?absolute_path ?https
          ~service
          ?hostname ?port ?fragment ?keep_nl_params ?nl_params get_params
      in
      `Get uri
  | `Post ->
      let path, get_params, fragment, post_params =
        Eliom_uri.make_post_uri_components__
          ?absolute ?absolute_path ?https
          ~service
          ?hostname ?port ?fragment ?keep_nl_params ?nl_params
          ?keep_get_na_params get_params post_params
      in
      let uri =
        Eliom_uri.make_string_uri_from_components (path, get_params, fragment)
      in
      `Post (uri, post_params)

let raw_call_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  lwt uri, content =
    match create_request_
      ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?keep_get_na_params
      get_params post_params
    with
      | `Get uri ->
        Eliom_request.http_get
          ?cookies_info:(Eliom_uri.make_cookies_info (https, service)) uri []
          Eliom_request.string_result
      | `Post (uri, post_params) ->
        Eliom_request.http_post
          ?cookies_info:(Eliom_uri.make_cookies_info (https, service))
          uri post_params Eliom_request.string_result in
  match content with
    | None -> raise_lwt (Eliom_request.Failed_request 204)
    | Some content -> Lwt.return (uri, content)

let call_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  lwt _, content =
    raw_call_service ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?keep_get_na_params
      get_params post_params in
  Lwt.return content


(* == Leave an application. *)

let exit_to
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  (match create_request_
     ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
     ?keep_nl_params ?nl_params ?keep_get_na_params
     get_params post_params
   with
     | `Get uri -> Eliom_request.redirect_get uri
     | `Post (uri, post_params) -> Eliom_request.redirect_post uri post_params)

let window_open ~window_name ?window_features
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params =
  match create_request_
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params ()
  with
    | `Get uri ->
        Dom_html.window##open_(Js.string uri, window_name, Js.Opt.option window_features)
    | `Post (uri, post_params) -> assert false

(* == Call caml service.

   Unwrap the data and execute the associated onload event
   handlers.
*)

let unwrap_caml_content content =
  let r : 'a Eliom_types.eliom_caml_service_data =
    Eliom_unwrap.unwrap (Url.decode content) 0
  in
  Lwt.return (r.Eliom_types.ecs_data, r.Eliom_types.ecs_request_data)

let call_caml_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  trace "Call caml service";
  lwt _, content =
    raw_call_service
      ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?keep_get_na_params
      get_params post_params in
  lwt content, request_data = unwrap_caml_content content in
  do_request_data request_data;
  run_callbacks (flush_onload ());
  reset_request_nodes ();
  Lwt.return content

(* == The function [change_url_string] changes the URL, without doing a request.

   It uses the History API if present, otherwise we write the new URL
   in the fragment part of the URL (see 'redirection_script' in
   'server/eliom_registration.ml'). *)

let current_pseudo_fragment = ref ""
let url_fragment_prefix = "!"
let url_fragment_prefix_with_sharp = "#!"

let change_url_string uri =
  current_uri := fst (Url.split_fragment uri);
  if Eliom_process.history_api then begin
    update_state();
    current_state_id := random_int ();
    Dom_html.window##history##pushState(Js.Opt.return (!current_state_id),
                                        Js.string "",
                                        Js.Opt.return (Js.string uri));
    Eliommod_dom.touch_base ();
  end else begin
    current_pseudo_fragment := url_fragment_prefix_with_sharp^uri;
    Eliom_request_info.set_current_path uri;
    if uri <> fst (Url.split_fragment Url.Current.as_string)
    then Dom_html.window##location##hash <- Js.string (url_fragment_prefix^uri)
  end

(* == Relink

   Traverse the Dom representation of the page in order to register
   "unique" nodes (or substitute previously known global nodes) and to
   bind Eliom's event handlers.
*)

let register_event_handlers node attribs =
  List.iter
    (fun ev -> register_event_handler (Js.Unsafe.coerce node : Dom_html.element Js.t) ev)
    attribs

let get_element_cookies_info elt =
  Js.Opt.to_option
    (Js.Opt.map (elt##getAttribute(Js.string Eliom_lib_base.RawXML.ce_call_service_attrib))
       (fun s -> of_json (Js.to_string s)))

let get_element_template elt =
  Js.Opt.to_option
    (Js.Opt.map (elt##getAttribute(Js.string Eliom_lib_base.RawXML.ce_template_attrib))
       (fun s -> Js.to_string s))

let a_handler =
  Dom_html.full_handler
    (fun node ev ->
      let node = Js.Opt.get (Dom_html.CoerceTo.a node) (fun () -> error "not an anchor element") in
      Js.bool (raw_a_handler node (get_element_cookies_info node) (get_element_template node) ev))

let form_handler =
  Dom_html.full_handler
    (fun node ev ->
      let form = Js.Opt.get (Dom_html.CoerceTo.form node) (fun () -> error "not a form element") in
      let kind =
        if String.lowercase(Js.to_string form##_method) = "get"
        then `Form_get
        else `Form_post
      in
      Js.bool (raw_form_handler form kind (get_element_cookies_info form) (get_element_template node) ev))

let relink_process_node (node:Dom_html.element Js.t) =
  let id = Js.Opt.get
    (node##getAttribute(Js.string Eliom_lib_base.RawXML.node_id_attrib))
    (fun () -> error "unique node without id attribute") in
  Js.Optdef.case (find_process_node id)
    (fun () ->
       trace "Relink process node: did not find %s" (Js.to_string id);
       register_process_node id (node:>Dom.node Js.t))
    (fun pnode ->
       trace "Relink process node: found %s" (Js.to_string id);
      Js.Opt.iter (node##parentNode)
        (fun parent -> Dom.replaceChild parent pnode node);
      if String.sub (Js.to_bytestring id) 0 7 <> "global_" then begin
        let childrens = Dom.list_of_nodeList (pnode##childNodes) in
        List.iter (fun c -> ignore(pnode##removeChild(c))) childrens;
        let childrens = Dom.list_of_nodeList (node##childNodes) in
        List.iter (fun c -> ignore(pnode##appendChild(c))) childrens
      end)

let relink_request_node (node:Dom_html.element Js.t) =
  let id = Js.Opt.get
    (node##getAttribute(Js.string Eliom_lib_base.RawXML.node_id_attrib))
    (fun () -> error "unique node without id attribute") in
  Js.Optdef.case (find_request_node id)
    (fun () ->
       trace "Relink request node: did not find %s" (Js.to_string id);
       register_request_node id (node:>Dom.node Js.t))
    (fun pnode ->
       trace "Relink request node: found %s" (Js.to_string id);
       Js.Opt.iter (node##parentNode)
         (fun parent -> Dom.replaceChild parent pnode node))

let relink_request_nodes root =
  trace "Relink request nodes";
  if !Eliom_config.debug_timings then
    Firebug.console##time
      (Js.string "relink_request_nodes");
  Eliommod_dom.iter_nodeList
    (Eliommod_dom.select_request_nodes root)
    relink_request_node;
  if !Eliom_config.debug_timings then
    Firebug.console##timeEnd(Js.string "relink_request_nodes")

(* Relinks a-elements, form-elements, and process nodes. The list of
   closure nodes is returned for application on [relink_closure_node]
   after the client values are initialized.
*)
let relink_page_but_closure_nodes (root:Dom_html.element Js.t) =
  trace "Relink page";
  let (a_nodeList,form_nodeList,process_nodeList,closure_nodeList) =
    Eliommod_dom.select_nodes root in
  Eliommod_dom.iter_nodeList a_nodeList
    (fun node -> node##onclick <- a_handler);
  Eliommod_dom.iter_nodeList form_nodeList
    (fun node -> node##onsubmit <- form_handler);
  Eliommod_dom.iter_nodeList process_nodeList
    relink_process_node;
  closure_nodeList

(* == Rebuild event handlers

   Event handlers inside the DOM tree are rebuilt from the closure map
   sent with the request. The actual functions will be taken from the
   client values.

   It returns a single handler ([unit -> unit]) which captures all
   onload event handlers found in the tree, and cancels the execution
   when on raises [False] (cf. [raw_event_handler]).
*)

let relink_closure_node root onload table (node:Dom_html.element Js.t) =
  trace "Relink closure node";
  let aux attr =
    if attr##value##substring(0,Eliom_lib_base.RawXML.closure_attr_prefix_len) =
      Js.string Eliom_lib_base.RawXML.closure_attr_prefix
    then
      let cid = Js.to_bytestring (attr##value##substring_toEnd(
        Eliom_lib_base.RawXML.closure_attr_prefix_len)) in
      try
        let cv = Xml.ClosureMap.find cid table in
        let closure = raw_event_handler cv in
        if attr##name = Js.string "onload" then
          (if Eliommod_dom.ancessor root node
           (* if not inside a unique node replaced by an older one *)
           then onload := closure :: !onload)
        else Js.Unsafe.set node (attr##name) (Dom_html.handler (fun ev -> Js.bool (closure ev)))
      with Not_found ->
        error "relink_closure_node: client value %s not found" cid
  in
  Eliommod_dom.iter_nodeList (node##attributes:>Dom.attr Dom.nodeList Js.t) aux

let relink_closure_nodes (root : Dom_html.element Js.t) event_handlers closure_nodeList =
  let onload = ref [] in
  Eliommod_dom.iter_nodeList closure_nodeList
    (fun node -> relink_closure_node root onload event_handlers node);
  fun () ->
    let ev = Eliommod_dom.createEvent (Js.string "load") in
    ignore
      (List.for_all (fun f -> f ev) (List.rev !onload))

(* == Extract the request data and the request tab-cookies from a page

   See the corresponding function on the server side:
   Eliom_registration.Eliom_appl_reg_make_param.make_eliom_data_script.
*)

let load_data_script page =
  trace "Load data script";
  let head = Eliommod_dom.get_head page in
  let data_script : Dom_html.scriptElement Js.t =
    match Dom.list_of_nodeList head##childNodes with
    | _ :: _ :: data_script :: _ ->
       let data_script : Dom.element Js.t = Js.Unsafe.coerce data_script in
       (match Js.to_bytestring (data_script##tagName##toLowerCase ()) with
        | "script" -> (Js.Unsafe.coerce data_script)
        | t ->
            Firebug.console##error_4(Js.string "load_data_script: the node ", data_script,
                                     Js.string " is not a script, its tag is", t);
            error "load_data_script: can't find data script (1).")
    | _ -> error "load_data_script: can't find data script (2)."
  in
  let script = data_script##text in
  if !Eliom_config.debug_timings then
    Firebug.console##time(Js.string "load_data_script");
  ignore (Js.Unsafe.eval_string (Js.to_string script));
  Eliom_request_info.reset_request_data ();
  if !Eliom_config.debug_timings then
    Firebug.console##timeEnd(Js.string "load_data_script")

(* == Scroll the current page such that the top of element with the id
   [fragment] is aligned with the window's top. If the optionnal
   argument [?offset] is given, ignore the fragment and scroll to the
   given offset. *)

let scroll_to_fragment ?offset fragment =
  match offset with
  | Some pos -> Eliommod_dom.setDocumentScroll pos
  | None ->
      match fragment with
      | None | Some "" ->
          Eliommod_dom.setDocumentScroll Eliommod_dom.top_position
      | Some fragment ->
          let scroll_to_element e = e##scrollIntoView(Js._true) in
          let elem = Dom_html.document##getElementById(Js.string fragment) in
          Js.Opt.iter elem scroll_to_element

(* == Main (internal) function: change the content of the page without leaving
      the javascript application. *)

let set_content ?uri ?offset ?fragment content =
  trace "Set content";
  match content with
  | None -> Lwt.return ()
  | Some content ->
    try_lwt
       set_loading_phase ();
       if !Eliom_config.debug_timings then
         Firebug.console##time(Js.string "set_content");
       run_callbacks (flush_onunload ());
       (match uri, fragment with
        | Some uri, None -> change_url_string uri
        | Some uri, Some fragment ->
            change_url_string (uri ^ "#" ^ fragment)
        | _ -> ());
       (* Convert the DOM nodes from XML elements to HTML elements. *)
       let fake_page = Eliommod_dom.html_document content registered_process_node in
       (* Inline CSS in the header to avoid the "flashing effect".
          Otherwise, the browser start to display the page before
          loading the CSS. *)
       let preloaded_css = Eliommod_dom.preload_css fake_page in
       (* Unique nodes of scope request must be bound before the
          unmarshalling/unwrapping of page data. *)
       relink_request_nodes fake_page;
       (* Put the loaded data script in action *)
       load_data_script fake_page;
       (* Unmarshall page data. *)
       let cookies = Eliom_request_info.get_request_cookies () in
       let js_data = Eliom_request_info.get_request_data () in
       (* Update tab-cookies. *)
       let host =
         match uri with
         | None -> None
         | Some uri ->
             match Url.url_of_string uri with
             | Some (Url.Http url)
             | Some (Url.Https url) -> Some url.Url.hu_host
             | _ -> None in
       Eliommod_cookies.update_cookie_table host cookies;
       (* Wait for CSS to be inlined before to substitute global nodes. *)
       lwt () = preloaded_css in
       (* Bind unique node (request and global) and register event
          handler.  Relinking closure nodes must take place after
          initializing the client values *)
       let closure_nodeList = relink_page_but_closure_nodes fake_page in
       Eliom_request_info.set_session_info js_data.Eliom_types.ejs_sess_info;
       (* Really change page contents *)
       if !Eliom_config.debug_timings then
         Firebug.console##time(Js.string "replace_page");
       trace "Replace page";
       Dom.replaceChild Dom_html.document
         fake_page
         Dom_html.document##documentElement;
       if !Eliom_config.debug_timings then
         Firebug.console##timeEnd(Js.string "replace_page");
       (* Initialize and provide client values. May need to access to
          new DOM. Necessary for relinking closure nodes *)
       do_request_data js_data.Eliom_types.ejs_request_data;
       (* Replace closure ids in document with event handlers (from client values) *)
       let onload_closure_nodes =
         relink_closure_nodes
           Dom_html.document##documentElement
           js_data.Eliom_types.ejs_event_handler_table closure_nodeList
       in
       (* The request node table must be empty when nodes received via
          call_caml_service are unwrapped. *)
       reset_request_nodes ();
       run_callbacks
         (Eliommod_dom.add_formdata_hack_onclick_handler ::
            flush_onload () @
            [onload_closure_nodes; broadcast_load_end]);
       scroll_to_fragment ?offset fragment;
       if !Eliom_config.debug_timings then
         Firebug.console##timeEnd(Js.string "set_content");
       Lwt.return ()
    with e ->
      debug_exn "set_content: exception raised: " e;
      if !Eliom_config.debug_timings then
        Firebug.console##timeEnd(Js.string "set_content");
      raise_lwt e

let set_template_content ?uri ?fragment = function
  | None -> Lwt.return ()
  | Some content ->
      (* Side-effect in the 'onload' event handler. *)
      run_callbacks (flush_onunload ());
      (match uri, fragment with
        | Some uri, None -> change_url_string uri
        | Some uri, Some fragment ->
          change_url_string (uri ^ "#" ^ fragment)
        | _ -> ());
      lwt (), request_data = unwrap_caml_content content in
      do_request_data request_data;
      run_callbacks (flush_onload ());
      reset_request_nodes ();
      Lwt.return ()

(* == Main (exported) function: change the content of the page without
   leaving the javascript application. See [change_page_uri] for the
   function used to change page when clicking a link and
   [change_page_{get,post}_form] when submiting a form. *)

let change_page
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?(nl_params = Eliom_parameter.empty_nl_params_set) ?keep_get_na_params
    get_params post_params =
  trace "Change page";
  let xhr = Eliom_service.xhr_with_cookies service in
  if xhr = None
  || (https = Some true && not Eliom_request_info.ssl_)
  || (https = Some false && Eliom_request_info.ssl_)
  then
    Lwt.return
      (exit_to
         ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
         ?keep_nl_params ~nl_params ?keep_get_na_params
         get_params post_params)
  else
    match xhr with
      | Some (Some tmpl as t) when t = Eliom_request_info.get_request_template () ->
        let nl_params =
          Eliom_parameter.add_nl_parameter nl_params Eliom_request.nl_template tmpl
        in
        lwt uri, content =
          raw_call_service
            ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
            ?keep_nl_params ~nl_params ?keep_get_na_params
            get_params post_params in
        set_template_content ~uri ?fragment (Some content)
      | _ ->
        let cookies_info = Eliom_uri.make_cookies_info (https, service) in
        lwt (uri, content) = match
            create_request_
              ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
              ?keep_nl_params ~nl_params ?keep_get_na_params
              get_params post_params
          with
            | `Get uri ->
              Eliom_request.http_get
                ~expecting_process_page:true ?cookies_info uri []
                Eliom_request.xml_result
            | `Post (uri, p) ->
              Eliom_request.http_post
                ~expecting_process_page:true ?cookies_info uri p
                Eliom_request.xml_result
        in
        let uri, fragment = Url.split_fragment uri in
        set_content ~uri ?fragment content

(* Function used in "onclick" event handler of <a>.  *)

let change_page_uri ?cookies_info ?tmpl ?(get_params = []) full_uri =
  trace "Change page uri";
  let uri, fragment = Url.split_fragment full_uri in
  if uri <> !current_uri || fragment = None then begin
    match tmpl with
    | Some t when tmpl = Eliom_request_info.get_request_template () ->
        lwt (uri, content) = Eliom_request.http_get
          ?cookies_info uri
          ((Eliom_request.nl_template_string, t) :: get_params)
          Eliom_request.string_result
        in
        set_template_content ~uri ?fragment content
    | _ ->
        lwt (uri, content) = Eliom_request.http_get
          ~expecting_process_page:true ?cookies_info uri get_params
          Eliom_request.xml_result
        in
        set_content ~uri ?fragment content
  end else
    ( change_url_string full_uri;
      scroll_to_fragment fragment;
      Lwt.return () )

(* Functions used in "onsubmit" event handler of <form>.  *)

let change_page_get_form ?cookies_info ?tmpl form full_uri =
  let form = Js.Unsafe.coerce form in
  let uri, fragment = Url.split_fragment full_uri in
  match tmpl with
  | Some t when tmpl = Eliom_request_info.get_request_template () ->
      lwt uri, content = Eliom_request.send_get_form
        ~get_args:[Eliom_request.nl_template_string, t]
        ?cookies_info form uri
        Eliom_request.string_result
      in
      set_template_content ~uri ?fragment content
  | _ ->
    lwt uri, content = Eliom_request.send_get_form
      ~expecting_process_page:true ?cookies_info form uri
      Eliom_request.xml_result
    in
    set_content ~uri ?fragment content

let change_page_post_form ?cookies_info ?tmpl form full_uri =
  let form = Js.Unsafe.coerce form in
      let uri, fragment = Url.split_fragment full_uri in
  match tmpl with
  | Some t when tmpl = Eliom_request_info.get_request_template () ->
      lwt uri, content = Eliom_request.send_post_form
        ~get_args:[Eliom_request.nl_template_string, t]
        ?cookies_info form uri
        Eliom_request.string_result
      in
      set_template_content ~uri ?fragment content
  | _ ->
      lwt uri, content = Eliom_request.send_post_form
        ~expecting_process_page:true ?cookies_info form uri
        Eliom_request.xml_result
      in
      set_content ~uri ?fragment content

let _ =
  change_page_uri_ :=
    (fun ?cookies_info ?tmpl href ->
      lwt_ignore(change_page_uri ?cookies_info ?tmpl href));
  change_page_get_form_ :=
    (fun ?cookies_info ?tmpl form href ->
      lwt_ignore(change_page_get_form ?cookies_info ?tmpl form href));
  change_page_post_form_ :=
    (fun ?cookies_info ?tmpl form href ->
      lwt_ignore(change_page_post_form ?cookies_info ?tmpl form href))

(* == Navigating through the history... *)

let () =

  if Eliom_process.history_api then

    let goto_uri full_uri state_id =
      leave_page ();
      current_state_id := state_id;
      let state = get_state state_id in
      let tmpl = (if state.template = Js.string "" then None else Some (Js.to_string state.template))in
      lwt_ignore
        (let uri, fragment = Url.split_fragment full_uri in
         if uri <> !current_uri then begin
           current_uri := uri;
           match tmpl with
           | Some t when tmpl = Eliom_request_info.get_request_template () ->
               lwt (uri, content) = Eliom_request.http_get
                 uri [(Eliom_request.nl_template_string, t)]
                 Eliom_request.string_result
               in
               set_template_content content >>
                 ( scroll_to_fragment ~offset:state.position fragment;
                   Lwt.return ())
           | _ ->
               lwt uri, content =
                 Eliom_request.http_get ~expecting_process_page:true uri []
                   Eliom_request.xml_result in
               set_content ~offset:state.position ?fragment content
         end else
           ( scroll_to_fragment ~offset:state.position fragment;
             Lwt.return () ))
    in

    lwt_ignore
      ( lwt () = wait_load_end () in
        Dom_html.window##history##replaceState(Js.Opt.return !current_state_id,
                                               Js.string "",
                                               Js.some Dom_html.window##location##href );
        Lwt.return ());

    Dom_html.window##onpopstate <-
      Dom_html.handler (fun event ->
        let full_uri = Js.to_string Dom_html.window##location##href in
        Eliommod_dom.touch_base ();
        Js.Opt.case (Obj.magic event##state : int Js.opt)
          (fun () -> () (* Ignore dummy popstate event fired by chromium. *))
          (goto_uri full_uri);
        Js._false)

  else (* Whithout history API *)

    (* FIXME: This should be adapted to work with template...
       Solution: add the "state_id" in the fragment ??
    *)

    let read_fragment () = Js.to_string Dom_html.window##location##hash in
    let auto_change_page fragment =
      lwt_ignore
        (let l = String.length fragment in
         if (l = 0) || ((l > 1) && (fragment.[1] = '!'))
         then if fragment <> !current_pseudo_fragment then
           ( current_pseudo_fragment := fragment;
             let uri =
               match l with
               | 2 -> "./" (* fix for firefox *)
               | 0 | 1 -> fst (Url.split_fragment Url.Current.as_string)
               | _ -> String.sub fragment 2 ((String.length fragment) - 2)
             in
             (* CCC TODO handle templates *)
             change_page_uri uri)
           else Lwt.return ()
         else Lwt.return () )
    in

    Eliommod_dom.onhashchange (fun s -> auto_change_page (Js.to_string s));
    let first_fragment = read_fragment () in
    if first_fragment <> !current_pseudo_fragment then
      lwt_ignore (
        lwt () = wait_load_end () in
        auto_change_page first_fragment;
        Lwt.return ())

(* Type for partially unwrapped elt. *)
type tmp_recontent =
  (* arguments ('econtent') are already unwrapped. *)
  | RELazy of Xml.econtent Eliom_lazy.request
  | RE of Xml.econtent
type tmp_elt = {
  (* to be unwrapped *)
  tmp_elt : tmp_recontent;
  tmp_node_id : Xml.node_id;
}

let rebuild_attrib node name a = match a with
  | Xml.AFloat f -> Js.Unsafe.set node (Js.string name) (Js.Unsafe.inject f)
  | Xml.AInt i -> Js.Unsafe.set node (Js.string name) (Js.Unsafe.inject i)
  | Xml.AStr s ->
    node##setAttribute(Js.string name, Js.string s)
  | Xml.AStrL (Xml.Space, sl) ->
    node##setAttribute(Js.string name, Js.string (String.concat " " sl))
  | Xml.AStrL (Xml.Comma, sl) ->
    node##setAttribute(Js.string name, Js.string (String.concat "," sl))

let rebuild_rattrib node ra = match Xml.racontent ra with
  | Xml.RA a -> rebuild_attrib node (Xml.aname ra) a
  | Xml.RACamlEventHandler ev -> register_event_handler node (Xml.aname ra, ev)
  | Xml.RALazyStr s ->
      node##setAttribute(Js.string (Xml.aname ra), Js.string s)
  | Xml.RALazyStrL (Xml.Space, l) ->
      node##setAttribute(Js.string (Xml.aname ra), Js.string (String.concat " " l))
  | Xml.RALazyStrL (Xml.Comma, l) ->
      node##setAttribute(Js.string (Xml.aname ra), Js.string (String.concat "," l))

let rec rebuild_node elt =
  match Xml.get_node elt with
  | Xml.DomNode node ->
      (* assert (Xml.get_node_id node <> NoId); *)
      node
  | Xml.TyXMLNode raw_elt ->
      match Xml.get_node_id elt with
      | Xml.NoId -> raw_rebuild_node raw_elt
      | Xml.RequestId _ ->
          (* Do not look in request_nodes hashtbl: such elements have
             been bind while unwrapping nodes. *)
          let node = raw_rebuild_node raw_elt in
          Xml.set_dom_node elt node;
          node
      | Xml.ProcessId id ->
        let id = (Js.string id) in
        Js.Optdef.case (find_process_node id)
          (fun () ->
            let node = raw_rebuild_node (Xml.content elt) in
            register_process_node id node;
            node)
          (fun n -> (n:> Dom.node Js.t))


and raw_rebuild_node = function
  | Xml.Empty
  | Xml.Comment _ ->
      (* FIXME *)
      (Dom_html.document##createTextNode (Js.string "") :> Dom.node Js.t)
  | Xml.EncodedPCDATA s
  | Xml.PCDATA s -> (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)
  | Xml.Entity s -> assert false (* FIXME *)
  | Xml.Leaf (name,attribs) ->
    let node = Dom_html.document##createElement (Js.string name) in
    List.iter (rebuild_rattrib node) attribs;
    (node :> Dom.node Js.t)
  | Xml.Node (name,attribs,childrens) ->
    let node = Dom_html.document##createElement (Js.string name) in
    List.iter (rebuild_rattrib node) attribs;
    List.iter (fun c -> Dom.appendChild node (rebuild_node c)) childrens;
    (node :> Dom.node Js.t)

let rebuild_node elt =
  let node = Js.Unsafe.coerce (rebuild_node (Html5.F.toelt elt)) in
  flush_load_script ();
  node

(******************************************************************************)
(*                            Register unwrappers                             *)

(* == Html5 elements
   
   Html5 elements are unwrapped lazily (cf. use of Xml.make_lazy in
   unwrap_tyxml), because the unwrapping of process and request
   elements needs access to the DOM.

   All recently unwrapped elements are forced when resetting the
   request nodes ([reset_request_nodes]).
*)

let unwrap_tyxml =
  fun tmp_elt ->
    let elt = match tmp_elt.tmp_elt with
      | RELazy elt -> Eliom_lazy.force elt
      | RE elt -> elt
    in
    trace "Unwrap tyxml";
    (* Do not rebuild dom node while unwrapping, otherwise we
       don't have control on when "onload" event handlers are
       triggered. *)
    let elt =
      Xml.make_lazy ~id:tmp_elt.tmp_node_id
        (lazy
           (match tmp_elt.tmp_node_id with
              | Xml.ProcessId process_id as id ->
                  trace "Unwrap tyxml from ProcessId %s" process_id;
                  Js.Optdef.case (find_process_node (Js.bytestring process_id))
                    (fun () ->
                       trace "not found";
                       let xml_elt : Xml.elt = Xml.make ~id elt in
                       let html_elt = (Obj.magic xml_elt : _ Html5.elt) in
                       let html_elt = Html5.set_classes_of_elt html_elt in
                       register_process_node (Js.bytestring process_id) (rebuild_node html_elt);
                       (Obj.magic html_elt : Xml.elt))
                    (fun elt ->
                       trace "found";
                       Xml.make_dom ~id elt)
              | Xml.RequestId request_id as id ->
                  trace "Unwrap tyxml from RequestId %s" request_id;
                  Js.Optdef.case (find_request_node (Js.bytestring request_id))
                    (fun () ->
                       trace "not found";
                       let xml_elt : Xml.elt = Xml.make ~id elt in
                       let html_elt = (Obj.magic xml_elt : _ Html5.elt) in
                       let html_elt = Html5.set_classes_of_elt html_elt in
                       register_request_node (Js.bytestring request_id) (rebuild_node html_elt);
                       (Obj.magic html_elt : Xml.elt))
                    (fun elt -> trace "found"; Xml.make_dom ~id elt)
              | Xml.NoId as id ->
                  trace "Unwrap tyxml from NoId";
                  Xml.make ~id elt))
    in
    register_unwrapped_elt elt;
    elt

let unwrap_global_data =
  fun (global_data', _) ->
    global_data := global_data'

let _ =
  (* BBB No unwrapper for Client_value.t! They are explicitly late
     unwrapped by Eliom_unwrap.late_unwrap_value in
     [Client_value.initialize]. *)
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_lib_base.tyxml_unwrap_id_int)
    unwrap_tyxml;
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_lib_base.global_data_unwrap_id_int)
    unwrap_global_data;
  ()

(******************************************************************************)

module Syntax_helpers = struct

  let register_client_closure closure_id closure =
    Client_closure.register ~closure_id ~closure

  let open_client_section compilation_unit_id =
    do_next_client_section_data ~compilation_unit_id

  let close_server_section compilation_unit_id =
    do_next_server_section_data ~compilation_unit_id

  let get_escaped_value =
    from_poly

  let get_injection name =
    Injection.get ~name

end
