(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Copyright (C) 2011 Jérôme Vouillon, Grégoire Henry, Pierre Chambart
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

(* == Closure *)

let closure_table  : (poly -> Dom_html.event Js.t -> unit) JsTable.t = JsTable.create ()
let register_closure (id:int64) (f : 'a -> Dom_html.event Js.t -> unit) =
  JsTable.add closure_table ((Obj.magic id)##toString():Js.js_string Js.t)
    (Obj.magic f : poly -> Dom_html.event Js.t -> unit)
let find_closure (id:int64) : ( poly -> Dom_html.event Js.t -> unit ) Js.Optdef.t =
  JsTable.find closure_table ((Obj.magic id)##toString():Js.js_string Js.t)

(* == Process nodes (a.k.a. nodes with a unique Dom instance on each client process) *)

let (register_process_node, find_process_node) =
  let process_nodes : Dom.node Js.t JsTable.t = JsTable.create () in
  let find id =
    Js.Optdef.bind
      (JsTable.find process_nodes id)
      (fun node ->
        if Js.to_bytestring (node##nodeName##toLowerCase()) == "script" then
          (* We don't wan't to reexecute global script. *)
          Js.def (Dom_html.document##createTextNode (Js.string "") :> Dom.node Js.t)
        else
          Js.def node)
  in
  let register id node = JsTable.add process_nodes id node in
  (register, find)
let registered_process_node id = Js.Optdef.test (find_process_node id)
let getElementById id =
  Js.Optdef.case (find_process_node (Js.string id))
    (fun () -> raise Not_found)
    (fun pnode -> pnode)

(* == Request nodes (a.k.a. nodes with a unique Dom instance in the current request) *)

let (register_request_node, find_request_node, reset_request_node) =
  let request_nodes : Dom.node Js.t JsTable.t ref = ref (JsTable.create ()) in
  let find id = JsTable.find !request_nodes id in
  let register id node = JsTable.add !request_nodes id node in
  let reset () = request_nodes := JsTable.create () in
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

let loading_phase = ref true
let load_end = Lwt_condition.create ()

let in_onload () = !loading_phase
let broadcast_load_end _ =
  loading_phase := false;
  Lwt_condition.broadcast load_end ();
  true
let wait_load_end () =
  if !loading_phase
  then Lwt_condition.wait load_end
  else Lwt.return ()

(* == List of event handlers to execute after loading a page. *)

let run_load_events on_load =
  let load_evt = Eliommod_dom.createEvent (Js.string "load") in
  ignore (List.for_all (fun f -> f load_evt) on_load)

(* == List of event handlers to execute before to leave a page. *)

let on_unload_scripts = ref []
let on_unload f =
  on_unload_scripts :=
    (fun () -> try f(); true with False -> false) :: !on_unload_scripts

let run_unload_events () =
  ignore (List.for_all (fun f -> f ()) !on_unload_scripts);
  on_unload_scripts := []

(* == Helper's functions for Eliom's event handler.

   Allow to convert XML.event_handler to javascript closure and to
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

let raw_event_handler function_id args =
  Js.Optdef.case (find_closure function_id)
    (fun () -> error "Closure not found (%Ld)" function_id)
    (fun f -> (fun ev -> try f args ev; true with False -> false))

let reify_caml_event node ce : #Dom_html.event Js.t -> bool = match ce with
  | XML.CE_call_service None -> (fun _ -> true)
  | XML.CE_call_service (Some (`A, cookies_info, tmpl)) ->
      (fun ev ->
        let node = Js.Opt.get (Dom_html.CoerceTo.a node) (fun () -> error "not an anchor element") in
        raw_a_handler node cookies_info tmpl ev)
  | XML.CE_call_service (Some ((`Form_get | `Form_post) as kind, cookies_info, tmpl)) ->
      (fun ev ->
        let form = Js.Opt.get (Dom_html.CoerceTo.form node) (fun () -> error "not a form element") in
        raw_form_handler form kind cookies_info tmpl ev)
  | XML.CE_client_closure f ->
      (fun ev -> try f ev; true with False -> false)
  | XML.CE_registered_closure (_, (function_id, args)) ->
      raw_event_handler function_id args

let reify_event node ev = match ev with
  | XML.Raw ev -> Js.Unsafe.variable ev
  | XML.Caml ce -> reify_caml_event node ce

let on_load_scripts = ref []
let register_event_handler node (name, ev) =
  let f = reify_caml_event node ev in
  if name = "onload" then
    on_load_scripts := f :: !on_load_scripts
  else
    Js.Unsafe.set node (Js.bytestring name)
        (Dom_html.handler (fun ev -> Js.bool (f ev)))

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
  run_unload_events ()

let () =
  ignore
    (Dom.addEventListener Dom_html.window
       (Dom.Event.make "unload")
       (Dom_html.handler (fun _ -> leave_page (); Js._false))
       Js._false)

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

  match Eliom_services.get_get_or_post service with
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

      Unwrap the data and execute the associated onload event handlers.

*)

let unwrap_caml_content content =
  let r : 'a Eliom_types.eliom_caml_service_data =
    Eliom_unwrap.unwrap (Url.decode content) 0 in
  let on_load =
    List.map
      (reify_caml_event Dom_html.document##documentElement)
      r.Eliom_types.ecs_onload in
  run_load_events on_load;
  Lwt.return r.Eliom_types.ecs_data

let call_caml_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  lwt _, content =
    raw_call_service
      ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?keep_get_na_params
      get_params post_params in
  lwt content = unwrap_caml_content content in
  Lwt.return content

(* == The function [change_url_string] changes the URL, without doing a request.

   It uses the History API if present, otherwise we write the new URL
   in the fragment part of the URL (see 'redirection_script' in
   'server/eliom_output.ml'). *)

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
    (fun () -> register_process_node id (node:>Dom.node Js.t))
    (fun pnode ->
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
    (fun () -> register_request_node id (node:>Dom.node Js.t))
    (fun pnode ->
      Js.Opt.iter (node##parentNode)
        (fun parent -> Dom.replaceChild parent pnode node))

let relink_request_nodes root =
  if !Eliom_config.debug_timings then
    Firebug.console##time
      (Js.string "relink_request_nodes");
  Eliommod_dom.iter_nodeList
    (Eliommod_dom.select_request_nodes root)
    relink_request_node;
  if !Eliom_config.debug_timings then
    Firebug.console##timeEnd(Js.string "relink_request_nodes")

let relink_closure_node root onload table (node:Dom_html.element Js.t) =
  let aux attr =
    if attr##value##substring(0,Eliom_lib_base.RawXML.closure_attr_prefix_len) =
      Js.string Eliom_lib_base.RawXML.closure_attr_prefix
    then
      let cid = Js.to_bytestring (attr##value##substring_toEnd(
        Eliom_lib_base.RawXML.closure_attr_prefix_len)) in
      let (id,args) = XML.ClosureMap.find cid table in
      let closure = raw_event_handler id args in
      if attr##name = Js.string "onload" then
        (if Eliommod_dom.ancessor root node
         (* if not inside a unique node replaced by an older one *)
         then onload := closure :: !onload)
      else Js.Unsafe.set node (attr##name) (Dom_html.handler (fun ev -> Js.bool (closure ev)))
  in
  Eliommod_dom.iter_nodeList (node##attributes:>Dom.attr Dom.nodeList Js.t) aux

let relink_page (root:Dom_html.element Js.t) event_handlers =
  let (a_nodeList,form_nodeList,process_nodeList,closure_nodeList) =
    Eliommod_dom.select_nodes root in
  Eliommod_dom.iter_nodeList a_nodeList
    (fun node -> node##onclick <- a_handler);
  Eliommod_dom.iter_nodeList form_nodeList
    (fun node -> node##onsubmit <- form_handler);
  Eliommod_dom.iter_nodeList process_nodeList
    relink_process_node;
  let onload = ref [] in
  Eliommod_dom.iter_nodeList closure_nodeList
    (fun node -> relink_closure_node root onload event_handlers node);
  List.rev !onload

let load_eliom_data js_data page =
  try
    if !Eliom_config.debug_timings then
      Firebug.console##time(Js.string "load_eliom_data");
    loading_phase := true;
    let nodes_on_load =
      relink_page page js_data.Eliom_types.ejs_event_handler_table
    in
    Eliom_request_info.set_session_info js_data.Eliom_types.ejs_sess_info;
    let on_load =
      List.map
        (reify_caml_event Dom_html.document##documentElement)
        js_data.Eliom_types.ejs_onload
    in
    let on_unload =
      List.map
        (reify_caml_event Dom_html.document##documentElement)
        js_data.Eliom_types.ejs_onunload
    in
    let unload_evt = Eliommod_dom.createEvent (Js.string "unload") in
    on_unload_scripts :=
      [fun () -> List.for_all (fun f -> f unload_evt) on_unload];
    if !Eliom_config.debug_timings then
      Firebug.console##timeEnd(Js.string "load_eliom_data");
    Eliommod_dom.add_formdata_hack_onclick_handler :: on_load @ nodes_on_load @ [broadcast_load_end]
  with e ->
    debug_exn "load_eliom_data failed: " e;
    raise e

(* == Extract the request data and the request tab-cookies from a
      page.

   See the corresponding function on the server side:
   Eliom_output.Eliom_appl_reg_make_param.make_eliom_data_script.

*)

let load_data_script page =
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
  if !Eliom_config.debug_timings then
    Firebug.console##timeEnd(Js.string "load_data_script");
  ( Eliom_request_info.get_request_data (),
    Eliom_request_info.get_request_cookies ())

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

(* == Main (internal) function: change the content of hte page without leaving
      the javascript application. *)

let set_content ?uri ?offset ?fragment = function
  | None -> Lwt.return ()
  | Some content ->
    if !Eliom_config.debug_timings then
      Firebug.console##time(Js.string "set_content");
    try_lwt
      if !Eliom_config.debug_timings then
        Firebug.console##time(Js.string "set_content_beginning");
       run_unload_events ();
       (match uri, fragment with
        | Some uri, None -> change_url_string uri
        | Some uri, Some fragment ->
            change_url_string (uri ^ "#" ^ fragment)
        | _ -> ());
       (* Convert the DOM nodes from XML elements to HTML elements. *)
       let fake_page = Eliommod_dom.html_document content registered_process_node in
       if !Eliom_config.debug_timings then
         ( Firebug.console##timeEnd(Js.string "set_content_beginning");
           Firebug.console##debug(Js.string ("loading: " ^ Js.to_string Dom_html.window##location##href)) );
       (* Inline CSS in the header to avoid the "flashing effect".
          Otherwise, the browser start to display the page before
          loading the CSS. *)
       let preloaded_css = Eliommod_dom.preload_css fake_page in
       (* Unique nodes of scope request must be bound before the
          unmarshalling/unwrapping of page data. *)
       relink_request_nodes fake_page;
       (* Unmarshall page data. *)
       let js_data, cookies = load_data_script fake_page in
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
       (* Wait for CSS to be inlined before to substitute global
          nodes. *)
       lwt () = preloaded_css in
       (* Bind unique node (named and global) and register event
          handler. *)
       let on_load = load_eliom_data js_data fake_page in
       (* The request node table must be empty when node received
          via call_caml_service are unwrapped. *)
       reset_request_node ();
       (* Really change page contents *)
       if !Eliom_config.debug_timings then
         Firebug.console##time(Js.string "replace_child");
       Dom.replaceChild Dom_html.document
         fake_page
         Dom_html.document##documentElement;
       if !Eliom_config.debug_timings then
         ( Firebug.console##timeEnd(Js.string "replace_child");
           Firebug.console##time(Js.string "set_content_end") );
       run_load_events on_load;
       scroll_to_fragment ?offset fragment;
       if !Eliom_config.debug_timings then
         ( Firebug.console##timeEnd(Js.string "set_content_end");
           Firebug.console##timeEnd(Js.string "set_content") );
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
      run_unload_events ();
      (match uri, fragment with
        | Some uri, None -> change_url_string uri
        | Some uri, Some fragment ->
          change_url_string (uri ^ "#" ^ fragment)
        | _ -> ());
      lwt () = unwrap_caml_content content in
      Lwt.return ()

(* == Main (exported) function: change the content of the page without
   leaving the javascript application. See [change_page_uri] for the
   function used to change page when clicking a link and
   [change_page_{get,post}_form] when submiting a form. *)

let change_page
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?(nl_params = Eliom_parameters.empty_nl_params_set) ?keep_get_na_params
    get_params post_params =

  let xhr = Eliom_services.xhr_with_cookies service in
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
          Eliom_parameters.add_nl_parameter nl_params Eliom_request.nl_template tmpl
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
  | RELazy of XML.econtent Eliom_lazy.request
  | RE of XML.econtent
type tmp_elt = {
  (* to be unwrapped *)
  tmp_elt : tmp_recontent;
  tmp_node_id : XML.node_id;
}

let _ =
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_lib_base.tyxml_unwrap_id_int)
    (fun tmp_elt ->
      let elt = match tmp_elt.tmp_elt with
        | RELazy elt -> Eliom_lazy.force elt
        | RE elt -> elt
      in
      (* Do not rebuild dom node while unwrapping, otherwise we
         don't have control on when "onload" event handlers are
         triggered. *)
      match tmp_elt.tmp_node_id with
      | XML.ProcessId process_id as id ->
          Js.Optdef.case (find_process_node (Js.bytestring process_id))
            (fun () -> XML.make ~id elt)
            (fun elt -> XML.make_dom ~id elt)
      | XML.RequestId request_id as id ->
          Js.Optdef.case (find_request_node (Js.bytestring request_id))
            (fun () -> XML.make ~id elt)
            (fun elt -> XML.make_dom ~id elt)
      | XML.NoId as id -> XML.make ~id elt)

let rebuild_attrib node name a = match a with
  | XML.AFloat f -> Js.Unsafe.set node (Js.string name) (Js.Unsafe.inject f)
  | XML.AInt i -> Js.Unsafe.set node (Js.string name) (Js.Unsafe.inject i)
  | XML.AStr s ->
    node##setAttribute(Js.string name, Js.string s)
  | XML.AStrL (XML.Space, sl) ->
    node##setAttribute(Js.string name, Js.string (String.concat " " sl))
  | XML.AStrL (XML.Comma, sl) ->
    node##setAttribute(Js.string name, Js.string (String.concat "," sl))

let rebuild_rattrib node ra = match XML.racontent ra with
  | XML.RA a -> rebuild_attrib node (XML.aname ra) a
  | XML.RACamlEventHandler ev -> register_event_handler node (XML.aname ra, ev)
  | XML.RALazyStr s ->
      node##setAttribute(Js.string (XML.aname ra), Js.string s)
  | XML.RALazyStrL (XML.Space, l) ->
      node##setAttribute(Js.string (XML.aname ra), Js.string (String.concat " " l))
  | XML.RALazyStrL (XML.Comma, l) ->
      node##setAttribute(Js.string (XML.aname ra), Js.string (String.concat "," l))

let rec rebuild_node elt =
  match XML.get_node elt with
  | XML.DomNode node ->
      (* assert (XML.get_node_id node <> NoId); *)
      node
  | XML.TyXMLNode raw_elt ->
      match XML.get_node_id elt with
      | XML.NoId -> raw_rebuild_node raw_elt
      | XML.RequestId _ ->
          (* Do not look in request_nodes hashtbl: such elements have
             been bind while unwrapping nodes. *)
          let node = raw_rebuild_node raw_elt in
          XML.set_dom_node elt node;
          node
      | XML.ProcessId id ->
        let id = (Js.string id) in
        Js.Optdef.case (find_process_node id)
          (fun () ->
            let node = raw_rebuild_node (XML.content elt) in
            register_process_node id node;
            node)
          (fun n -> (n:> Dom.node Js.t))


and raw_rebuild_node = function
  | XML.Empty
  | XML.Comment _ ->
      (* FIXME *)
      (Dom_html.document##createTextNode (Js.string "") :> Dom.node Js.t)
  | XML.EncodedPCDATA s
  | XML.PCDATA s -> (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)
  | XML.Entity s -> assert false (* FIXME *)
  | XML.Leaf (name,attribs) ->
    let node = Dom_html.document##createElement (Js.string name) in
    List.iter (rebuild_rattrib node) attribs;
    (node :> Dom.node Js.t)
  | XML.Node (name,attribs,childrens) ->
    let node = Dom_html.document##createElement (Js.string name) in
    List.iter (rebuild_rattrib node) attribs;
    List.iter (fun c -> Dom.appendChild node (rebuild_node c)) childrens;
    (node :> Dom.node Js.t)

let rebuild_node elt =
  let node = Js.Unsafe.coerce (rebuild_node (HTML5.F.toelt elt)) in
  run_load_events (List.rev !on_load_scripts);
  on_load_scripts := [];
  node

