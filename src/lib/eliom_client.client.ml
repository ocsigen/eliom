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

module Xml = Eliom_content_core.Xml

module JsTable = Eliommod_jstable

let init_client_app ?(ssl = false) ~hostname ?(port = 80) ~full_path () =
  Eliom_request_info.client_app_initialised := true;
  Eliom_process.set_sitedata
    {Eliom_types.site_dir = full_path;
     site_dir_string = String.concat "/" full_path};
  Eliom_process.set_info {Eliom_common.cpi_ssl = ssl ;
                          cpi_hostname = hostname;
                          cpi_server_port = port;
                          cpi_original_full_path = full_path
                         };
  Eliom_process.set_request_template None;
  Eliom_process.set_request_cookies Ocsigen_cookies.Cookies.empty


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
  val find : closure_id:int64 -> instance_id:int64 -> _
  val initialize : client_value_datum -> unit
end = struct

  let table = JsTable.create ()

  let closure_key closure_id =
    Js.string (Int64.to_string closure_id)

  let instance_key instance_id =
    Js.string (Int64.to_string instance_id)

  let find ~closure_id ~instance_id =
    trace "Get client value %Ld/%Ld" closure_id instance_id;
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
    trace "Initialize client value %Ld/%Ld" closure_id instance_id;
    let closure =
      try
        Client_closure.find ~closure_id
      with Not_found ->
        Eliom_lib.error "Client closure %Ld not found (is the module linked on the client?)"
          closure_id
    in
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
  trace "Do next client value data section in compilation unit %s" compilation_unit_id;
  try
    let data = String_map.find compilation_unit_id !global_data in
    List.iter Client_value.initialize
      (Queue.take data.server_sections_data)
  with
    | Not_found -> () (* Client-only compilation unit *)
    | Queue.Empty ->
      error "Queue of client value data for compilation unit %s is empty \
             (is it linked on the server?)"
        compilation_unit_id

let do_next_client_section_data ~compilation_unit_id =
  trace "Do next injection data section in compilation unit %s" compilation_unit_id;
  try
    let data = String_map.find compilation_unit_id !global_data in
    List.iter Injection.initialize
      (Queue.take data.client_sections_data)
  with
    | Not_found -> () (* Client-only compilation unit *)
    | Queue.Empty ->
      error "Queue of injection data for compilation unit %s is empty \
             (is it linked on the server?)"
        compilation_unit_id

(* == Initialize the client values sent with a request *)

let do_request_data request_data =
  trace "Do request data (%a)" (fun () l -> string_of_int (List.length l)) request_data;
  (* On a request, i.e. after running the toplevel definitions, global_data
     must contain at most empty sections_data lists, which stem from server-
     only eliom files. *)
  String_map.iter
    (fun _ { server_sections_data; client_sections_data } ->
       Queue.iter
         (function
           | [] -> ()
           | data ->
             Printf.ksprintf (fun s -> Firebug.console##error(Js.string s))
               "Code generating the following client values is not linked on the client: %s"
               (String.concat ","
                  (List.map
                     (fun d -> Printf.sprintf "%Ld/%Ld" d.closure_id d.instance_id)
                     data)))
         server_sections_data;
       Queue.iter
         (function
           | [] -> ()
           | data ->
             Printf.ksprintf (fun s -> Firebug.console##error(Js.string s))
               "Code containing the following injections is not linked on the client: %s"
               (String.concat ","
                  (List.map (fun d -> d.Eliom_lib_base.injection_id) data)))
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
    trace "Find process node %a" (fun () -> Js.to_string) id;
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
    trace "Register request node %a" (fun () -> Js.to_string) id;
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

(* [is_before_initial_load] tests whether it is executed before the
   loading of the initial document, e.g. during the initialization of the
   (OCaml) module, i.e. before [Eliom_client_main.onload]. *)
let is_before_initial_load, set_initial_load =
  let before_load = ref true in
  (fun () -> !before_load),
  (fun () -> before_load := false)

(* == Organize the phase of loading or change_page

   In the following functions, onload referers the initial loading phase
   *and* to the phange_page phase. *)

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

   Allow conversion of Xml.event_handler to javascript closure and
   their registration in Dom node.

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
    error "Client value %Ld/%Ld not found as event handler" closure_id instance_id
let closure_name_prefix = Eliom_lib_base.RawXML.closure_name_prefix
let closure_name_prefix_len = String.length closure_name_prefix
let reify_caml_event name node ce : string * (#Dom_html.event Js.t -> bool) = match ce with
  | Xml.CE_call_service None -> name,(fun _ -> true)
  | Xml.CE_call_service (Some (`A, cookies_info, tmpl)) ->
      name,(fun ev ->
        let node = Js.Opt.get (Dom_html.CoerceTo.a node) (fun () -> error "not an anchor element") in
        raw_a_handler node cookies_info tmpl ev)
  | Xml.CE_call_service (Some ((`Form_get | `Form_post) as kind, cookies_info, tmpl)) ->
      name,(fun ev ->
        let form = Js.Opt.get (Dom_html.CoerceTo.form node) (fun () -> error "not a form element") in
        raw_form_handler form kind cookies_info tmpl ev)
  | Xml.CE_client_closure f ->
      name,(fun ev -> try f ev; true with False -> false)
  | Xml.CE_registered_closure (_, cv) ->
    let name =
      let len = String.length name in
      if len > closure_name_prefix_len && String.sub name 0 closure_name_prefix_len = closure_name_prefix
      then String.sub name closure_name_prefix_len (len - closure_name_prefix_len)
      else name in
    name, raw_event_handler cv

let register_event_handler, flush_load_script =
  let add, flush = create_buffer () in
  let register node (name, ev) =
    let name,f = reify_caml_event name node ev in
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


let rebuild_attrib_val = function
  | Xml.AFloat f -> (Js.number_of_float f)##toString()
  | Xml.AInt i ->   (Js.number_of_float (float_of_int i))##toString()
  | Xml.AStr s ->   Js.string s
  | Xml.AStrL (Xml.Space, sl) -> Js.string (String.concat " " sl)
  | Xml.AStrL (Xml.Comma, sl) -> Js.string (String.concat "," sl)


(* html attributes and dom properties use differant names
   **exemple**: maxlength vs maxLenght (case sensitive).
   - Before dom react, it was enought to set html attributes only as
   there were no update after creation.
   - Dom React may update attributes later.
   Html attrib changes are not taken into account if the corresponding
   Dom property is defined.
   **exemple**: udpating html attribute `value` has no effect if the dom property
   `value` has be set by the user.

   =WE NEED TO SET DOM PROPERTIES=
   -Tyxml only gives us html attribute names and we can set them safely.
   -The name for dom properties is maybe differant.
    We set it only if we find out that the property match_the_attribute_name / is_alerady_defined (get_prop).
*)

(* TODO: fix get_prop
   it only work when html attribute and dom property names correspond.
   find a way to get dom property name corresponding to html attribute
*)

let get_prop node name =
  if Js.Optdef.test (Js.Unsafe.get node name)
  then Some name
  else None

let iter_prop node name f =
  match get_prop node name with
  | Some n -> f n
  | None -> ()

let rebuild_rattrib node ra = match Xml.racontent ra with
  | Xml.RA a ->
    let name = Xml.aname ra in
    let v = rebuild_attrib_val a in
    node##setAttribute (name,v);
  | Xml.RAReact s ->
    let name = Js.string (Xml.aname ra) in
    let _ = React.S.map (function
        | None ->
          node##removeAttribute (name);
          iter_prop node name (Js.Unsafe.delete node);
        | Some v ->
          let v = rebuild_attrib_val v in
          node##setAttribute (name,v);
          iter_prop node name (fun name -> Js.Unsafe.set node name v);
      ) s in ()
  | Xml.RACamlEventHandler ev -> register_event_handler node (Xml.aname ra, ev)
  | Xml.RALazyStr s ->
      node##setAttribute(Js.string (Xml.aname ra), Js.string s)
  | Xml.RALazyStrL (Xml.Space, l) ->
      node##setAttribute(Js.string (Xml.aname ra), Js.string (String.concat " " l))
  | Xml.RALazyStrL (Xml.Comma, l) ->
    node##setAttribute(Js.string (Xml.aname ra), Js.string (String.concat "," l))
  | Xml.RAClient _ -> assert false


(* == Associate data to state of the History API.

   We store an 'id' in the state, and store data in an association
   table in the session storage. This allows avoiding "replaceState"
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
  | `Post | `Put | `Delete as http_method ->
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
      (match http_method with
      | `Post -> `Post (uri, post_params)
      | `Put -> `Put (uri, post_params)
      | `Delete -> `Delete (uri, post_params))

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
          uri post_params Eliom_request.string_result
      | `Put (uri, post_params) ->
        Eliom_request.http_put
          ?cookies_info:(Eliom_uri.make_cookies_info (https, service))
          uri post_params Eliom_request.string_result
      | `Delete (uri, post_params) ->
        Eliom_request.http_delete
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
     | `Post (uri, post_params) -> Eliom_request.redirect_post uri post_params
     | `Put (uri, post_params) -> Eliom_request.redirect_put uri post_params
     | `Delete (uri, post_params) -> Eliom_request.redirect_delete uri post_params)

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
    | `Put (uri, post_params) -> assert false
    | `Delete (uri, post_params) -> assert false

(* == Call caml service.

   Unwrap the data and execute the associated onload event
   handlers.
*)

let unwrap_caml_content content =
  let r : 'a Eliom_types.eliom_caml_service_data =
    Eliom_unwrap.unwrap (Url.decode content) 0
  in
  Lwt.return (r.Eliom_types.ecs_data, r.Eliom_types.ecs_request_data)

let call_ocaml_service
    ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
    ?keep_nl_params ?nl_params ?keep_get_na_params
    get_params post_params =
  trace "Call OCaml service";
  lwt _, content =
    raw_call_service
      ?absolute ?absolute_path ?https ~service ?hostname ?port ?fragment
      ?keep_nl_params ?nl_params ?keep_get_na_params
      get_params post_params in
  lwt content, request_data = unwrap_caml_content content in
  do_request_data request_data;
  run_callbacks (flush_onload ());
  reset_request_nodes ();
  match content with
    | `Success result ->
      Lwt.return result
    | `Failure msg ->
      Lwt.fail (Exception_on_server msg)

(* == Function [change_url_string] changes the URL, without doing a request.

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



(* == Function [change_url] changes the URL, without doing a request.
   It takes a GET (co-)service as parameter and its parameters.
 *)

let change_url
    ?absolute
    ?absolute_path
    ?https
    ~service
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params params =
  change_url_string
    (Eliom_uri.make_string_uri
       ?absolute
       ?absolute_path
       ?https
       ~service
       ?hostname
       ?port
       ?fragment
       ?keep_nl_params
       ?nl_params params)

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
       trace "Relink process node: did not find %a" (fun () -> Js.to_string) id;
       register_process_node id (node:>Dom.node Js.t))
    (fun pnode ->
       trace "Relink process node: found %a" (fun () -> Js.to_string) id;
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
       trace "Relink request node: did not find %a" (fun () -> Js.to_string) id;
       register_request_node id (node:>Dom.node Js.t))
    (fun pnode ->
       trace "Relink request node: found %a" (fun () -> Js.to_string) id;
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
let relink_page_but_client_values (root:Dom_html.element Js.t) =
  trace "Relink page";
  let (a_nodeList,form_nodeList,process_nodeList,closure_nodeList,attrib_nodeList) =
    Eliommod_dom.select_nodes root in
  Eliommod_dom.iter_nodeList a_nodeList
    (fun node -> node##onclick <- a_handler);
  Eliommod_dom.iter_nodeList form_nodeList
    (fun node -> node##onsubmit <- form_handler);
  Eliommod_dom.iter_nodeList process_nodeList
    relink_process_node;
  closure_nodeList,attrib_nodeList

(* == Rebuild event handlers

   Event handlers inside the DOM tree are rebuilt from the closure map
   sent with the request. The actual functions will be taken from the
   client values.

   It returns a single handler ([unit -> unit]) which captures all
   onload event handlers found in the tree, and cancels the execution
   when on raises [False] (cf. [raw_event_handler]).
*)

let is_closure_attrib,get_closure_name,get_closure_id =
  let v_prefix = Eliom_lib_base.RawXML.closure_attr_prefix in
  let v_len = String.length v_prefix in
  let v_prefix_js = Js.string v_prefix in

  let n_prefix = Eliom_lib_base.RawXML.closure_name_prefix in
  let n_len = String.length n_prefix in
  let n_prefix_js = Js.string n_prefix in

  (fun attr ->
    attr##value##substring(0,v_len) = v_prefix_js &&
    attr##name##substring(0,n_len) = n_prefix_js),
  (fun attr -> attr##name##substring_toEnd(n_len)),
  (fun attr -> attr##value##substring_toEnd(v_len))

let relink_closure_node root onload table (node:Dom_html.element Js.t) =
  trace "Relink closure node";
  let aux attr =
    if is_closure_attrib attr
    then
      let cid = Js.to_bytestring (get_closure_id attr) in
      let name = get_closure_name attr in
      try
        let cv = Eliom_lib.RawXML.ClosureMap.find cid table in
        let closure = raw_event_handler cv in
        if name = Js.string "onload" then
          (if Eliommod_dom.ancessor root node
          (* if not inside a unique node replaced by an older one *)
           then onload := closure :: !onload)
        else Js.Unsafe.set node name (Dom_html.handler (fun ev -> Js.bool (closure ev)))
      with Not_found ->
        error "relink_closure_node: client value %s not found" cid
  in
  Eliommod_dom.iter_attrList (node##attributes) aux

let relink_closure_nodes (root : Dom_html.element Js.t) event_handlers closure_nodeList =
  trace "Relink %i closure nodes" (closure_nodeList##length);
  let onload = ref [] in
  Eliommod_dom.iter_nodeList closure_nodeList
    (fun node -> relink_closure_node root onload event_handlers node);
  fun () ->
    let ev = Eliommod_dom.createEvent (Js.string "load") in
    ignore
      (List.for_all (fun f -> f ev) (List.rev !onload))

let is_attrib_attrib,get_attrib_id =
  let v_prefix = Eliom_lib_base.RawXML.client_attr_prefix in
  let v_len = String.length v_prefix in
  let v_prefix_js = Js.string v_prefix in

  let n_prefix = Eliom_lib_base.RawXML.client_name_prefix in
  let n_len = String.length n_prefix in
  let n_prefix_js = Js.string n_prefix in

  (fun attr ->
    attr##value##substring(0,v_len) = v_prefix_js &&
    attr##name##substring(0,n_len) = n_prefix_js),
  (fun attr -> attr##value##substring_toEnd(v_len))

let relink_attrib root table (node:Dom_html.element Js.t) =
  trace "Relink attribute";
  let aux attr =
    if is_attrib_attrib attr
    then
      let cid = Js.to_bytestring (get_attrib_id attr) in
      try
        let cv = Eliom_lib.RawXML.ClosureMap.find cid table in
        let closure_id = Client_value_server_repr.closure_id cv in
        let instance_id = Client_value_server_repr.instance_id cv in
        begin
          try
            let value = Client_value.find ~closure_id ~instance_id in
            let rattrib = (Eliom_lib.from_poly value : Eliom_content_core.Xml.attrib) in
            rebuild_rattrib node rattrib
          with Not_found ->
            error "Client value %Ld/%Ld not found as event handler" closure_id instance_id
        end
      with Not_found ->
        error "relink_attrib: client value %s not found" cid
  in
  Eliommod_dom.iter_attrList (node##attributes) aux


let relink_attribs (root : Dom_html.element Js.t) attribs attrib_nodeList =
  trace "Relink %i attributes" (attrib_nodeList##length);
  Eliommod_dom.iter_nodeList attrib_nodeList
    (fun node -> relink_attrib root attribs node)

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
  Eliom_process.reset_request_template ();
  Eliom_process.reset_request_cookies ();
  if !Eliom_config.debug_timings then
    Firebug.console##timeEnd(Js.string "load_data_script")

(* == Scroll the current page such that the top of element with the id
   [fragment] is aligned with the window's top. If the optional
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

let with_progress_cursor : 'a Lwt.t -> 'a Lwt.t =
  fun t ->
    try_lwt
      Dom_html.document##body##style##cursor <- Js.string "progress";
      lwt res = t in
      Dom_html.document##body##style##cursor <- Js.string "auto";
      Lwt.return res
    with exn ->
      Dom_html.document##body##style##cursor <- Js.string "auto";
      Lwt.fail exn


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
       let closure_nodeList,attrib_nodeList = relink_page_but_client_values fake_page in
       Eliom_request_info.set_session_info js_data.Eliom_common.ejs_sess_info;
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
       do_request_data js_data.Eliom_common.ejs_request_data;
       (* Replace closure ids in document with event handlers (from client values) *)
       let () = relink_attribs
           Dom_html.document##documentElement
           js_data.Eliom_common.ejs_client_attrib_table attrib_nodeList in
       let onload_closure_nodes =
         relink_closure_nodes
           Dom_html.document##documentElement
           js_data.Eliom_common.ejs_event_handler_table closure_nodeList
       in
       (* The request node table must be empty when nodes received via
          call_ocaml_service are unwrapped. *)
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
    with_progress_cursor
      ( match xhr with
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
              | `Put (uri, p) ->
                Eliom_request.http_put
                  ~expecting_process_page:true ?cookies_info uri p
                  Eliom_request.xml_result
              | `Delete (uri, p) ->
                Eliom_request.http_delete
                  ~expecting_process_page:true ?cookies_info uri p
                  Eliom_request.xml_result
          in
          let uri, fragment = Url.split_fragment uri in
          set_content ~uri ?fragment content )

(* Function used in "onclick" event handler of <a>.  *)

let change_page_uri ?cookies_info ?tmpl ?(get_params = []) full_uri =
  trace "Change page uri";
  with_progress_cursor
    ( let uri, fragment = Url.split_fragment full_uri in
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
          Lwt.return () ) )

(* Functions used in "onsubmit" event handler of <form>.  *)

let change_page_get_form ?cookies_info ?tmpl form full_uri =
  with_progress_cursor
    ( let form = Js.Unsafe.coerce form in
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
          set_content ~uri ?fragment content )

let change_page_post_form ?cookies_info ?tmpl form full_uri =
  with_progress_cursor
    ( let form = Js.Unsafe.coerce form in
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
          set_content ~uri ?fragment content )

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
        ( with_progress_cursor
            ( let uri, fragment = Url.split_fragment full_uri in
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
                  Lwt.return () ) ) )
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
        Js.Opt.case ((Js.Unsafe.coerce event)##state : int Js.opt)
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

let delay f = Lwt.ignore_result ( Lwt.pause () >>= (fun () -> f (); Lwt.return_unit))

module ReactState : sig
  type t
  val get_node : t -> Dom.node Js.t
  val change_dom : t -> Dom.node Js.t -> bool
  val init_or_update : ?state:t -> Eliom_content_core.Xml.elt -> t
end = struct

  (*
     ISSUE
     =====
     There is a confict when many dom react are inside each other.

     let s_lvl1 = S.map (function
     | case1 -> ..
     | case2 -> let s_lvl2 = ... in R.node s_lvl2) ...
     in R.node s_lvl1

     both dom react will update the same dom element (call it `dom_elt`) and
     we have to prevent an (outdated) s_lvl2 signal
     to replace `dom_elt` (updated last by a s_lvl1 signal)

     SOLUTION
     ========
     - an array to track versions of updates - a dom react store its version at a specify position (computed from the depth).
     - a child can only update `dom_elt` if versions of its parents haven't changed.
     - every time a dom react update `dom_elt`, it increment its version.
  *)

  type t = {
    elt : Eliom_content_core.Xml.elt;  (* top element that will store the dom *)
    global_version : int Js.js_array Js.t; (* global versions array *)
    version_copy : int Js.js_array Js.t; (* versions when the signal started *)
    pos : int; (* equal the depth *)
  }

  let get_node t = match Xml.get_node t.elt with
    | Xml.DomNode d -> d
    | _ -> assert false

  let change_dom state dom =
    let pos = state.pos in
    let outdated = ref false in
    for i = 0 to pos - 1 do
      if Js.array_get state.version_copy i != Js.array_get state.global_version i (* a parent changed *)
      then outdated := true
    done;
    if not !outdated
    then
      begin
        if dom != get_node state
        then
          begin
            (* new version *)
            let nv = Js.Optdef.get (Js.array_get state.global_version pos) (fun _ -> 0) + 1 in
            Js.array_set state.global_version pos nv;
            (* Js.array_set state.version_copy pos nv; *)

            Js.Opt.case ((get_node state)##parentNode)
              (fun () -> (* no parent -> no replace needed *) ())
              (fun parent ->
                 Js.Opt.iter (Dom.CoerceTo.element parent) (fun parent ->
                     (* really update the dom *)
                     ignore ((Dom_html.element parent)##replaceChild(dom, get_node state))));
            Xml.set_dom_node state.elt dom;
          end;
        false
      end
    else
      begin
        (* a parent signal changed, this dom react is outdated, do not update the dom *)
        true
      end

  let clone_array a = a##slice_end(0)

  let init_or_update ?state elt = match state with
    | None -> (* top dom react, create a state *)
      let global_version = jsnew Js.array_empty () in
      let pos = 0 in
      ignore(Js.array_set global_version pos 0);
      let node = (Dom_html.document##createElement (Js.string "span")  :> Dom.node Js.t) in
      Xml.set_dom_node elt node;
      {pos;global_version;version_copy = clone_array global_version; elt}
    | Some p -> (* child dom react, compute a state from the previous one *)
      let pos = p.pos + 1 in
      ignore(Js.array_set p.global_version pos 0);
      {p with pos;version_copy = clone_array p.global_version}

end

type content_ns = [ `HTML5 | `SVG ]

let rec rebuild_node_with_state ns ?state elt =
  match Xml.get_node elt with
  | Xml.DomNode node ->
      (* assert (Xml.get_node_id node <> NoId); *)
      node
  | Xml.ReactNode signal ->
      let state = ReactState.init_or_update ?state elt in
      let clear = ref None in
      let update_signal = React.S.map (fun elt' ->
        let dom = rebuild_node_with_state ns ~state elt' in
        let need_cleaning = ReactState.change_dom state dom in
        if need_cleaning then
          match !clear with
            | None -> ()
            | Some s ->
              begin
                delay (fun () -> React.S.stop s (* clear/stop the signal we created *));
                clear := None
              end
      ) signal in
      clear := Some update_signal;
      ReactState.get_node state
  | Xml.TyXMLNode raw_elt ->
      match Xml.get_node_id elt with
      | Xml.NoId -> raw_rebuild_node ns raw_elt
      | Xml.RequestId _ ->
          (* Do not look in request_nodes hashtbl: such elements have
             been bind while unwrapping nodes. *)
          let node = raw_rebuild_node ns raw_elt in
          Xml.set_dom_node elt node;
          node
      | Xml.ProcessId id ->
        let id = (Js.string id) in
        Js.Optdef.case (find_process_node id)
          (fun () ->
            let node = raw_rebuild_node ns (Xml.content elt) in
            register_process_node id node;
            node)
          (fun n -> (n:> Dom.node Js.t))

and rebuild_node' ns e = rebuild_node_with_state ns e

and raw_rebuild_node ns = function
  | Xml.Empty
  | Xml.Comment _ ->
      (* FIXME *)
      (Dom_html.document##createTextNode (Js.string "") :> Dom.node Js.t)
  | Xml.EncodedPCDATA s
  | Xml.PCDATA s -> (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)
  | Xml.Entity s ->
      let entity = Dom_html.decode_html_entities (Js.string ("&" ^ s ^ ";")) in
      (Dom_html.document##createTextNode(entity) :> Dom.node Js.t)
  | Xml.Leaf (name,attribs) ->
    let node = Dom_html.document##createElement (Js.string name) in
    List.iter (rebuild_rattrib node) attribs;
    (node :> Dom.node Js.t)
  | Xml.Node (name,attribs,childrens) ->
    let ns = if name = "svg" then `SVG else ns in
    let node =
      match ns with
      | `HTML5 -> Dom_html.document##createElement (Js.string name)
      | `SVG ->
	let svg_ns = "http://www.w3.org/2000/svg" in
	Dom_html.document##createElementNS (Js.string svg_ns, Js.string name)
    in
    List.iter (rebuild_rattrib node) attribs;
    List.iter (fun c -> Dom.appendChild node (rebuild_node' ns c)) childrens;
    (node :> Dom.node Js.t)

let rebuild_node_ns ns context elt' =
  trace "Rebuild node %a (%s)"
    (fun () e -> Eliom_content_core.Xml.string_of_node_id (Xml.get_node_id e))
    elt' context;
  if is_before_initial_load () then
    error_any (rebuild_node' ns elt')
      "Cannot apply %s%s before the document is initially loaded"
      context
      Xml.(match get_node_id elt' with
           | NoId -> " "
           | RequestId id -> " on request node "^id
           | ProcessId id -> " on global node "^id);
  let node = Js.Unsafe.coerce (rebuild_node' ns elt') in
  flush_load_script ();
  node

let rebuild_node_svg context elt =
  let elt' = Eliom_content_core.Svg.F.toelt elt in
  rebuild_node_ns `SVG context elt'


(** The first argument describes the calling function (if any) in case
    of an error. *)
let rebuild_node context elt =
  let elt' = Eliom_content_core.Html5.F.toelt elt in
  rebuild_node_ns `HTML5 context elt'

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
      let context = "unwrapping (i.e. utilize it in whatsoever form)" in
      Xml.make_lazy ~id:tmp_elt.tmp_node_id
        (lazy
           (match tmp_elt.tmp_node_id with
              | Xml.ProcessId process_id as id ->
                  trace "Unwrap tyxml from ProcessId %s" process_id;
                  Js.Optdef.case (find_process_node (Js.bytestring process_id))
                    (fun () ->
                       trace "not found";
                       let xml_elt : Xml.elt = Xml.make ~id elt in
                       let xml_elt = Eliom_content_core.Xml.set_classes_of_elt xml_elt in
                       register_process_node (Js.bytestring process_id)
                         (rebuild_node_ns `HTML5 context xml_elt);
                       xml_elt)
                    (fun elt ->
                       trace "found";
                       Xml.make_dom ~id elt)
              | Xml.RequestId request_id as id ->
                  trace "Unwrap tyxml from RequestId %s" request_id;
                  Js.Optdef.case (find_request_node (Js.bytestring request_id))
                    (fun () ->
                       trace "not found";
                       let xml_elt : Xml.elt = Xml.make ~id elt in
                       register_request_node (Js.bytestring request_id)
                         (rebuild_node_ns `HTML5 context xml_elt);
                       xml_elt)
                    (fun elt -> trace "found"; Xml.make_dom ~id elt)
              | Xml.NoId as id ->
                  trace "Unwrap tyxml from NoId";
                  Xml.make ~id elt))
    in
    register_unwrapped_elt elt;
    elt

let unwrap_client_value ({closure_id; instance_id},_) =
  try
    Some (Client_value.find ~closure_id ~instance_id)
  with Not_found ->
    (* BB By returning [None] this value will be registered for late
       unwrapping, and late unwrapped in Client_value.initialize as
       soon as it is available. *)
    None

let unwrap_global_data =
  fun (global_data', _) ->
    global_data := global_data'

let _ =
  Eliom_unwrap.register_unwrapper'
    (Eliom_unwrap.id_of_int Eliom_lib_base.client_value_unwrap_id_int)
    unwrap_client_value;
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_lib_base.tyxml_unwrap_id_int)
    unwrap_tyxml;
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_lib_base.global_data_unwrap_id_int)
    unwrap_global_data;
  ()


let init () =
  let js_data =
    Eliom_request_info.get_request_data () in

  let onload ev =
    trace "onload (client main)";
    set_initial_load ();
    Lwt.async
      (fun () ->
         if !Eliom_config.debug_timings then
           Firebug.console##time(Js.string "onload");
         Eliommod_cookies.update_cookie_table (Some Url.Current.host)
           (Eliom_request_info.get_request_cookies ());
         Eliom_request_info.set_session_info js_data.Eliom_common.ejs_sess_info;
         (* Give the browser the chance to actually display the page NOW *)
         lwt () = Lwt_js.sleep 0.001 in
         (* Ordering matters. See [Eliom_client.set_content] for explanations *)
         relink_request_nodes (Dom_html.document##documentElement);
         let root = Dom_html.document##documentElement in
         let closure_nodeList,attrib_nodeList = relink_page_but_client_values root in
         do_request_data js_data.Eliom_common.ejs_request_data;
         ((* A similar check is necessary in Injection.initialize *)
           match Eliom_unwrap.remaining_values_for_late_unwrapping () with
           | [] -> ()
           | unwrap_ids ->
             alert "Values marked for unwrapping remain (for unwrapping id %s)."
               (String.concat ", " (List.map string_of_int unwrap_ids)));
         let () =
           relink_attribs root
             js_data.Eliom_common.ejs_client_attrib_table attrib_nodeList in

         let onload_closure_nodes =
           relink_closure_nodes root js_data.Eliom_common.ejs_event_handler_table
             closure_nodeList
         in
         reset_request_nodes ();
         run_callbacks
           (Eliommod_dom.add_formdata_hack_onclick_handler ::
            flush_onload () @
            [ onload_closure_nodes; broadcast_load_end ]);
         if !Eliom_config.debug_timings then
           Firebug.console##timeEnd(Js.string "onload");
         Lwt.return ());
    Js._false in

  trace "Set load/onload events";
  let onunload _ = leave_page (); Js._true in
  (* IE<9: Script438: Object doesn't support property or method
     addEventListener.
     Other browsers: Ask whether you really want to navigate away if
     onbeforeunload is assigned *)
  if Js.Unsafe.get Dom_html.window (Js.string "addEventListener") == Js.undefined then
    ( Dom_html.window##onload <- Dom_html.handler onload;
      Dom_html.window##onbeforeunload <- Dom_html.handler onunload )
  else
    ( ignore
        (Dom.addEventListener Dom_html.window (Dom.Event.make "load")
           (Dom.handler onload) Js._true);
      ignore
        (Dom.addEventListener Dom_html.window (Dom.Event.make "unload")
           (Dom_html.handler onunload) Js._false) )


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
