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

(* Logs *)
let section = Lwt_log.Section.make "eliom:client"
let log_section = section
let _ = Lwt_log.Section.set_level log_section Lwt_log.Info
(* *)



(* == Auxiliaries *)

let create_buffer () =
  let stack = ref [] in
  let elts = ref [] in
  let add x = elts := x :: !elts
  and get () = List.rev !elts in
  let push () = stack := !elts :: !stack; elts := [] in
  let flush () =
    let res = get () in
    begin match !stack with
      l :: r -> elts := l; stack := r
    | []     -> elts := []
    end;
    res
  in
  add, get, flush, push

(* == Callbacks for onload, onbeforeunload, and onunload *)

let run_callbacks handlers = List.iter (fun f -> f ()) handlers

let onload, _, flush_onload, push_onload :
  ((unit -> unit) -> unit) * (unit -> (unit -> unit) list) * (unit -> (unit -> unit) list) * (unit -> unit)
  = create_buffer ()

let onunload, _, flush_onunload, _ = create_buffer ()

let onbeforeunload, run_onbeforeunload, flush_onbeforeunload =
  let add, get, flush, _ = create_buffer () in
  let rec run lst =
    match lst with
    | [] ->
        None
    | f :: rem ->
        match f () with
        | None   -> run rem
        | Some s -> Some s

  in
  add, (fun () -> run (get ())), flush

let run_onunload_wrapper f g =
  match run_onbeforeunload () with
  | Some s when not (confirm "%s" s) ->
      g ()
  | _ ->
      ignore (flush_onbeforeunload ());
      run_callbacks (flush_onunload ());
      f ()

let lwt_onload () =
  let t, u = Lwt.wait () in
  onload (Lwt.wakeup u);
  t


(* == Closure *)

module Client_closure : sig
  val register : closure_id:string -> closure:(_ -> _) -> unit
  val find : closure_id:string -> (poly -> poly)
end = struct

  let client_closures = Jstable.create ()

  let register ~closure_id ~closure =
    Jstable.add client_closures (Js.string closure_id)
      (from_poly (to_poly closure))

  let find ~closure_id =
    Js.Optdef.get
      (Jstable.find client_closures (Js.string closure_id))
      (fun () -> raise Not_found)
end

module Client_value : sig
  val find : instance_id:int -> poly option
  val initialize : Eliom_runtime.client_value_datum -> unit
end = struct

  let table = jsnew Js.array_empty ()

  let find ~instance_id =
    if instance_id = 0 then (* local client value *) None else
    Js.Optdef.to_option (Js.array_get table instance_id)

  let initialize {Eliom_runtime.closure_id; args; value = server_value} =
    let closure =
      try
        Client_closure.find ~closure_id
      with Not_found ->
        let pos =
          match Eliom_runtime.Client_value_server_repr.loc server_value
          with
          | None -> ""
          | Some p -> Printf.sprintf "(%s)" (Eliom_lib.pos_to_string p) in
        Lwt_log.raise_error_f ~section
         "Client closure %s not found %s (is the module linked on the client?)"
          closure_id pos
    in
    let value = try closure args
      with e -> Js.debugger (); raise e
    in
    Eliom_unwrap.late_unwrap_value server_value value;
    (* Only register global client values *)
    let instance_id =
      Eliom_runtime.Client_value_server_repr.instance_id server_value in
    if instance_id <> 0 then Js.array_set table instance_id value
end

let middleClick ev =
  match Dom_html.taggedEvent ev with
  | Dom_html.MouseEvent ev ->
    Dom_html.buttonPressed ev = Dom_html.Middle_button
    || Js.to_bool ev##ctrlKey
    || Js.to_bool ev##shiftKey
    || Js.to_bool ev##altKey
    || Js.to_bool ev##metaKey
  | _ -> false

module Injection : sig
  val get : ?ident:string -> ?pos:pos -> name:string -> _
  val initialize :
    compilation_unit_id:string -> Eliom_client_value.injection_datum -> unit
end = struct

  let table = Jstable.create ()

  let get ?ident ?pos ~name =
    Lwt_log.ign_debug_f ~section "Get injection %s" name;
    from_poly
      (Js.Optdef.get
         (Jstable.find table (Js.string name))
         (fun () ->
            let name = match ident,pos with
              | None,None -> Printf.sprintf "%s" name
              | None,Some pos ->
                Printf.sprintf "%s at %s" name (Eliom_lib.pos_to_string pos)
              | Some i,None -> Printf.sprintf "%s (%s)" name i
              | Some i,Some pos ->
                Printf.sprintf "%s (%s at %s)" name i
                  (Eliom_lib.pos_to_string pos)
            in
            Lwt_log.raise_error_f "Did not find injection %s" name))

  let initialize ~compilation_unit_id
        { Eliom_runtime.injection_id; injection_value } =
    Lwt_log.ign_debug_f ~section "Initialize injection %d" injection_id;
    (* BBB One should assert that injection_value doesn't contain any
       value marked for late unwrapping. How to do this efficiently? *)
    Jstable.add table
      (Js.string (compilation_unit_id ^ string_of_int injection_id))
      injection_value

end

(* == Populating client values and injections by global data *)

type compilation_unit_global_data =
  { mutable server_section : Eliom_runtime.client_value_datum array list;
    mutable client_section : Eliom_runtime.injection_datum array list }

let global_data = ref String_map.empty

let do_next_server_section_data ~compilation_unit_id =
  Lwt_log.ign_debug_f ~section
    "Do next client value data section in compilation unit %s"
    compilation_unit_id;
  try
    let data = String_map.find compilation_unit_id !global_data in
    match data.server_section with
      l :: r ->
        data.server_section <- r;
        Array.iter Client_value.initialize l
    | [] ->
        Lwt_log.raise_error_f ~section
          "Queue of client value data for compilation unit %s is empty \
           (is it linked on the server?)"
          compilation_unit_id
  with Not_found -> () (* Client-only compilation unit *)

let do_next_client_section_data ~compilation_unit_id =
  Lwt_log.ign_debug_f ~section
    "Do next injection data section in compilation unit %s"
    compilation_unit_id;
  try
    let data = String_map.find compilation_unit_id !global_data in
    match data.client_section with
      l :: r ->
        data.client_section <- r;
        Array.iter (fun i -> Injection.initialize ~compilation_unit_id i) l
    | [] ->
        Lwt_log.raise_error_f ~section
          "Queue of injection data for compilation unit %s is empty \
               (is it linked on the server?)"
          compilation_unit_id
  with Not_found -> () (* Client-only compilation unit *)

let check_global_data global_data =
  let missing_client_values = ref [] in
  let missing_injections = ref [] in
  String_map.iter
    (fun _ { server_section; client_section } ->
       List.iter
         (fun data ->
            missing_client_values :=
              List.rev_append (Array.to_list data) !missing_client_values)
         server_section;
       List.iter
         (fun data ->
            missing_injections :=
              List.rev_append (Array.to_list data) !missing_injections)
         client_section;
    )
    global_data;
  (match !missing_client_values with
   | [] -> ()
   | l ->
     Printf.ksprintf (fun s -> Firebug.console##error(Js.string s))
       "Code generating the following client values is not linked on the client:\n%s"
       (String.concat "\n"
          (List.rev_map
             (fun {Eliom_runtime.closure_id; value} ->
                let instance_id =
                  Eliom_runtime.Client_value_server_repr.instance_id value
                in
                match Eliom_runtime.Client_value_server_repr.loc value
                with
                | None -> Printf.sprintf "%s/%d" closure_id instance_id
                | Some pos ->
                  Printf.sprintf "%s/%d at %s" closure_id instance_id
                    (Eliom_lib.pos_to_string pos)
             )
             l
          )));
  (match !missing_injections with
   | [] -> ()
   | l ->
     Printf.ksprintf (fun s -> Firebug.console##error(Js.string s))
       "Code containing the following injections is not linked on the client:\n%s"
       (String.concat "\n"
          (List.rev_map (fun d ->
             let id = d.Eliom_runtime.injection_id in
             match d.Eliom_runtime.injection_dbg with
             | None -> Printf.sprintf "%d" id
             | Some (pos, Some i) ->
               Printf.sprintf "%d (%s at %s)" id i (Eliom_lib.pos_to_string pos)
             | Some (pos, None) ->
               Printf.sprintf "%d (at %s)" id (Eliom_lib.pos_to_string pos)
           ) l)))

(* == Initialize the client values sent with a request *)

let do_request_data request_data =
  Lwt_log.ign_debug_f ~section "Do request data (%a)"
    (fun () l -> string_of_int (Array.length l)) request_data;
  (* On a request, i.e. after running the toplevel definitions, global_data
     must contain at most empty sections_data lists, which stem from server-
     only eliom files. *)
  check_global_data !global_data;
  global_data := String_map.empty;
  Array.iter Client_value.initialize request_data

(*******************************************************************************)

let register_unwrapped_elt, force_unwrapped_elts =
  let suspended_nodes = ref [] in
  (fun elt ->
     suspended_nodes := elt :: !suspended_nodes),
  (fun () ->
     Lwt_log.ign_debug ~section "Force unwrapped elements";
     List.iter Xml.force_lazy !suspended_nodes;
     suspended_nodes := [])

(* == Process nodes
   (a.k.a. nodes with a unique Dom instance on each client process) *)

let (register_process_node, find_process_node) =
  let process_nodes : Dom.node Js.t Jstable.t = Jstable.create () in
  let find id =
    Lwt_log.ign_debug_f ~section "Find process node %a"
      (fun () -> Js.to_string) id;
    Jstable.find process_nodes id
  in
  let register id node =
    Lwt_log.ign_debug_f ~section "Register process node %a"
      (fun () -> Js.to_string) id;
    let node =
      if node##nodeName##toLowerCase() == Js.string "script" then
        (* We don't wan't to reexecute global scripts. *)
        (Dom_html.document##createTextNode (Js.string "") :> Dom.node Js.t)
      else
        node
    in
    Jstable.add process_nodes id node in
  (register, find)

let registered_process_node id = Js.Optdef.test (find_process_node id)

let getElementById id =
  Js.Optdef.case (find_process_node (Js.string id))
    (fun () -> Lwt_log.ign_warning_f ~section "getElementById %s: Not_found" id; raise Not_found)
    (fun pnode -> pnode)

(* == Request nodes
   (a.k.a. nodes with a unique Dom instance in the current request) *)

let register_request_node, find_request_node, reset_request_nodes =
  let request_nodes : Dom.node Js.t Jstable.t ref = ref (Jstable.create ()) in
  let find id = Jstable.find !request_nodes id in
  let register id node =
    Lwt_log.ign_debug_f ~section "Register request node %a"
      (fun () -> Js.to_string) id;
    Jstable.add !request_nodes id node in
  let reset () =
    Lwt_log.ign_debug ~section "Reset request nodes";
    (* Unwrapped elements must be forced
       before reseting the request node table. *)
    force_unwrapped_elts ();
    request_nodes := Jstable.create () in
  (register, find, reset)



(* == Organize the phase of loading or change_page

   In the following functions, onload referers the initial loading phase
   *and* to the change_page phase
   *and* to the loading phase after caml services (added 2016-03 --V). *)

let load_mutex = Lwt_mutex.create ()
let _ = ignore (Lwt_mutex.lock load_mutex)

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
let change_page_uri_
  : (?cookies_info:bool * string list -> ?tmpl:string -> string -> unit) ref
  = ref (fun ?cookies_info ?tmpl href -> assert false)
let change_page_get_form_
  : (?cookies_info:bool * string list ->
     ?tmpl:string -> Dom_html.formElement Js.t -> string -> unit)
      ref
  = ref (fun ?cookies_info ?tmpl form href -> assert false)
let change_page_post_form_ =
  ref (fun ?cookies_info ?tmpl form href -> assert false)

type client_form_handler = Dom_html.event Js.t -> bool Lwt.t

let raw_a_handler node cookies_info tmpl ev =
  let href = (Js.Unsafe.coerce node : Dom_html.anchorElement Js.t)##href in
  let https = Url.get_ssl (Js.to_string href) in
  (* Returns true when the default link behaviour is to be kept: *)
  (middleClick ev)
  || (not !Eliom_common.is_client_app
      && (  (https = Some true && not Eliom_request_info.ssl_)
         || (https = Some false && Eliom_request_info.ssl_)))
  || (
    (* If a link is clicked, we do not want to continue propagation
       (for example if the link is in a wider clickable area)  *)
    Dom_html.stopPropagation ev;
    !change_page_uri_ ?cookies_info ?tmpl (Js.to_string href);
    false)

let raw_form_handler form kind cookies_info tmpl ev client_form_handler =
  let action = Js.to_string form##action in
  let https = Url.get_ssl action in
  let change_page_form = match kind with
    | `Form_get -> !change_page_get_form_
    | `Form_post -> !change_page_post_form_ in
  let f () =
    Lwt.async @@ fun () ->
    lwt b = client_form_handler ev in
    if not b then change_page_form ?cookies_info ?tmpl form action;
    Lwt.return ()
  in
  (not !Eliom_common.is_client_app
   && (  (https = Some true && not Eliom_request_info.ssl_)
      || (https = Some false && Eliom_request_info.ssl_)))
  || (f (); false)

let raw_event_handler value =
  let handler = (*XXX???*)
    (Eliom_lib.from_poly (Eliom_lib.to_poly value) : #Dom_html.event Js.t -> unit) in
  fun ev -> try handler ev; true with Eliom_client_value.False -> false

let closure_name_prefix = Eliom_runtime.RawXML.closure_name_prefix
let closure_name_prefix_len = String.length closure_name_prefix
let reify_caml_event name node ce =
  match ce with
  | Xml.CE_call_service None -> name, `Other (fun _ -> true)
  | Xml.CE_call_service (Some (`A, cookies_info, tmpl, _)) ->
    name, `Other (fun ev ->
      let node = Js.Opt.get (Dom_html.CoerceTo.a node)
          (fun () -> Lwt_log.raise_error ~section "not an anchor element")
      in
      raw_a_handler node cookies_info tmpl ev)
  | Xml.CE_call_service
      (Some ((`Form_get | `Form_post) as kind, cookies_info, tmpl,
             client_hdlr)) ->
    name, `Other (fun ev ->
      let form = Js.Opt.get (Dom_html.CoerceTo.form node)
          (fun () -> Lwt_log.raise_error ~section "not a form element") in
      raw_form_handler form kind cookies_info tmpl ev
        (Eliom_lib.from_poly client_hdlr : client_form_handler))
  | Xml.CE_client_closure f ->
    name,
    `Other (fun ev -> try f ev; true with Eliom_client_value.False -> false)
  | Xml.CE_client_closure_keyboard f ->
    name,
    `Keyboard
      (fun ev -> try f ev; true with Eliom_client_value.False -> false)
  | Xml.CE_client_closure_mouse f ->
    name,
    `Mouse
      (fun ev -> try f ev; true with Eliom_client_value.False -> false)
  | Xml.CE_registered_closure (_, cv) ->
    let name =
      let len = String.length name in
      if
        len > closure_name_prefix_len &&
        String.sub name 0 closure_name_prefix_len = closure_name_prefix
      then String.sub name closure_name_prefix_len
          (len - closure_name_prefix_len)
      else name in
    name, `Other (raw_event_handler cv)

let register_event_handler, flush_load_script =
  let add, _, flush, _ = create_buffer () in
  let register node (name, ev) =
    match reify_caml_event name node ev with
    | "onload", `Other f ->
      add f
    | "onload", `Keyboard _ ->
      failwith "keyboard event handler for onload"
    | "onload", `Mouse _ ->
      failwith "keyboard event handler for onload"
    | name, `Other f ->
      Js.Unsafe.set node (Js.bytestring name)
        (Dom_html.handler (fun ev -> Js.bool (f ev)))
    | name, `Keyboard f ->
      Js.Unsafe.set node (Js.bytestring name)
        (Dom_html.handler (fun ev -> Js.bool (f ev)))
    | name, `Mouse f ->
      Js.Unsafe.set node (Js.bytestring name)
        (Dom_html.handler (fun ev -> Js.bool (f ev)))
  in
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

let class_list_of_racontent = function
  | Xml.AStr s ->
    [s]
  | Xml.AStrL (space, l) ->
    l
  | _ ->
    failwith "attribute class is not a string"

let class_list_of_racontent_o = function
  | Some c ->
    class_list_of_racontent c
  | None ->
    []

let rebuild_class_list l1 l2 l3 =
  let f s =
    not (List.exists ((=) s) l2) &&
    not (List.exists ((=) s) l3)
  in
  l3 @ List.filter f l1

let rebuild_class_string l1 l2 l3 =
  rebuild_class_list l1 l2 l3 |> String.concat " " |> Js.string

(* html attributes and dom properties use different names
   **exemple**: maxlength vs maxLenght (case sensitive).
   - Before dom react, it was enought to set html attributes only as
   there were no update after creation.
   - Dom React may update attributes later.
   Html attrib changes are not taken into account if the corresponding
   Dom property is defined.
   **exemple**: udpating html attribute `value` has no effect
   if the dom property `value` has be set by the user.

   =WE NEED TO SET DOM PROPERTIES=
   -Tyxml only gives us html attribute names and we can set them safely.
   -The name for dom properties is maybe differant.
    We set it only if we find out that the property
    match_the_attribute_name / is_already_defined (get_prop).
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

let iter_prop_protected node name f =
  match get_prop node name with
  | Some n -> begin try f n with _ -> () end
  | None -> ()

let current_classes node =
  let name = Js.string "class" in
  Js.Opt.case (node##getAttribute(name))
    (fun () -> [])
    (fun s -> Js.to_string s |> Regexp.(split (regexp " ")))

let rebuild_reactive_class_rattrib node s =
  let name = Js.string "class" in
  let e = React.S.diff (fun v v' -> v', v) s
  and f (v, v') =
    let l1 = current_classes node
    and l2 = class_list_of_racontent_o v
    and l3 = class_list_of_racontent_o v' in
    let s = rebuild_class_string l1 l2 l3 in
    node##setAttribute (name, s);
    iter_prop node name (fun name -> Js.Unsafe.set node name s)
  in
  f (None, React.S.value s);
  React.E.map f e |> ignore

let rec rebuild_rattrib node ra = match Xml.racontent ra with
  | Xml.RA a when Xml.aname ra = "class" ->
    let l1 = current_classes node
    and l2 = class_list_of_racontent a in
    let name = Js.string "class"
    and s = rebuild_class_string l1 l2 l2 in
    node##setAttribute (name, s)
  | Xml.RA a ->
    let name = Js.string (Xml.aname ra) in
    let v = rebuild_attrib_val a in
    node##setAttribute (name,v);
  | Xml.RAReact s when Xml.aname ra = "class" ->
    rebuild_reactive_class_rattrib node s
  | Xml.RAReact s ->
    let name = Js.string (Xml.aname ra) in
    let _ = React.S.map (function
      | None ->
        node##removeAttribute (name);
        iter_prop_protected node name
          (fun name -> Js.Unsafe.set node name Js.null)
      | Some v ->
        let v = rebuild_attrib_val v in
        node##setAttribute (name,v);
        iter_prop_protected node name
          (fun name -> Js.Unsafe.set node name v)
    ) s in ()
  | Xml.RACamlEventHandler ev -> register_event_handler node (Xml.aname ra, ev)
  | Xml.RALazyStr s ->
    node##setAttribute(Js.string (Xml.aname ra), Js.string s)
  | Xml.RALazyStrL (Xml.Space, l) ->
    node##setAttribute(Js.string (Xml.aname ra),
                       Js.string (String.concat " " l))
  | Xml.RALazyStrL (Xml.Comma, l) ->
    node##setAttribute(Js.string (Xml.aname ra),
                       Js.string (String.concat "," l))
  | Xml.RAClient (_,_,value) ->
    rebuild_rattrib node
      (Eliom_lib.from_poly (Eliom_lib.to_poly value) : Xml.attrib)



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

let random_int =
  if Js.Optdef.test Js.Unsafe.global##crypto &&
     Js.Optdef.test Js.Unsafe.global##crypto##getRandomValues
  then
    fun () ->
      Typed_array.unsafe_get
        (Js.Unsafe.global##crypto##getRandomValues
           (jsnew Typed_array.int32Array (1)))
        0
  else
     fun () -> truncate (4294967296. *. Js.math##random())
let session_id = random_int ()
let next_state_id =
  let last = ref 0 in fun () -> incr last; (session_id, !last)
let current_state_id = ref (next_state_id ())

let state_key (session_id, i) =
  Js.string (Printf.sprintf "state_history_%x_%x" session_id i)

let get_state i : state =
  Js.Opt.case
    (Js.Optdef.case ( Dom_html.window##sessionStorage )
       (fun () ->
          (* We use this only when the history API is
             available. Sessionstorage seems to be available
             everywhere the history API exists. *)
          Lwt_log.raise_error_f ~section "sessionStorage not available")
       (fun s -> s##getItem(state_key i)))
    (fun () -> Lwt_log.raise_error_f ~section "State id not found %x/%x in sessionStorage" (fst i) (snd i))
    (fun s -> Json.unsafe_input s)
let set_state i (v:state) =
  Js.Optdef.case ( Dom_html.window##sessionStorage )
    (fun () -> () )
    (fun s -> s##setItem(state_key i, Json.output v))
let update_state () =
  set_state !current_state_id
    { template =
        (match Eliom_request_info.get_request_template () with
         | Some tmpl -> Js.bytestring tmpl
         | None -> Js.string  "");
      position = Eliommod_dom.getDocumentScroll () }

(* TODO: Registering a global "onunload" event handler breaks the
   'bfcache' mechanism of Firefox and Safari. We may try to use
   "pagehide" whenever this event exists. See:

   https://developer.mozilla.org/En/Using_Firefox_1.5_caching

   http://www.webkit.org/blog/516/webkit-page-cache-ii-the-unload-event/

   and the function [Eliommod_dom.test_pageshow_pagehide]. *)



(* == Relink

   Traverse the Dom representation of the page in order to register
   "unique" nodes (or substitute previously known global nodes) and to
   bind Eliom's event handlers.
*)

let register_event_handlers node attribs =
  List.iter
    (fun ev -> register_event_handler
        (Js.Unsafe.coerce node : Dom_html.element Js.t)
        ev)
    attribs

let get_element_cookies_info elt =
  Js.Opt.to_option
    (Js.Opt.map
       (elt##getAttribute(Js.string
                            Eliom_runtime.RawXML.ce_call_service_attrib))
       (fun s -> of_json (Js.to_string s)))

let get_element_template elt =
  Js.Opt.to_option
    (Js.Opt.map (elt##getAttribute(Js.string
                                     Eliom_runtime.RawXML.ce_template_attrib))
       (fun s -> Js.to_string s))

let a_handler =
  Dom_html.full_handler
    (fun node ev ->
       let node = Js.Opt.get (Dom_html.CoerceTo.a node)
           (fun () -> Lwt_log.raise_error_f ~section "not an anchor element")
       in
       (* We prevent default behaviour
          only if raw_a_handler has taken the change page itself *)
       (*VVV Better: use preventdefault rather than returning false *)
       Js.bool (raw_a_handler node (get_element_cookies_info node)
                  (get_element_template node) ev))

let form_handler
  : (Dom_html.element Js.t, Dom_html.event Js.t) Dom_html.event_listener
  =
  Dom_html.full_handler
    (fun node ev ->
       let form = Js.Opt.get (Dom_html.CoerceTo.form node)
           (fun () -> Lwt_log.raise_error_f ~section "not a form element")
       in
       let kind =
         if String.lowercase(Js.to_string form##_method) = "get"
         then `Form_get
         else `Form_post
       and f _ = Lwt.return false in
       Js.bool (raw_form_handler form kind (get_element_cookies_info form)
                  (get_element_template node) ev f))

let relink_process_node (node:Dom_html.element Js.t) =
  let id = Js.Opt.get
      (node##getAttribute(Js.string Eliom_runtime.RawXML.node_id_attrib))
      (fun () -> Lwt_log.raise_error_f ~section
          "unique node without id attribute")
  in
  Js.Optdef.case (find_process_node id)
    (fun () ->
       Lwt_log.ign_debug_f ~section
         "Relink process node: did not find %a. Will add it."
         (fun () -> Js.to_string) id;
       register_process_node id (node:>Dom.node Js.t))
    (fun pnode ->
       Lwt_log.ign_debug_f ~section "Relink process node: found %a"
         (fun () -> Js.to_string) id;
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
    (node##getAttribute(Js.string Eliom_runtime.RawXML.node_id_attrib))
    (fun () -> Lwt_log.raise_error_f ~section
        "unique node without id attribute")
  in
  Js.Optdef.case (find_request_node id)
    (fun () ->
       Lwt_log.ign_debug_f ~section
         "Relink request node: did not find %a. Will add it."
         (fun () -> Js.to_string) id;
       register_request_node id (node:>Dom.node Js.t))
    (fun pnode ->
       Lwt_log.ign_debug_f ~section "Relink request node: found %a"
         (fun () -> Js.to_string) id;
       Js.Opt.iter (node##parentNode)
         (fun parent -> Dom.replaceChild parent pnode node))

let relink_request_nodes root =
  Lwt_log.ign_debug ~section "Relink request nodes";
  if !Eliom_config.debug_timings
  then Firebug.console##time (Js.string "relink_request_nodes");
  Eliommod_dom.iter_nodeList
    (Eliommod_dom.select_request_nodes root)
    relink_request_node;
  if !Eliom_config.debug_timings
  then Firebug.console##timeEnd(Js.string "relink_request_nodes")

(* Relinks a-elements, form-elements, and process nodes. The list of
   closure nodes is returned for application on [relink_closure_node]
   after the client values are initialized.
*)
let relink_page_but_client_values (root:Dom_html.element Js.t) =
  Lwt_log.ign_debug ~section "Relink page";
  let (a_nodeList, form_nodeList, process_nodeList, closure_nodeList,
       attrib_nodeList) =
    Eliommod_dom.select_nodes root
  in
  Eliommod_dom.iter_nodeList a_nodeList
    (fun node -> node##onclick <- a_handler);
  Eliommod_dom.iter_nodeList form_nodeList
    (fun node -> node##onsubmit <- form_handler);
  Eliommod_dom.iter_nodeList process_nodeList relink_process_node;
  closure_nodeList, attrib_nodeList

(* == Rebuild event handlers

   Event handlers inside the DOM tree are rebuilt from the closure map
   sent with the request. The actual functions will be taken from the
   client values.

   It returns a single handler ([unit -> unit]) which captures all
   onload event handlers found in the tree, and cancels the execution
   when on raises [False] (cf. [raw_event_handler]).
*)

let is_closure_attrib, get_closure_name, get_closure_id =
  let v_prefix = Eliom_runtime.RawXML.closure_attr_prefix in
  let v_len = String.length v_prefix in
  let v_prefix_js = Js.string v_prefix in

  let n_prefix = Eliom_runtime.RawXML.closure_name_prefix in
  let n_len = String.length n_prefix in
  let n_prefix_js = Js.string n_prefix in

  (fun attr ->
     attr##value##substring(0,v_len) = v_prefix_js &&
     attr##name##substring(0,n_len) = n_prefix_js),
  (fun attr -> attr##name##substring_toEnd(n_len)),
  (fun attr -> attr##value##substring_toEnd(v_len))

let relink_closure_node root onload table (node:Dom_html.element Js.t) =
  Lwt_log.ign_debug ~section "Relink closure node";
  let aux attr =
    if is_closure_attrib attr
    then
      let cid = Js.to_bytestring (get_closure_id attr) in
      let name = get_closure_name attr in
      try
        let cv = Eliom_runtime.RawXML.ClosureMap.find cid table in
        let closure = raw_event_handler cv in
        if name = Js.string "onload" then
          (if Eliommod_dom.ancessor root node
          (* if not inside a unique node replaced by an older one *)
           then onload := closure :: !onload)
        else Js.Unsafe.set node name (Dom_html.handler (fun ev -> Js.bool (closure ev)))
      with Not_found ->
        Lwt_log.ign_error_f ~section "relink_closure_node: client value %s not found" cid
  in
  Eliommod_dom.iter_attrList (node##attributes) aux

let relink_closure_nodes (root : Dom_html.element Js.t)
    event_handlers closure_nodeList =
  Lwt_log.ign_debug_f ~section "Relink %i closure nodes"
    (closure_nodeList##length);
  let onload = ref [] in
  Eliommod_dom.iter_nodeList closure_nodeList
    (fun node -> relink_closure_node root onload event_handlers node);
  fun () ->
    let ev = Eliommod_dom.createEvent (Js.string "load") in
    ignore (List.for_all (fun f -> f ev) (List.rev !onload))

let is_attrib_attrib,get_attrib_id =
  let v_prefix = Eliom_runtime.RawXML.client_attr_prefix in
  let v_len = String.length v_prefix in
  let v_prefix_js = Js.string v_prefix in

  let n_prefix = Eliom_runtime.RawXML.client_name_prefix in
  let n_len = String.length n_prefix in
  let n_prefix_js = Js.string n_prefix in

  (fun attr ->
     attr##value##substring(0,v_len) = v_prefix_js &&
     attr##name##substring(0,n_len) = n_prefix_js),
  (fun attr -> attr##value##substring_toEnd(v_len))

let relink_attrib root table (node:Dom_html.element Js.t) =
  Lwt_log.ign_debug ~section "Relink attribute";
  let aux attr =
    if is_attrib_attrib attr
    then
      let cid = Js.to_bytestring (get_attrib_id attr) in
      try
        let value = Eliom_runtime.RawXML.ClosureMap.find cid table in
        let rattrib: Eliom_content_core.Xml.attrib =
          (Eliom_lib.from_poly (Eliom_lib.to_poly value)) in
        rebuild_rattrib node rattrib
      with Not_found ->
        Lwt_log.raise_error_f ~section
          "relink_attrib: client value %s not found" cid
  in
  Eliommod_dom.iter_attrList (node##attributes) aux


let relink_attribs (root : Dom_html.element Js.t) attribs attrib_nodeList =
  Lwt_log.ign_debug_f ~section "Relink %i attributes" (attrib_nodeList##length);
  Eliommod_dom.iter_nodeList attrib_nodeList
    (fun node -> relink_attrib root attribs node)

(* == Extract the request data and the request tab-cookies from a page

   See the corresponding function on the server side:
   Eliom_registration.Eliom_appl_reg_make_param.make_eliom_data_script.
*)

let load_data_script page =
  Lwt_log.ign_debug ~section "Load Eliom application data";
  let head = Eliommod_dom.get_head page in
  let data_script : Dom_html.scriptElement Js.t =
    match Dom.list_of_nodeList head##childNodes with
    | _ :: _ :: data_script :: _ ->
      let data_script : Dom.element Js.t = Js.Unsafe.coerce data_script in
      (match Js.to_bytestring (data_script##tagName##toLowerCase ()) with
       | "script" -> (Js.Unsafe.coerce data_script)
       | t ->
         Lwt_log.raise_error_f ~section
           "Unable to find Eliom application data (script element expected, found %s element)" t)
    | _ -> Lwt_log.raise_error_f ~section
             "Unable to find Eliom application data."
  in
  let script = data_script##text in
  if !Eliom_config.debug_timings
  then Firebug.console##time(Js.string "load_data_script");
  ignore (Js.Unsafe.eval_string (Js.to_string script));
  Eliom_process.reset_request_template ();
  Eliom_process.reset_request_cookies ();
  if !Eliom_config.debug_timings
  then Firebug.console##timeEnd(Js.string "load_data_script")

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

let delay f =
  Lwt.ignore_result ( Lwt.pause () >>= (fun () -> f (); Lwt.return_unit))

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
      if Js.array_get state.version_copy i
         != Js.array_get state.global_version i (* a parent changed *)
      then outdated := true
    done;
    if not !outdated
    then
      begin
        if dom != get_node state
        then
          begin
            (* new version *)
            let nv = Js.Optdef.get (Js.array_get state.global_version pos)
                (fun _ -> 0) + 1
            in
            Js.array_set state.global_version pos nv;
            (* Js.array_set state.version_copy pos nv; *)

            Js.Opt.case ((get_node state)##parentNode)
              (fun () -> (* no parent -> no replace needed *) ())
              (fun parent ->
                 Js.Opt.iter (Dom.CoerceTo.element parent) (fun parent ->
                   (* really update the dom *)
                   ignore ((Dom_html.element
                              parent)##replaceChild(dom, get_node state))));
            Xml.set_dom_node state.elt dom;
          end;
        false
      end
    else
      begin
        (* a parent signal changed,
           this dom react is outdated, do not update the dom *)
        true
      end

  let clone_array a = a##slice_end(0)

  let init_or_update ?state elt = match state with
    | None -> (* top dom react, create a state *)
      let global_version = jsnew Js.array_empty () in
      let pos = 0 in
      ignore(Js.array_set global_version pos 0);
      let node = (Dom_html.document##createElement (Js.string "span")
                  :> Dom.node Js.t)
      in
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
  | Xml.ReactChildren (node,elts) ->
    let dom = raw_rebuild_node ns node in
    Tyxml_js.Util.update_children
      dom
      (ReactiveData.RList.map (rebuild_node' ns) elts);
    Xml.set_dom_node elt dom;
    dom
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
            delay (fun () -> React.S.stop s
            (* clear/stop the signal we created *));
            clear := None
          end)
      signal
    in
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
  | Xml.PCDATA s ->
    (Dom_html.document##createTextNode (Js.string s) :> Dom.node Js.t)
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

(* [is_before_initial_load] tests whether it is executed before the
   loading of the initial document, e.g. during the initialization of the
   (OCaml) module, i.e. before [Eliom_client_main.onload]. *)
let is_before_initial_load, set_initial_load =
  let before_load = ref true in
  (fun () -> !before_load),
  (fun () -> before_load := false)

let rebuild_node_ns ns context elt' =
  Lwt_log.ign_debug_f ~section "Rebuild node %a (%s)"
    (fun () e -> Eliom_content_core.Xml.string_of_node_id (Xml.get_node_id e))
    elt' context;
  if is_before_initial_load ()
  then begin
      Lwt_log.raise_error_f ~section ~inspect:(rebuild_node' ns elt')
      "Cannot apply %s%s before the document is initially loaded"
      context
      Xml.(match get_node_id elt' with
        | NoId -> " "
        | RequestId id -> " on request node "^id
        | ProcessId id -> " on global node "^id)
    end;
  let node = Js.Unsafe.coerce (rebuild_node' ns elt') in
  flush_load_script ();
  node

let rebuild_node_svg context elt =
  let elt' = Eliom_content_core.Svg.F.toelt elt in
  rebuild_node_ns `SVG context elt'


(** The first argument describes the calling function (if any) in case
    of an error. *)
let rebuild_node context elt =
  let elt' = Eliom_content_core.Html.F.toelt elt in
  rebuild_node_ns `HTML5 context elt'

(******************************************************************************)
(*                            Register unwrappers                             *)

(* == Html elements

   Html elements are unwrapped lazily (cf. use of Xml.make_lazy in
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
    Lwt_log.ign_debug ~section "Unwrap tyxml";
    (* Do not rebuild dom node while unwrapping, otherwise we
       don't have control on when "onload" event handlers are
       triggered. *)
    let elt =
      let context = "unwrapping (i.e. utilize it in whatsoever form)" in
      Xml.make_lazy ~id:tmp_elt.tmp_node_id
        (lazy
          (match tmp_elt.tmp_node_id with
           | Xml.ProcessId process_id as id ->
             Lwt_log.ign_debug_f ~section "Unwrap tyxml from ProcessId %s"
               process_id;
             Js.Optdef.case (find_process_node (Js.bytestring process_id))
               (fun () ->
                  Lwt_log.ign_debug ~section "not found";
                  let xml_elt : Xml.elt = Xml.make ~id elt in
                  let xml_elt =
                    Eliom_content_core.Xml.set_classes_of_elt xml_elt
                  in
                  register_process_node (Js.bytestring process_id)
                    (rebuild_node_ns `HTML5 context xml_elt);
                  xml_elt)
               (fun elt ->
                  Lwt_log.ign_debug ~section "found";
                  Xml.make_dom ~id elt)
           | Xml.RequestId request_id as id ->
             Lwt_log.ign_debug_f ~section "Unwrap tyxml from RequestId %s"
               request_id;
             Js.Optdef.case (find_request_node (Js.bytestring request_id))
               (fun () ->
                  Lwt_log.ign_debug ~section "not found";
                  let xml_elt : Xml.elt = Xml.make ~id elt in
                  register_request_node (Js.bytestring request_id)
                    (rebuild_node_ns `HTML5 context xml_elt);
                  xml_elt)
               (fun elt -> Lwt_log.ign_debug ~section "found";
                 Xml.make_dom ~id elt)
           | Xml.NoId as id ->
             Lwt_log.ign_debug ~section "Unwrap tyxml from NoId";
             Xml.make ~id elt))
    in
    register_unwrapped_elt elt;
    elt

let unwrap_client_value cv =
  Client_value.find
    ~instance_id:(Eliom_runtime.Client_value_server_repr.instance_id cv)
  (* BB By returning [None] this value will be registered for late
     unwrapping, and late unwrapped in Client_value.initialize as
     soon as it is available. *)

let unwrap_global_data = fun (global_data', _) ->
  global_data :=
    String_map.map
      (fun {Eliom_runtime.server_sections_data; client_sections_data} ->
         {server_section = Array.to_list server_sections_data;
          client_section = Array.to_list client_sections_data})
      global_data'

let _ =
  Eliom_unwrap.register_unwrapper'
    (Eliom_unwrap.id_of_int Eliom_common_base.client_value_unwrap_id_int)
    unwrap_client_value;
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_runtime.tyxml_unwrap_id_int)
    unwrap_tyxml;
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_common_base.global_data_unwrap_id_int)
    unwrap_global_data;
  ()

let add_string_event_listener o e f capt : unit =
  let e = Js.string e
  and capt = Js.bool capt
  and f e =
    match f e with
    | Some s ->
      let s = Js.string s in
      (Js.Unsafe.coerce e)##returnValue <- s;
      Js.def s
    | None ->
      Js.undefined
  in
  let f = Js.Unsafe.callback f in
  ignore @@
  if (Js.Unsafe.coerce o)##addEventListener == Js.undefined then
    let e = (Js.string "on")##concat(e)
    and cb e = Js.Unsafe.call (f, e, [||]) in
    (Js.Unsafe.coerce o)##attachEvent(e, cb)
  else
    (Js.Unsafe.coerce o)##addEventListener(e, f, capt)


(******************************************************************************)

module Syntax_helpers = struct

  let register_client_closure closure_id closure =
    Client_closure.register ~closure_id ~closure

  let open_client_section compilation_unit_id =
    do_next_client_section_data ~compilation_unit_id

  let close_server_section compilation_unit_id =
    do_next_server_section_data ~compilation_unit_id

  let get_escaped_value = from_poly

  let get_injection ?ident ?pos name = Injection.get ?ident ?pos ~name

end
