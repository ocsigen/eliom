open Eio.Std

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
open Eliom_lib
module Xml = Eliom_content_core.Xml

(* Logs *)
let section = Logs.Src.create "eliom:client"

(* == Auxiliaries *)

let create_buffer () =
  let stack = ref [] in
  let elts = ref [] in
  let add x = elts := x :: !elts and get () = List.rev !elts in
  let push () =
    stack := !elts :: !stack;
    elts := []
  in
  let flush () =
    let res = get () in
    (match !stack with
    | l :: r ->
        elts := l;
        stack := r
    | [] -> elts := []);
    res
  in
  add, get, flush, push

(* == Closure *)

module Client_closure : sig
  val register : closure_id:string -> closure:(_ -> _) -> unit
  val find : closure_id:string -> poly -> poly
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
  let table = new%js Js.array_empty

  let find ~instance_id =
    if instance_id = 0
    then (* local client value *) None
    else Js.Optdef.to_option (Js.array_get table instance_id)

  let initialize {Eliom_runtime.closure_id; args; value = server_value} =
    let closure =
      try Client_closure.find ~closure_id
      with Not_found ->
        let pos =
          match Eliom_runtime.Client_value_server_repr.loc server_value with
          | None -> ""
          | Some p -> Printf.sprintf "(%s)" (Eliom_lib.pos_to_string p)
        in
        raise_error ~section
          "Client closure %s not found %s (is the module linked on the client?)"
          closure_id pos
    in
    let value = closure args in
    Eliom_unwrap.late_unwrap_value server_value value;
    (* Only register global client values *)
    let instance_id =
      Eliom_runtime.Client_value_server_repr.instance_id server_value
    in
    if instance_id <> 0 then Js.array_set table instance_id value
end

let middleClick ev =
  match Dom_html.taggedEvent ev with
  | Dom_html.MouseEvent ev ->
      Dom_html.buttonPressed ev = Dom_html.Middle_button
      || Js.to_bool ev##.ctrlKey
      || Js.to_bool ev##.shiftKey
      || Js.to_bool ev##.altKey
      || Js.to_bool ev##.metaKey
  | _ -> false

module Injection : sig
  val get : ?ident:string -> ?pos:pos -> name:string -> _

  val initialize :
     compilation_unit_id:string
    -> Eliom_client_value.injection_datum
    -> unit
end = struct
  let table = Jstable.create ()

  let get ?ident ?pos ~name =
    Logs.debug ~src:section (fun fmt -> fmt "Get injection %s" name);
    from_poly
      (Js.Optdef.get
         (Jstable.find table (Js.string name))
         (fun () ->
            let name =
              match ident, pos with
              | None, None -> Printf.sprintf "%s" name
              | None, Some pos ->
                  Printf.sprintf "%s at %s" name (Eliom_lib.pos_to_string pos)
              | Some i, None -> Printf.sprintf "%s (%s)" name i
              | Some i, Some pos ->
                  Printf.sprintf "%s (%s at %s)" name i
                    (Eliom_lib.pos_to_string pos)
            in
            raise_error "Did not find injection %s" name))

  let initialize
        ~compilation_unit_id
        {Eliom_runtime.injection_id; injection_value; _}
    =
    Logs.debug ~src:section (fun fmt ->
      fmt "Initialize injection %d" injection_id);
    (* BBB One should assert that injection_value doesn't contain any
       value marked for late unwrapping. How to do this efficiently? *)
    Jstable.add table
      (Js.string (compilation_unit_id ^ string_of_int injection_id))
      injection_value
end

(* == Populating client values and injections by global data *)

type compilation_unit_global_data =
  { mutable server_section : Eliom_runtime.client_value_datum array list
  ; mutable client_section : Eliom_runtime.injection_datum array list }

let global_data = ref String_map.empty

let do_next_server_section_data ~compilation_unit_id =
  Logs.debug ~src:section (fun fmt ->
    fmt "Do next client value data section in compilation unit %s"
      compilation_unit_id);
  try
    let data = String_map.find compilation_unit_id !global_data in
    match data.server_section with
    | l :: r ->
        data.server_section <- r;
        Array.iter Client_value.initialize l
    | [] ->
        raise_error ~section
          "Queue of client value data for compilation unit %s is empty (is it linked on the server?)"
          compilation_unit_id
  with Not_found -> ()
(* Client-only compilation unit *)

let do_next_client_section_data ~compilation_unit_id =
  Logs.debug ~src:section (fun fmt ->
    fmt "Do next injection data section in compilation unit %s"
      compilation_unit_id);
  try
    let data = String_map.find compilation_unit_id !global_data in
    match data.client_section with
    | l :: r ->
        data.client_section <- r;
        Array.iter (fun i -> Injection.initialize ~compilation_unit_id i) l
    | [] ->
        raise_error ~section
          "Queue of injection data for compilation unit %s is empty (is it linked on the server?)"
          compilation_unit_id
  with Not_found -> ()
(* Client-only compilation unit *)

(*******************************************************************************)

let register_unwrapped_elt, force_unwrapped_elts =
  let suspended_nodes = ref [] in
  ( (fun elt -> suspended_nodes := elt :: !suspended_nodes)
  , fun () ->
      Logs.debug ~src:section (fun fmt -> fmt "Force unwrapped elements");
      List.iter Xml.force_lazy !suspended_nodes;
      suspended_nodes := [] )

(* == Process nodes
   (a.k.a. nodes with a unique Dom instance on each client process) *)

let register_process_node, find_process_node =
  let process_nodes : Dom.node Js.t Jstable.t = Jstable.create () in
  let find id =
    Logs.debug ~src:section (fun fmt ->
      fmt "Find process node %s" (Js.to_string id));
    Jstable.find process_nodes id
  in
  let register id node =
    Logs.debug ~src:section (fun fmt ->
      fmt "Register process node %s" (Js.to_string id));
    let node =
      if node##.nodeName##toLowerCase == Js.string "script"
      then
        (* We don't want to reexecute global scripts. *)
        (Dom_html.document##(createTextNode (Js.string "")) :> Dom.node Js.t)
      else node
    in
    Jstable.add process_nodes id node
  in
  register, find

let registered_process_node id = Js.Optdef.test (find_process_node id)

let getElementById id =
  Js.Optdef.case
    (find_process_node (Js.string id))
    (fun () ->
       Logs.warn ~src:section (fun fmt -> fmt "getElementById %s: Not_found" id);
       raise Not_found)
    (fun pnode -> pnode)

(* == Request nodes
   (a.k.a. nodes with a unique Dom instance in the current request) *)

let register_request_node, find_request_node, reset_request_nodes =
  let request_nodes : Dom.node Js.t Jstable.t ref = ref (Jstable.create ()) in
  let find id = Jstable.find !request_nodes id in
  let register id node =
    Logs.debug ~src:section (fun fmt ->
      fmt "Register request node %s" (Js.to_string id));
    Jstable.add !request_nodes id node
  in
  let reset () =
    Logs.debug ~src:section (fun fmt -> fmt "Reset request nodes");
    (* Unwrapped elements must be forced
       before resetting the request node table. *)
    force_unwrapped_elts ();
    request_nodes := Jstable.create ()
  in
  register, find, reset

(* == Organize the phase of loading or change_page

   In the following functions, onload referrers the initial loading phase
   *and* to the change_page phase
   *and* to the loading phase after caml services (added 2016-03 --V). *)

(* For synchronization during page loading.
   We use a simple boolean flag instead of Eio.Mutex because:
   1. Eio.Mutex operations require being inside an Eio fiber
   2. The original Lwt code used a mutex that was locked at module init time
   3. We only need to track "loading in progress" state, not actual mutual exclusion *)
let load_mutex_locked = ref true  (* Start locked, like the Lwt version *)

let loading_phase = ref true
let load_end = lazy (Eio.Condition.create ())

let in_onload () = !loading_phase

let set_loading_phase () = loading_phase := true

let broadcast_load_end () =
  loading_phase := false;
  if Lazy.is_val load_end then Eio.Condition.broadcast (Lazy.force load_end)

let wait_load_end () =
  if !loading_phase then Eio.Condition.await_no_mutex (Lazy.force load_end) else ()

(* Expose the lock state for eliom_client.client.ml *)
let lock_load_mutex () = load_mutex_locked := true
let unlock_load_mutex () = load_mutex_locked := false
let is_load_mutex_locked () = !load_mutex_locked

(* == Helper's functions for Eliom's event handler.

   Allow conversion of Xml.event_handler to javascript closure and
   their registration in Dom node.
*)

(* forward declaration... *)
let change_page_uri_ :
  (?cookies_info:bool * string list -> ?tmpl:string -> string -> unit) ref
  =
  ref (fun ?cookies_info:_ ?tmpl:_ _href -> assert false)

let change_page_get_form_ :
  (?cookies_info:bool * string list
   -> ?tmpl:string
   -> Dom_html.formElement Js.t
   -> string
   -> unit)
    ref
  =
  ref (fun ?cookies_info:_ ?tmpl:_ _form _href -> assert false)

let change_page_post_form_ =
  ref (fun ?cookies_info:_ ?tmpl:_ _form _href -> assert false)

type client_form_handler = Dom_html.event Js.t -> bool

let raw_a_handler node cookies_info tmpl ev =
  let href = (Js.Unsafe.coerce node : Dom_html.anchorElement Js.t)##.href in
  let https = Url.get_ssl (Js.to_string href) in
  (* Returns true when the default link behaviour is to be kept: *)
  middleClick ev
  || (not !Eliom_common.is_client_app)
     && ((https = Some true && not Eliom_request_info.ssl_)
        || (https = Some false && Eliom_request_info.ssl_))
  ||
  ((* If a link is clicked, we do not want to continue propagation
       (for example if the link is in a wider clickable area)  *)
   Dom_html.stopPropagation ev;
   !change_page_uri_ ?cookies_info ?tmpl (Js.to_string href);
   false)

let raw_form_handler form kind cookies_info tmpl ev client_form_handler =
  let action = Js.to_string form##.action in
  let https = Url.get_ssl action in
  let change_page_form =
    match kind with
    | `Form_get -> !change_page_get_form_
    | `Form_post -> !change_page_post_form_
  in
  let f () =
    Js_of_ocaml_eio.Eio_js.start (fun () ->
      let b = client_form_handler ev in
      if not b then change_page_form ?cookies_info ?tmpl form action)
  in
  (not !Eliom_common.is_client_app)
  && ((https = Some true && not Eliom_request_info.ssl_)
     || (https = Some false && Eliom_request_info.ssl_))
  || (f (); false)

let raw_event_handler value =
  let handler =
    (*XXX???*)
    (Eliom_lib.from_poly (Eliom_lib.to_poly value)
     : #Dom_html.event Js.t -> unit)
  in
  fun ev -> try handler ev; true with Eliom_client_value.False -> false

let closure_name_prefix = Eliom_runtime.RawXML.closure_name_prefix
let closure_name_prefix_len = String.length closure_name_prefix

let reify_caml_event name node ce =
  match ce with
  | Xml.CE_call_service None -> name, `Other (fun _ -> true)
  | Xml.CE_call_service (Some (`A, cookies_info, tmpl, _)) ->
      ( name
      , `Other
          (fun ev ->
            let node =
              Js.Opt.get (Dom_html.CoerceTo.a node) (fun () ->
                raise_error ~section "not an anchor element")
            in
            raw_a_handler node cookies_info tmpl ev) )
  | Xml.CE_call_service
      (Some (((`Form_get | `Form_post) as kind), cookies_info, tmpl, client_hdlr))
    ->
      ( name
      , `Other
          (fun ev ->
            let form =
              Js.Opt.get (Dom_html.CoerceTo.form node) (fun () ->
                raise_error ~section "not a form element")
            in
            raw_form_handler form kind cookies_info tmpl ev
              (Eliom_lib.from_poly client_hdlr : client_form_handler)) )
  | Xml.CE_client_closure f ->
      ( name
      , `Other
          (fun ev -> try f ev; true with Eliom_client_value.False -> false) )
  | Xml.CE_client_closure_keyboard f ->
      ( name
      , `Keyboard
          (fun ev -> try f ev; true with Eliom_client_value.False -> false) )
  | Xml.CE_client_closure_touch f ->
      ( name
      , `Touch
          (fun ev -> try f ev; true with Eliom_client_value.False -> false) )
  | Xml.CE_client_closure_mouse f ->
      ( name
      , `Mouse
          (fun ev -> try f ev; true with Eliom_client_value.False -> false) )
  | Xml.CE_registered_closure (_, cv) ->
      let name =
        let len = String.length name in
        if
          len > closure_name_prefix_len
          && String.sub name 0 closure_name_prefix_len = closure_name_prefix
        then
          String.sub name closure_name_prefix_len (len - closure_name_prefix_len)
        else name
      in
      name, `Other (raw_event_handler cv)

let register_event_handler, flush_load_script =
  let add, _, flush, _ = create_buffer () in
  let register node (name, ev) =
    match reify_caml_event name node ev with
    | "onload", `Other f -> add f
    | "onload", `Keyboard _ -> failwith "keyboard event handler for onload"
    | "onload", `Touch _ -> failwith "touch event handler for onload"
    | "onload", `Mouse _ -> failwith "mouse event handler for onload"
    | name, `Other f ->
        Js.Unsafe.set node (Js.bytestring name)
          (Dom_html.handler (fun ev -> Js.bool (f ev)))
    | name, `Keyboard f ->
        Js.Unsafe.set node (Js.bytestring name)
          (Dom_html.handler (fun ev -> Js.bool (f ev)))
    | name, `Touch f ->
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
  | Xml.AFloat f -> (Js.number_of_float f)##toString
  | Xml.AInt i -> (Js.number_of_float (float_of_int i))##toString
  | Xml.AStr s -> Js.string s
  | Xml.AStrL (Xml.Space, sl) -> Js.string (String.concat " " sl)
  | Xml.AStrL (Xml.Comma, sl) -> Js.string (String.concat "," sl)

let class_list_of_racontent = function
  | Xml.AStr s -> [s]
  | Xml.AStrL (_space, l) -> l
  | _ -> failwith "attribute class is not a string"

let class_list_of_racontent_o = function
  | Some c -> class_list_of_racontent c
  | None -> []

let rebuild_class_list l1 l2 l3 =
  let f s =
    (not (List.exists (( = ) s) l2)) && not (List.exists (( = ) s) l3)
  in
  l3 @ List.filter f l1

let rebuild_class_string l1 l2 l3 =
  rebuild_class_list l1 l2 l3 |> String.concat " " |> Js.string

(* html attributes and dom properties use different names
   **example**: maxlength vs maxLenght (case sensitive).
   - Before dom react, it was enough to set html attributes only as
   there were no update after creation.
   - Dom React may update attributes later.
   Html attrib changes are not taken into account if the corresponding
   Dom property is defined.
   **example**: updating html attribute `value` has no effect
   if the dom property `value` has be set by the user.

   =WE NEED TO SET DOM PROPERTIES=
   -Tyxml only gives us html attribute names and we can set them safely.
   -The name for dom properties is maybe different.
    We set it only if we find out that the property
    match_the_attribute_name / is_already_defined (get_prop).
*)

(* TODO: fix get_prop
   it only work when html attribute and dom property names correspond.
   find a way to get dom property name corresponding to html attribute
*)

let get_prop node name =
  if Js.Optdef.test (Js.Unsafe.get node name) then Some name else None

let iter_prop node name f =
  match get_prop node name with Some n -> f n | None -> ()

let iter_prop_protected node name f =
  match get_prop node name with
  | Some n -> ( try f n with _ -> ())
  | None -> ()

let space_re = Regexp.regexp " "

let current_classes node =
  let name = Js.string "class" in
  Js.Opt.case
    node##(getAttribute name)
    (fun () -> [])
    (fun s -> Js.to_string s |> Regexp.(split space_re))

let rebuild_reactive_class_rattrib node s =
  let name = Js.string "class" in
  let e = React.S.diff (fun v v' -> v', v) s
  and f (v, v') =
    let l1 = current_classes node
    and l2 = class_list_of_racontent_o v
    and l3 = class_list_of_racontent_o v' in
    let s = rebuild_class_string l1 l2 l3 in
    node##(setAttribute name s);
    iter_prop node name (fun name -> Js.Unsafe.set node name s)
  in
  f (None, React.S.value s);
  Dom_reference.retain node ~keep:(React.E.map f e)

let rec rebuild_rattrib node ra =
  match Xml.racontent ra with
  | Xml.RA a when Xml.aname ra = "class" ->
      let l1 = current_classes node and l2 = class_list_of_racontent a in
      let name = Js.string "class" and s = rebuild_class_string l1 l2 l2 in
      node##(setAttribute name s)
  | Xml.RA a ->
      let name = Js.string (Xml.aname ra) in
      let v = rebuild_attrib_val a in
      node##(setAttribute name v)
  | Xml.RAReact s when Xml.aname ra = "class" ->
      rebuild_reactive_class_rattrib node s
  | Xml.RAReact s ->
      let name = Js.string (Xml.aname ra) in
      Dom_reference.retain node
        ~keep:
          (React.S.map
             (function
               | None ->
                   node##(removeAttribute name);
                   iter_prop_protected node name (fun name ->
                     Js.Unsafe.set node name Js.null)
               | Some v ->
                   let v = rebuild_attrib_val v in
                   node##(setAttribute name v);
                   iter_prop_protected node name (fun name ->
                     Js.Unsafe.set node name v))
             s)
  | Xml.RACamlEventHandler ev -> register_event_handler node (Xml.aname ra, ev)
  | Xml.RALazyStr s ->
      node##(setAttribute (Js.string (Xml.aname ra)) (Js.string s))
  | Xml.RALazyStrL (Xml.Space, l) ->
      node##(setAttribute
               (Js.string (Xml.aname ra))
               (Js.string (String.concat " " l)))
  | Xml.RALazyStrL (Xml.Comma, l) ->
      node##(setAttribute
               (Js.string (Xml.aname ra))
               (Js.string (String.concat "," l)))
  | Xml.RAClient (_, _, value) ->
      rebuild_rattrib node
        (Eliom_lib.from_poly (Eliom_lib.to_poly value) : Xml.attrib)

(* TODO: Registering a global "onunload" event handler breaks the
   'bfcache' mechanism of Firefox and Safari. We may try to use
   "pagehide" whenever this event exists. See:

   https://developer.mozilla.org/En/Using_Firefox_1.5_caching

   http://www.webkit.org/blog/516/webkit-page-cache-ii-the-unload-event/

   and the function [Eliommod_dom.test_pageshow_pagehide]. *)

let delay f =
  Js_of_ocaml_eio.Eio_js.start (fun () ->
    Fiber.yield
      ();
    f ())

module ReactState : sig
  type t

  val start_signal : (t -> unit React.signal) -> Dom.node Js.t
  val change_dom : t -> Dom.node Js.t -> unit
end = struct
  (*
     ISSUE
     =====
     There is a conflict when many dom react are inside each other.

     let s_lvl1 = S.map (function
     | case1 -> ..
     | case2 -> let s_lvl2 = ... in R.node s_lvl2) ...
     in R.node s_lvl1

     both dom react will update the same dom element (call it `dom_elt`) and
     we have to prevent an (outdated) s_lvl2 signal
     to replace `dom_elt` (updated last by a s_lvl1 signal)

     SOLUTION
     ========
     - we associate to the dom element an array of the signals that may update it
     - when a dom element is updated, we transfer the signals to the appropriate
       element: outer dom react are moved to the new element while inner dom react
       are left to the old element.
  *)

  class type ['a, 'b] weakMap = object
    method set : 'a -> 'b -> unit Js.meth
    method get : 'a -> 'b Js.Optdef.t Js.meth
  end

  type t =
    {mutable node : Dom.node Js.t option; mutable signal : unit React.S.t option}
  [@@warning "-69"]

  let signals : (Dom.node Js.t, t array) weakMap Js.t =
    let weakMap = Js.Unsafe.global##._WeakMap in
    new%js weakMap

  let get_signals (elt : Dom.node Js.t) : t array =
    Js.Optdef.get (signals##get elt) (fun () -> [||])

  let set_signals (elt : Dom.node Js.t) (a : t array) = signals##set elt a

  let signal_index id a =
    let rec find_rec id a l i =
      assert (i < l);
      if id == a.(i) then i else find_rec id a l (i + 1)
    in
    find_rec id a (Array.length a) 0

  let start_signal f =
    let state = {node = None; signal = None} in
    state.signal <- Some (f state);
    match state.node with Some dom -> dom | None -> assert false

  let change_dom state dom =
    match state.node with
    | None ->
        state.node <- Some dom;
        set_signals dom (Array.append [|state|] (get_signals dom))
    | Some dom' ->
        let signals' = get_signals dom' in
        let i = signal_index state signals' in
        let signals = get_signals dom in
        set_signals dom'
          (Array.sub signals' (i + 1) (Array.length signals' - i - 1));
        let parent_signals = Array.sub signals' 0 (i + 1) in
        Array.iter (fun state -> state.node <- Some dom) parent_signals;
        set_signals dom (Array.append parent_signals signals);
        Js.Opt.case dom'##.parentNode
          (fun () -> (* no parent -> no replace needed *) ())
          (fun parent ->
             Js.Opt.iter (Dom.CoerceTo.element parent) (fun parent ->
               (* really update the dom *)
               ignore (Dom_html.element parent)##(replaceChild dom dom')))
end

type content_ns = [`HTML5 | `SVG]

let rec rebuild_node' ns elt =
  match Xml.get_node elt with
  | Xml.DomNode node ->
      (* assert (Xml.get_node_id node <> NoId); *)
      node
  | Xml.ReactChildren (node, elts) ->
      let dom = raw_rebuild_node ns node in
      Js_of_ocaml_tyxml.Tyxml_js.Util.update_children dom
        (ReactiveData.RList.map (rebuild_node' ns) elts);
      Xml.set_dom_node elt dom;
      dom
  | Xml.ReactNode signal ->
      let dom =
        ReactState.start_signal (fun state ->
          React.S.map
            (fun elt' ->
               let dom = rebuild_node' ns elt' in
               Xml.set_dom_node elt dom;
               ReactState.change_dom state dom)
            signal)
      in
      Xml.set_dom_node elt dom; dom
  | Xml.TyXMLNode raw_elt -> (
    match Xml.get_node_id elt with
    | Xml.NoId -> raw_rebuild_node ns raw_elt
    | Xml.RequestId _ ->
        (* Do not look in request_nodes hashtbl: such elements have
         been bind while unwrapping nodes. *)
        let node = raw_rebuild_node ns raw_elt in
        Xml.set_dom_node elt node; node
    | Xml.ProcessId id ->
        let id = Js.string id in
        Js.Optdef.case (find_process_node id)
          (fun () ->
             let node = raw_rebuild_node ns (Xml.content elt) in
             register_process_node id node;
             node)
          (fun n -> (n :> Dom.node Js.t)))

and raw_rebuild_node ns = function
  | Xml.Empty | Xml.Comment _ ->
      (* FIXME *)
      (Dom_html.document##(createTextNode (Js.string "")) :> Dom.node Js.t)
  | Xml.EncodedPCDATA s | Xml.PCDATA s ->
      (Dom_html.document##(createTextNode (Js.string s)) :> Dom.node Js.t)
  | Xml.Entity s ->
      let entity = Dom_html.decode_html_entities (Js.string ("&" ^ s ^ ";")) in
      (Dom_html.document##(createTextNode entity) :> Dom.node Js.t)
  | Xml.Leaf (name, attribs) ->
      let node = Dom_html.document##(createElement (Js.string name)) in
      List.iter (rebuild_rattrib node) attribs;
      (node :> Dom.node Js.t)
  | Xml.Node (name, attribs, childrens) ->
      let ns = if name = "svg" then `SVG else ns in
      let node =
        match ns with
        | `HTML5 -> Dom_html.document##(createElement (Js.string name))
        | `SVG ->
            let svg_ns = "http://www.w3.org/2000/svg" in
            Dom_html.document##(createElementNS (Js.string svg_ns)
                                  (Js.string name))
      in
      List.iter (rebuild_rattrib node) attribs;
      List.iter (fun c -> Dom.appendChild node (rebuild_node' ns c)) childrens;
      (node :> Dom.node Js.t)

(* [is_before_initial_load] tests whether it is executed before the
   loading of the initial document, e.g. during the initialization of the
   (OCaml) module, i.e. before [Eliom_client_main.onload]. *)
let is_before_initial_load, set_initial_load =
  let before_load = ref true in
  (fun () -> !before_load), fun () -> before_load := false

let rebuild_node_ns ns context elt' =
  Logs.debug ~src:section (fun fmt ->
    fmt "Rebuild node %s (%s)"
      (Eliom_content_core.Xml.string_of_node_id (Xml.get_node_id elt'))
      context);
  if is_before_initial_load ()
  then (
    log_inspect (rebuild_node' ns elt');
    raise_error ~section
      "Cannot apply %s%s before the document is initially loaded@\n" context
      Xml.(
        match get_node_id elt' with
        | NoId -> " "
        | RequestId id -> " on request node " ^ id
        | ProcessId id -> " on global node " ^ id));
  let node = Js.Unsafe.coerce (rebuild_node' ns elt') in
  flush_load_script (); node

let rebuild_node_svg context elt =
  let elt' = Eliom_content_core.Svg.F.toelt elt in
  rebuild_node_ns `SVG context elt'

(** The first argument describes the calling function (if any) in case
    of an error. *)
let rebuild_node context elt =
  let elt' = Eliom_content_core.Html.F.toelt elt in
  rebuild_node_ns `HTML5 context elt'

(******************************************************************************)

module Syntax_helpers = struct
  let register_client_closure closure_id closure =
    Client_closure.register ~closure_id ~closure

  let open_client_section compilation_unit_id =
    do_next_server_section_data ~compilation_unit_id;
    do_next_client_section_data ~compilation_unit_id

  let close_server_section compilation_unit_id =
    do_next_server_section_data ~compilation_unit_id

  let get_escaped_value = from_poly
  let get_injection ?ident ?pos name = Injection.get ?ident ?pos ~name
end
