(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

let section = Lwt_log.Section.make "eliom:dom"

let iter_nodeList nodeList f =
  for i = 0 to nodeList##.length - 1 do
    (* Unsafe.get is ten time faster than nodeList##item *)
    f (Js.Unsafe.get nodeList i)
  done

let iter_attrList (attrList : Dom.attr Dom.namedNodeMap Js.t)
    (f : Dom.attr Js.t -> unit)
  =
  for i = 0 to attrList##.length - 1 do
    (* Unsafe.get is ten time faster than nodeList##item.
       Is it the same for attrList ? *)
    (* let v = attrList##item(i) in *)
    let v = Js.Unsafe.get attrList i in
    (* IE8 provides [null] in node##attributes;
       so we wrap v to be a Js.opt *)
    Js.Opt.iter v f
  done

(* Dummy type used in the following "test_*" functions to test the
   presence of methods in various browsers. *)
class type dom_tester =
  object
    method compareDocumentPosition : unit Js.optdef Js.prop
    method querySelectorAll : unit Js.optdef Js.prop
    method classList : unit Js.optdef Js.prop
    method createEvent : unit Js.optdef Js.prop
    method onpageshow : unit Js.optdef Js.prop
    method onpagehide : unit Js.optdef Js.prop
    method onhashchange : unit Js.optdef Js.prop
  end

let test_querySelectorAll () =
  Js.Optdef.test
    (Js.Unsafe.coerce Dom_html.document : dom_tester Js.t)##.querySelectorAll

let test_compareDocumentPosition () =
  Js.Optdef.test
    (Js.Unsafe.coerce Dom_html.document : dom_tester Js.t)##.compareDocumentPosition

let test_classList () =
  Js.Optdef.test
    (Js.Unsafe.coerce Dom_html.document##.documentElement : dom_tester Js.t)##.classList

let test_createEvent () =
  Js.Optdef.test
    (Js.Unsafe.coerce Dom_html.document : dom_tester Js.t)##.createEvent

let test_pageshow_pagehide () =
  let tester = (Js.Unsafe.coerce Dom_html.window : dom_tester Js.t) in
  Js.Optdef.test tester##.onpageshow && Js.Optdef.test tester##.onpagehide

let test_onhashchange () =
  Js.Optdef.test
    (Js.Unsafe.coerce Dom_html.window : dom_tester Js.t)##.onhashchange

let fast_ancessor (elt1 : #Dom.node Js.t) (elt2 : #Dom.node Js.t) =
  let open Dom.DocumentPosition in
  has elt1 ## (compareDocumentPosition (elt2 :> Dom.node Js.t)) contained_by

let slow_ancessor (elt1 : #Dom.node Js.t) (elt2 : #Dom.node Js.t) =
  let rec check_parent n =
    if Js.strict_equals n (elt1 :> Dom.node Js.t)
    then true
    else
      match Js.Opt.to_option n##.parentNode with
      | None -> false
      | Some p -> check_parent p
  in
  check_parent (elt2 :> Dom.node Js.t)

let ancessor =
  if test_compareDocumentPosition () then fast_ancessor else slow_ancessor

let fast_select_request_nodes root =
  root
  ## (querySelectorAll
        (Js.string ("." ^ Eliom_runtime.RawXML.request_node_class)))

let fast_select_nodes root =
  if !Eliom_config.debug_timings
  then Firebug.console ## (time (Js.string "fast_select_nodes"));
  let a_nodeList : Dom_html.element Dom.nodeList Js.t =
    root
    ## (querySelectorAll
          (Js.string ("a." ^ Eliom_runtime.RawXML.ce_call_service_class)))
  in
  let a_nodeList : Dom_html.anchorElement Dom.nodeList Js.t =
    Js.Unsafe.coerce a_nodeList
  in
  let form_nodeList : Dom_html.element Dom.nodeList Js.t =
    root
    ## (querySelectorAll
          (Js.string ("form." ^ Eliom_runtime.RawXML.ce_call_service_class)))
  in
  let form_nodeList : Dom_html.formElement Dom.nodeList Js.t =
    Js.Unsafe.coerce form_nodeList
  in
  let process_node_nodeList =
    root
    ## (querySelectorAll
          (Js.string ("." ^ Eliom_runtime.RawXML.process_node_class)))
  in
  let closure_nodeList =
    root
    ## (querySelectorAll
          (Js.string ("." ^ Eliom_runtime.RawXML.ce_registered_closure_class)))
  in
  let attrib_nodeList =
    root
    ## (querySelectorAll
          (Js.string ("." ^ Eliom_runtime.RawXML.ce_registered_attr_class)))
  in
  if !Eliom_config.debug_timings
  then Firebug.console ## (timeEnd (Js.string "fast_select_nodes"));
  ( a_nodeList
  , form_nodeList
  , process_node_nodeList
  , closure_nodeList
  , attrib_nodeList )

let slow_has_classes (node : Dom_html.element Js.t) =
  let classes =
    (* IE<9: className is not set after change_page; getAttribute("class")
       does not work for the initial document *)
    let str =
      if node##.className = Js.string ""
      then
        Js.Opt.get
          node ## (getAttribute (Js.string "class"))
          (fun () -> Js.string "")
      else node##.className
    in
    Js.str_array str ## (split (Js.string " "))
  in
  let found_call_service = ref false in
  let found_process_node = ref false in
  let found_closure = ref false in
  let found_attrib = ref false in
  for i = 0 to classes##.length - 1 do
    found_call_service :=
      Js.Optdef.strict_equals (Js.array_get classes i)
        (Js.def (Js.string Eliom_runtime.RawXML.ce_call_service_class))
      || !found_call_service;
    found_process_node :=
      Js.Optdef.strict_equals (Js.array_get classes i)
        (Js.def (Js.string Eliom_runtime.RawXML.process_node_class))
      || !found_process_node;
    found_closure :=
      Js.Optdef.strict_equals (Js.array_get classes i)
        (Js.def (Js.string Eliom_runtime.RawXML.ce_registered_closure_class))
      || !found_closure;
    found_attrib :=
      Js.Optdef.strict_equals (Js.array_get classes i)
        (Js.def (Js.string Eliom_runtime.RawXML.ce_registered_attr_class))
      || !found_attrib
  done;
  !found_call_service, !found_process_node, !found_closure, !found_attrib

let slow_has_request_class (node : Dom_html.element Js.t) =
  let classes = Js.str_array node ##. className ## (split (Js.string " ")) in
  let found_request_node = ref false in
  for i = 0 to classes##.length - 1 do
    found_request_node :=
      Js.Optdef.strict_equals (Js.array_get classes i)
        (Js.def (Js.string Eliom_runtime.RawXML.request_node_class))
      || !found_request_node
  done;
  !found_request_node

let fast_has_classes (node : Dom_html.element Js.t) =
  ( Js.to_bool
      node ##. classList
      ## (contains (Js.string Eliom_runtime.RawXML.ce_call_service_class))
  , Js.to_bool
      node ##. classList
      ## (contains (Js.string Eliom_runtime.RawXML.process_node_class))
  , Js.to_bool
      node ##. classList
      ## (contains (Js.string Eliom_runtime.RawXML.ce_registered_closure_class))
  , Js.to_bool
      node ##. classList
      ## (contains (Js.string Eliom_runtime.RawXML.ce_registered_attr_class)) )

let fast_has_request_class (node : Dom_html.element Js.t) =
  Js.to_bool
    node ##. classList
    ## (contains (Js.string Eliom_runtime.RawXML.request_node_class))

let has_classes : Dom_html.element Js.t -> bool * bool * bool * bool =
  if test_classList () then fast_has_classes else slow_has_classes

let has_request_class : Dom_html.element Js.t -> bool =
  if test_classList () then fast_has_request_class else slow_has_request_class

let slow_select_request_nodes (root : Dom_html.element Js.t) =
  let node_array = new%js Js.array_empty in
  let rec traverse (node : Dom.node Js.t) =
    match node##.nodeType with
    | Dom.ELEMENT ->
        let node = (Js.Unsafe.coerce node : Dom_html.element Js.t) in
        if has_request_class node then ignore node_array ## (push node);
        iter_nodeList node##.childNodes traverse
    | _ -> ()
  in
  traverse (root :> Dom.node Js.t);
  (Js.Unsafe.coerce node_array : Dom_html.element Dom.nodeList Js.t)

let slow_select_nodes (root : Dom_html.element Js.t) =
  let a_array = new%js Js.array_empty in
  let form_array = new%js Js.array_empty in
  let node_array = new%js Js.array_empty in
  let closure_array = new%js Js.array_empty in
  let attrib_array = new%js Js.array_empty in
  let rec traverse (node : Dom.node Js.t) =
    match node##.nodeType with
    | Dom.ELEMENT ->
        let node = (Js.Unsafe.coerce node : Dom_html.element Js.t) in
        let call_service, process_node, closure, attrib = has_classes node in
        (if call_service
        then
          match Dom_html.tagged node with
          | Dom_html.A e -> ignore a_array ## (push e)
          | Dom_html.Form e -> ignore form_array ## (push e)
          | _ ->
              Lwt_log.raise_error_f ~section "%s element tagged as eliom link"
                (Js.to_string node##.tagName));
        if process_node then ignore node_array ## (push node);
        if closure then ignore closure_array ## (push node);
        if attrib then ignore attrib_array ## (push node);
        iter_nodeList node##.childNodes traverse
    | _ -> ()
  in
  traverse (root :> Dom.node Js.t);
  ( (Js.Unsafe.coerce a_array : Dom_html.anchorElement Dom.nodeList Js.t)
  , (Js.Unsafe.coerce form_array : Dom_html.formElement Dom.nodeList Js.t)
  , (Js.Unsafe.coerce node_array : Dom_html.element Dom.nodeList Js.t)
  , (Js.Unsafe.coerce closure_array : Dom_html.element Dom.nodeList Js.t)
  , (Js.Unsafe.coerce attrib_array : Dom_html.element Dom.nodeList Js.t) )

let select_nodes =
  if test_querySelectorAll () then fast_select_nodes else slow_select_nodes

let select_request_nodes =
  if test_querySelectorAll ()
  then fast_select_request_nodes
  else slow_select_request_nodes

(* createEvent for ie < 9 *)

let createEvent_ie ev_type =
  let evt : #Dom_html.event Js.t =
    (Js.Unsafe.coerce Dom_html.document)##createEventObject
  in
  (Js.Unsafe.coerce evt)##._type := (Js.string "on") ## (concat ev_type);
  evt

let createEvent_normal ev_type =
  let evt : #Dom_html.event Js.t =
    (Js.Unsafe.coerce Dom_html.document)
    ## (createEvent (Js.string "HTMLEvents"))
  in
  let () = (Js.Unsafe.coerce evt) ## (initEvent ev_type false false) in
  evt

let createEvent =
  if test_createEvent () then createEvent_normal else createEvent_ie

(* DOM traversal *)

class type ['element] get_tag =
  object
    method getElementsByTagName :
      Js.js_string Js.t -> 'element Dom.nodeList Js.t Js.meth
  end

(* We can't use Dom_html.document##head: it is not defined in ff3.6... *)
let get_head (page : 'element #get_tag Js.t) : 'element Js.t =
  Js.Opt.get
    page ## (getElementsByTagName (Js.string "head")) ## (item 0)
    (fun () -> Lwt_log.raise_error ~section "get_head")

let get_body (page : 'element #get_tag Js.t) : 'element Js.t =
  Js.Opt.get
    page ## (getElementsByTagName (Js.string "body")) ## (item 0)
    (fun () -> Lwt_log.raise_error ~section "get_body")

let iter_dom_array (f : 'a -> unit)
    (a :
      < length : < get : int ; .. > Js.gen_prop
      ; item : int -> 'a Js.opt Js.meth
      ; .. >
      Js.t)
  =
  let length = a##.length in
  for i = 0 to length - 1 do
    Js.Opt.iter a ## (item i) f
  done

let copy_text t = Dom_html.document ## (createTextNode t##.data)

(* ie, ff3.6 and safari does not like setting innerHTML on html and
   head nodes: we need to rebuild the HTML dom tree from the XML dom
   tree received in the xhr *)

(* BEGIN IE<9 HACK:
   appendChild is broken in ie:
   see
     http://webbugtrack.blogspot.com/2009/01/bug-143-createtextnode-doesnt-work-on.html
     http://webbugtrack.blogspot.com/2007/10/bug-142-appendchild-doesnt-work-on.html

   This fix appending to script element.
   TODO: it is also broken when appending tr to tbody, need to find a solution
*)
let add_childrens (elt : Dom_html.element Js.t) (sons : Dom.node Js.t list) =
  try List.iter (Dom.appendChild elt) sons
  with exn -> (
    (* this code is ie only, there are no reason for an appendChild
       to fail normally *)
    let concat l =
      let rec concat acc = function
        | [] -> acc
        | t :: q ->
            let txt =
              match Dom.nodeType t with
              | Dom.Text t -> t
              | _ ->
                  Lwt_log.raise_error_f ~section
                    "add_childrens: not text node in tag %s"
                    (Js.to_string elt##.tagName)
            in
            concat acc ## (concat txt##.data) q
      in
      concat (Js.string "") l
    in
    match Dom_html.tagged elt with
    | Dom_html.Script elt -> elt##.text := concat sons
    | Dom_html.Style elt ->
        (* we need to append the style node to something. If we
         don't do that the styleSheet field is not created if we.
         And we can't do it by creating it with the ie specific
         document.createStyleSheet: the styleSheet field is not
         initialised and it can't be set either. *)
        let d = Dom_html.createHead Dom_html.document in
        Dom.appendChild d elt;
        (Js.Unsafe.coerce elt)##.styleSheet##.cssText := concat sons
    | _ -> Lwt_log.raise_error ~section ~exn "add_childrens: can't appendChild")

(* END IE HACK *)

let copy_element (e : Dom.element Js.t)
    (registered_process_node : Js.js_string Js.t -> bool)
    : Dom_html.element Js.t
  =
  let rec aux (e : Dom.element Js.t) =
    let copy = Dom_html.document ## (createElement e##.tagName) in
    (* IE<9: Copy className separately, it's not updated when displayed *)
    Js.Opt.iter (Dom_html.CoerceTo.element e) (fun e ->
        copy##.className := e##.className);
    let node_id =
      Js.Opt.to_option
        e ## (getAttribute (Js.string Eliom_runtime.RawXML.node_id_attrib))
    in
    match node_id with
    | Some id when registered_process_node id ->
        Js.Opt.iter
          e ## (getAttribute (Js.string "class"))
          (fun classes -> copy ## (setAttribute (Js.string "class") classes));
        copy
        ## (setAttribute (Js.string Eliom_runtime.RawXML.node_id_attrib) id);
        Some copy
    | _ ->
        let add_attribute a =
          Js.Opt.iter (Dom.CoerceTo.attr a)
            (* we don't use copy##attributes##setNameditem:
             in ie 9 it fail setting types of buttons... *)
            (fun a -> copy ## (setAttribute a##.name a##.value))
        in
        iter_dom_array add_attribute e##.attributes;
        let child_copies =
          List.map_filter
            (fun child ->
              match Dom.nodeType child with
              | Dom.Text t -> Some (copy_text t :> Dom.node Js.t)
              | Dom.Element child -> (aux child :> Dom.node Js.t option)
              | _ -> None)
            (Dom.list_of_nodeList e##.childNodes)
        in
        add_childrens copy child_copies;
        Some copy
  in
  match aux e with
  | None -> Lwt_log.raise_error ~section "copy_element"
  | Some e -> e

let html_document (src : Dom.element Dom.document Js.t) registered_process_node
    : Dom_html.element Js.t
  =
  let content = src##.documentElement in
  match Js.Opt.to_option (Dom_html.CoerceTo.element content) with
  | Some e -> (
    try Dom_html.document ## (adoptNode (e :> Dom.element Js.t))
    with exn -> (
      Lwt_log.ign_debug ~section ~exn "can't adopt node, import instead";
      try Dom_html.document ## (importNode (e :> Dom.element Js.t) Js._true)
      with exn ->
        Lwt_log.ign_debug ~section ~exn "can't import node, copy instead";
        copy_element content registered_process_node))
  | None ->
      Lwt_log.ign_debug ~section
        "can't adopt node, document not parsed as html. copy instead";
      copy_element content registered_process_node

(** CSS preloading. *)

let spaces_re = Regexp.regexp " +"

let is_stylesheet e =
  (* FIX: should eventually use Dom_html.element *)
  Js.Opt.case
    (Dom_html.CoerceTo.link (Js.Unsafe.coerce e))
    (fun _ -> false)
    (fun e ->
      List.exists
        (fun s -> s = "stylesheet")
        (Regexp.split spaces_re (Js.to_string e##.rel))
      && Js.strict_equals e##._type (Js.string "text/css"))

let basedir_re = Regexp.regexp "^(([^/?]*/)*)([^/?]*)(\\?.*)?$"

let basedir path =
  match Regexp.string_match basedir_re path 0 with
  | None -> "/"
  | Some res -> (
    match Regexp.matched_group res 1 with
    | None -> (
      match Regexp.matched_group res 3 with Some ".." -> "../" | _ -> "/")
    | Some dir -> (
      match Regexp.matched_group res 3 with
      | Some ".." -> dir ^ "../"
      | _ -> dir))

let fetch_linked_css e =
  let rec extract acc (e : Dom.node Js.t) =
    match Dom.nodeType e with
    | Dom.Element e when is_stylesheet e ->
        let e : Dom_html.linkElement Js.t = Js.Unsafe.coerce e in
        let href = e##.href in
        if Js.to_bool e##.disabled
           || e##.title##.length > 0
           || href##.length = 0
        then acc
        else
          let href = Js.to_string href in
          let css =
            Eliom_request.http_get href [] Eliom_request.string_result
          in
          acc @ [e, (e##.media, href, css >|= snd)]
    | Dom.Element e ->
        let c = e##.childNodes in
        let acc = ref acc in
        for i = 0 to c##.length - 1 do
          acc := extract !acc (Js.Opt.get c ## (item i) (fun _ -> assert false))
        done;
        !acc
    | _ -> acc
  in
  extract [] (e :> Dom.node Js.t)

let url_content_raw = "([^'\\\"]([^\\\\\\)]|\\\\.)*)"
let dbl_quoted_url_raw = "\"(([^\\\\\"]|\\\\.)*)\""
let quoted_url_raw = "'(([^\\\\']|\\\\.)*)'"

let url_re =
  Regexp.regexp
    (Printf.sprintf "url\\s*\\(\\s*(%s|%s|%s)\\s*\\)\\s*" dbl_quoted_url_raw
       quoted_url_raw url_content_raw)

let raw_url_re =
  Regexp.regexp
    (Printf.sprintf "\\s*(%s|%s)\\s*" dbl_quoted_url_raw quoted_url_raw)

let absolute_re = Regexp.regexp "\\s*(https?:\\/\\/|data:|file:|\\/)"

let absolute_re2 =
  Regexp.regexp "['\\\"]\\s*((https?:\\/\\/|data:|file:|\\/).*)['\\\"]$"

exception Incorrect_url

let parse_absolute ~prefix href =
  match Regexp.search absolute_re href 0 with
  | Some (i, _) when i = 0 -> (* absolute URL -> do not rewrite *) href
  | _ -> (
    match Regexp.search absolute_re2 href 0 with
    | Some (i, res) when i = 0 -> (
      match Regexp.matched_group res 1 with
      | Some href -> (* absolute URL -> do not rewrite *) href
      | None -> raise Incorrect_url)
    | _ -> prefix ^ href)

let parse_url ~prefix css pos =
  match Regexp.search url_re css pos with
  | Some (i, res) when i = pos -> (
      ( i + String.length (Regexp.matched_string res)
      , match Regexp.matched_group res 2 with
        | Some href -> parse_absolute ~prefix href
        | None -> (
          match Regexp.matched_group res 3 with
          | Some href -> parse_absolute ~prefix href
          | None -> (
            match Regexp.matched_group res 4 with
            | Some href -> parse_absolute ~prefix href
            | None -> raise Incorrect_url)) ))
  | _ -> (
    match Regexp.search raw_url_re css pos with
    | Some (i, res) when i = pos -> (
        ( i + String.length (Regexp.matched_string res)
        , match Regexp.matched_group res 1 with
          | Some href -> parse_absolute ~prefix href
          | None -> raise Incorrect_url ))
    | _ -> raise Incorrect_url)

let parse_media css pos =
  let i =
    try String.index_from css pos ';' with Not_found -> String.length css
  in
  i + 1, String.sub css pos (i - pos)

(* Look for relative URL only... *)
let url_re =
  Regexp.regexp "url\\s*\\(\\s*(?!('|\")?(https?:\\/\\/|data:|file:|\\/))"

let rewrite_css_url ~prefix css pos =
  let len = String.length css - pos in
  let buf = Buffer.create (len + (len / 2)) in
  let rec rewrite pos =
    if pos < String.length css
    then
      match Regexp.search url_re css pos with
      | None -> Buffer.add_substring buf css pos (String.length css - pos)
      | Some (i, _res) -> (
          Buffer.add_substring buf css pos (i - pos);
          try
            let i, href = parse_url ~prefix css i in
            Buffer.add_string buf "url('";
            Buffer.add_string buf href;
            Buffer.add_string buf "')";
            rewrite i
          with Incorrect_url ->
            Buffer.add_substring buf css i (String.length css - i))
  in
  rewrite pos; Buffer.contents buf

let import_re = Regexp.regexp "@import\\s*"

let rec rewrite_css ~max (media, href, css) =
  try%lwt
    css >>= function
    | None -> Lwt.return_nil
    | Some css ->
        if !Eliom_config.debug_timings
        then Firebug.console ## (time (Js.string ("rewrite_CSS: " ^ href)));
        let%lwt imports, css =
          rewrite_css_import ~max ~prefix:(basedir href) ~media css 0
        in
        if !Eliom_config.debug_timings
        then Firebug.console ## (timeEnd (Js.string ("rewrite_CSS: " ^ href)));
        Lwt.return (imports @ [media, css])
  with _ -> Lwt.return [media, Printf.sprintf "@import url(%s);" href]

and rewrite_css_import ?(charset = "") ~max ~prefix ~media css pos =
  match Regexp.search import_re css pos with
  | None ->
      (* No @import anymore, rewrite url. *)
      Lwt.return ([], rewrite_css_url ~prefix css pos)
  | Some (i, res) -> (
      (* Found @import rule, try to preload. *)
      let init = String.sub css pos (i - pos) in
      let charset = if pos = 0 then init else charset in
      try
        let i = i + String.length (Regexp.matched_string res) in
        let i, href = parse_url ~prefix css i in
        let i, media' = parse_media css i in
        let%lwt import =
          if max = 0
          then
            (* Maximum imbrication of @import reached, rewrite url. *)
            Lwt.return
              [media, Printf.sprintf "@import url('%s') %s;\n" href media']
          else if media##.length > 0 && String.length media' > 0
          then
            (* TODO combine media if possible...
               in the mean time keep explicit import. *)
            Lwt.return
              [media, Printf.sprintf "@import url('%s') %s;\n" href media']
          else
            let media =
              if media##.length > 0 then media else Js.string media'
            in
            let css =
              Eliom_request.http_get href [] Eliom_request.string_result
            in
            rewrite_css ~max:(max - 1) (media, href, css >|= snd)
        and imports, css =
          rewrite_css_import ~charset ~max ~prefix ~media css i
        in
        Lwt.return (import @ imports, css)
      with
      | Incorrect_url -> Lwt.return ([], rewrite_css_url ~prefix css pos)
      | exn ->
          Lwt_log.ign_info ~section ~exn "Error while importing css";
          Lwt.return ([], rewrite_css_url ~prefix css pos))

let max_preload_depth = ref 4

let build_style (e, css) =
  let%lwt css = rewrite_css ~max:!max_preload_depth css in
  (* lwt css = *)
  Lwt_list.map_p
    (fun (media, css) ->
      let style = Dom_html.createStyle Dom_html.document in
      style##._type := Js.string "text/css";
      style##.media := media;
      (* IE8: Assigning to style##innerHTML results in
          "Unknown runtime error" *)
      let styleSheet = Js.Unsafe.(get style (Js.string "styleSheet")) in
      if Js.Optdef.test styleSheet
      then Js.Unsafe.(set styleSheet (Js.string "cssText") (Js.string css))
      else style##.innerHTML := Js.string css;
      Lwt.return (e, (style :> Dom.node Js.t)))
    css

(* IE8 doesn't allow appendChild on noscript-elements *)
(* (\* Noscript is used to group style. It's ignored by the parser when *)
(*    scripting is enabled, but does not seems to be ignore when *)
(*    inserted as a DOM element. *\) *)
(* let node = Dom_html.createNoscript Dom_html.document in *)
(* List.iteri (fun i x -> debug "HOC 3.%i" i; Dom.appendChild node x) css; *)
(* Lwt.return (e, node )*)

let preload_css (doc : Dom_html.element Js.t) =
  if !Eliom_config.debug_timings
  then Firebug.console ## (time (Js.string "preload_css (fetch+rewrite)"));
  let%lwt css = Lwt_list.map_p build_style (fetch_linked_css (get_head doc)) in
  let css = List.concat css in
  List.iter
    (fun (e, css) ->
      try Dom.replaceChild (get_head doc) css e
      with _ ->
        (* Node was a unique node that has been removed...
                       in a perfect settings we won't have parsed it... *)
        Lwt_log.ign_info ~section "Unique CSS skipped...")
    css;
  if !Eliom_config.debug_timings
  then Firebug.console ## (timeEnd (Js.string "preload_css (fetch+rewrite)"));
  Lwt.return_unit

(** Window scrolling *)

(* Correct scrolling information in Chromium are found
   Dom_html.document##body while on Firefox they are found on
   Dom_html.document##documentElement. *)

type position =
  {html_top : int; html_left : int; body_top : int; body_left : int}
[@@deriving json]

let top_position = {html_top = 0; html_left = 0; body_top = 0; body_left = 0}

let createDocumentScroll () =
  { html_top = Dom_html.document##.documentElement##.scrollTop
  ; html_left = Dom_html.document##.documentElement##.scrollLeft
  ; body_top = Dom_html.document##.body##.scrollTop
  ; body_left = Dom_html.document##.body##.scrollLeft }

(* With firefox, the scroll position is restored before to fire the
   popstate event. We maintain our own position. *)

let current_position = ref top_position

let _ =
  (* HACK: Remove this when js_of_ocaml 1.1.2 or greater is released... *)
  (* window##onscroll <- *)
  ignore
    (Dom.addEventListener Dom_html.document (Dom.Event.make "scroll")
       (Dom_html.handler (fun _event ->
            current_position := createDocumentScroll ();
            Js._false))
       Js._true
      : Dom_html.event_listener_id)

let getDocumentScroll () = !current_position

let setDocumentScroll pos =
  Dom_html.document##.documentElement##.scrollTop := pos.html_top;
  Dom_html.document##.documentElement##.scrollLeft := pos.html_left;
  Dom_html.document##.body##.scrollTop := pos.body_top;
  Dom_html.document##.body##.scrollLeft := pos.body_left;
  current_position := pos

(* UGLY HACK for Opera bug: Opera seem does not always take into
   account the content of the base element. If we touch it like that,
   it remember its presence... *)
let touch_base () =
  Js.Opt.iter
    (Js.Opt.bind
       Dom_html.document
       ## (getElementById (Js.string Eliom_common_base.base_elt_id))
       Dom_html.CoerceTo.base)
    (fun e ->
      let href = e##.href in
      e##.href := href)

(* BEGIN FORMDATA HACK: This is only needed if FormData is not available in the browser.
   When it will be commonly available, remove all sections marked by "FORMDATA HACK" !
   Notice: this hack is used to circumvent a limitation in FF4 implementation of formdata:
     if the user click on a button in a form, formdatas created in the onsubmit callback normally contains the value of the button. ( it is the behaviour of chromium )
     in FF4, it is not the case: we must do this hack to find which button was clicked.

   NOTICE: this may not be corrected the way we want:
     see https://bugzilla.mozilla.org/show_bug.cgi?id=647231
     html5 will explicitly specify that chromium behaviour is wrong...

   This is implemented in:
   * this file -> here and called in load_eliom_data
   * Eliom_request: in send_post_form
   * in js_of_ocaml, module Form: the code to emulate FormData *)

let onclick_on_body_handler event =
  (match Dom_html.tagged (Dom_html.eventTarget event) with
  | Dom_html.Button button -> Js.Unsafe.global##.eliomLastButton := Some button
  | Dom_html.Input input when input##._type = Js.string "submit" ->
      Js.Unsafe.global##.eliomLastButton := Some input
  | _ -> Js.Unsafe.global##.eliomLastButton := None);
  Js._true

let add_formdata_hack_onclick_handler () =
  ignore
    (Dom_html.addEventListener
       Dom_html.window##.document##.body
       Dom_html.Event.click
       (Dom_html.handler onclick_on_body_handler)
       Js._true
      : Dom_html.event_listener_id)

(* END FORMDATA HACK *)

(** onhashchange *)

let hashchange = Dom.Event.make "hashchange"

let onhashchange f =
  if test_onhashchange ()
  then
    ignore
      (Dom.addEventListener Dom_html.window hashchange
         (Dom_html.handler (fun _ ->
              f Dom_html.window##.location##.hash;
              Js._false))
         Js._true
        : Dom_html.event_listener_id)
  else
    let last_fragment = ref Dom_html.window##.location##.hash in
    let check () =
      if not (Js.equals !last_fragment Dom_html.window##.location##.hash)
      then (
        last_fragment := Dom_html.window##.location##.hash;
        f Dom_html.window##.location##.hash)
    in
    ignore
      Dom_html.window
      ## (setInterval (Js.wrap_callback check) (Js.float (0.2 *. 1000.)))
