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

open Lwt.Syntax
open Js_of_ocaml
open Lib

let section = Logs.Src.create "eliom:dom"

let iter_nodeList nodeList f =
  for i = 0 to nodeList##.length - 1 do
    (* Unsafe.get is ten time faster than nodeList##item *)
    f (Js.Unsafe.get nodeList i)
  done

let iter_attrList
      (attrList : Dom.attr Dom.namedNodeMap Js.t)
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
class type dom_tester = object
  method onpageshow : unit Js.optdef Js.prop
  method onpagehide : unit Js.optdef Js.prop
  method onhashchange : unit Js.optdef Js.prop
end

let test_pageshow_pagehide () =
  let tester = (Js.Unsafe.coerce Dom_html.window : dom_tester Js.t) in
  Js.Optdef.test tester##.onpageshow && Js.Optdef.test tester##.onpagehide

let test_onhashchange () =
  Js.Optdef.test
    (Js.Unsafe.coerce Dom_html.window : dom_tester Js.t)##.onhashchange

let ancestor (elt1 : #Dom.node Js.t) (elt2 : #Dom.node Js.t) =
  let open Dom.DocumentPosition in
  has elt1##(compareDocumentPosition (elt2 :> Dom.node Js.t)) contained_by

let select_request_nodes root =
  root##(querySelectorAll (Js.string ("." ^ Runtime.RawXML.request_node_class)))

let select_nodes root =
  Config.debug_time "select_nodes";
  let a_nodeList : Dom_html.element Dom.nodeList Js.t =
    root##(querySelectorAll
             (Js.string ("a." ^ Runtime.RawXML.ce_call_service_class)))
  in
  let a_nodeList : Dom_html.anchorElement Dom.nodeList Js.t =
    Js.Unsafe.coerce a_nodeList
  in
  let form_nodeList : Dom_html.element Dom.nodeList Js.t =
    root##(querySelectorAll
             (Js.string ("form." ^ Runtime.RawXML.ce_call_service_class)))
  in
  let form_nodeList : Dom_html.formElement Dom.nodeList Js.t =
    Js.Unsafe.coerce form_nodeList
  in
  let process_node_nodeList =
    root##(querySelectorAll
             (Js.string ("." ^ Runtime.RawXML.process_node_class)))
  in
  let closure_nodeList =
    root##(querySelectorAll
             (Js.string ("." ^ Runtime.RawXML.ce_registered_closure_class)))
  in
  let attrib_nodeList =
    root##(querySelectorAll
             (Js.string ("." ^ Runtime.RawXML.ce_registered_attr_class)))
  in
  Config.debug_time_end "select_nodes";
  ( a_nodeList
  , form_nodeList
  , process_node_nodeList
  , closure_nodeList
  , attrib_nodeList )

let createEvent ev_type =
  let evt : #Dom_html.event Js.t =
    (Js.Unsafe.coerce Dom_html.document)##(createEvent (Js.string "HTMLEvents"))
  in
  let () = (Js.Unsafe.coerce evt)##(initEvent ev_type false false) in
  evt

(* DOM traversal *)

(* We can't use Dom_html.document##head: it is not defined in ff3.6...
   [getElementsByTagName] returns a [Dom.nodeList] on js_of_ocaml < 6.4 and a
   [Dom.collection] since 6.4; both provide [item], so we just require a
   [#Dom.element]. *)
let get_head (page : #Dom.element Js.t) : Dom.element Js.t =
  Js.Opt.get
    page##(getElementsByTagName (Js.string "head"))##(item 0)
    (fun () -> raise_error ~section "get_head")

let get_body (page : #Dom.element Js.t) : Dom.element Js.t =
  Js.Opt.get
    page##(getElementsByTagName (Js.string "body"))##(item 0)
    (fun () -> raise_error ~section "get_body")

let iter_dom_array
      (f : 'a -> unit)
      (a :
        < length : < get : int ; .. > Js.gen_prop
        ; item : int -> 'a Js.opt Js.meth
        ; .. >
          Js.t)
  =
  let length = a##.length in
  for i = 0 to length - 1 do
    Js.Opt.iter a##(item i) f
  done

let copy_text t = Dom_html.document##(createTextNode t##.data)

(* ie, ff3.6 and safari does not like setting innerHTML on html and
   head nodes: we need to rebuild the HTML dom tree from the XML dom
   tree received in the xhr *)

let copy_element
      (e : Dom.element Js.t)
      (registered_process_node : Js.js_string Js.t -> bool) :
  Dom_html.element Js.t
  =
  let rec aux (e : Dom.element Js.t) =
    let copy = Dom_html.document##(createElement e##.tagName) in
    let node_id =
      Js.Opt.to_option
        e##(getAttribute (Js.string Runtime.RawXML.node_id_attrib))
    in
    match node_id with
    | Some id when registered_process_node id ->
        Js.Opt.iter
          e##(getAttribute (Js.string "class"))
          (fun classes -> copy##(setAttribute (Js.string "class") classes));
        copy##(setAttribute (Js.string Runtime.RawXML.node_id_attrib) id);
        Some copy
    | _ ->
        let add_attribute a =
          Js.Opt.iter (Dom.CoerceTo.attr a)
            (* we don't use copy##attributes##setNameditem:
             in ie 9 it fail setting types of buttons... *)
            (fun a -> copy##(setAttribute a##.name a##.value))
        in
        iter_dom_array add_attribute e##.attributes;
        let child_copies =
          List.filter_map
            (fun child ->
               match Dom.nodeType child with
               | Dom.Text t -> Some (copy_text t :> Dom.node Js.t)
               | Dom.Element child -> (aux child :> Dom.node Js.t option)
               | _ -> None)
            (Dom.list_of_nodeList e##.childNodes)
        in
        List.iter (Dom.appendChild copy) child_copies;
        Some copy
  in
  match aux e with None -> raise_error ~section "copy_element" | Some e -> e

let html_document (src : Dom.element Dom.document Js.t) registered_process_node
  : Dom_html.element Js.t
  =
  let content = src##.documentElement in
  match Js.Opt.to_option (Dom_html.CoerceTo.element content) with
  | Some e -> (
    try Dom_html.document##(adoptNode (e :> Dom.element Js.t))
    with exn -> (
      Logs.debug ~src:section (fun fmt ->
        fmt "can't adopt node, import instead@\n%s" (Printexc.to_string exn));
      try Dom_html.document##(importNode (e :> Dom.element Js.t) Js._true)
      with exn ->
        Logs.debug ~src:section (fun fmt ->
          fmt "can't import node, copy instead@\n%s" (Printexc.to_string exn));
        copy_element content registered_process_node))
  | None ->
      Logs.debug ~src:section (fun fmt ->
        fmt "can't adopt node, document not parsed as html. copy instead");
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
       && e##._type == Js.string "text/css")

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
        if
          Js.to_bool e##.disabled || e##.title##.length > 0 || href##.length = 0
        then acc
        else
          let href = Js.to_string href in
          let css = Request.http_get href [] Request.string_result in
          acc @ [e, (e##.media, href, Lwt.map snd css)]
    | Dom.Element e ->
        let c = e##.childNodes in
        let acc = ref acc in
        for i = 0 to c##.length - 1 do
          acc := extract !acc (Js.Opt.get c##(item i) (fun _ -> assert false))
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
  Lwt.catch
    (fun () ->
       let* css = css in
       match css with
       | None -> Lwt.return_nil
       | Some css ->
           Config.debug_time ("rewrite_CSS: " ^ href);
           let* imports, css =
             rewrite_css_import ~max ~prefix:(basedir href) ~media css 0
           in
           Config.debug_time_end ("rewrite_CSS: " ^ href);
           Lwt.return (imports @ [media, css]))
    (fun _ -> Lwt.return [media, Printf.sprintf "@import url(%s);" href])

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
        let* import =
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
            let css = Request.http_get href [] Request.string_result in
            rewrite_css ~max:(max - 1) (media, href, Lwt.map snd css)
        and* imports, css =
          rewrite_css_import ~charset ~max ~prefix ~media css i
        in
        Lwt.return (import @ imports, css)
      with
      | Incorrect_url -> Lwt.return ([], rewrite_css_url ~prefix css pos)
      | exn ->
          Logs.info ~src:section (fun fmt ->
            fmt "Error while importing css@\n%s" (Printexc.to_string exn));
          Lwt.return ([], rewrite_css_url ~prefix css pos))

let max_preload_depth = ref 4

let build_style (e, css) =
  let* css = rewrite_css ~max:!max_preload_depth css in
  Lwt_list.map_p
    (fun (media, css) ->
       let style = Dom_html.createStyle Dom_html.document in
       style##._type := Js.string "text/css";
       style##.media := media;
       style##.innerHTML := Js.string css;
       Lwt.return (e, (style :> Dom.node Js.t)))
    css

let preload_css (doc : Dom_html.element Js.t) =
  Config.debug_time "preload_css (fetch+rewrite)";
  let* css = Lwt_list.map_p build_style (fetch_linked_css (get_head doc)) in
  let css = List.concat css in
  List.iter
    (fun (e, css) ->
       try Dom.replaceChild (get_head doc) css e
       with _ ->
         Logs.info
           ~src:
             (* Node was a unique node that has been removed...
                       in a perfect settings we won't have parsed it... *)
             section (fun fmt -> fmt "Unique CSS skipped..."))
    css;
  Config.debug_time_end "preload_css (fetch+rewrite)";
  Lwt.return_unit

(** Window scrolling *)

(* Correct scrolling information in Chromium are found
   Dom_html.document##body while on Firefox they are found on
   Dom_html.document##documentElement. *)

[@@@warning "-39"]

type position =
  {html_top : float; html_left : float; body_top : float; body_left : float}
[@@deriving json]

[@@@warning "+39"]

let top_position = {html_top = 0.; html_left = 0.; body_top = 0.; body_left = 0.}

let createDocumentScroll () =
  { html_top = Js.to_float Dom_html.document##.documentElement##.scrollTop
  ; html_left = Js.to_float Dom_html.document##.documentElement##.scrollLeft
  ; body_top = Js.to_float Dom_html.document##.body##.scrollTop
  ; body_left = Js.to_float Dom_html.document##.body##.scrollLeft }

(* With firefox, the scroll position is restored before to fire the
   popstate event. We maintain our own position. *)

let current_position = ref top_position

let _ =
  ignore
    (Dom.addEventListener Dom_html.document (Dom.Event.make "scroll")
       (Dom_html.handler (fun _event ->
          current_position := createDocumentScroll ();
          Js._false))
       Js._true
     : Dom_html.event_listener_id)

let getDocumentScroll () = !current_position

let setDocumentScroll pos =
  Dom_html.document##.documentElement##.scrollTop := Js.float pos.html_top;
  Dom_html.document##.documentElement##.scrollLeft := Js.float pos.html_left;
  Dom_html.document##.body##.scrollTop := Js.float pos.body_top;
  Dom_html.document##.body##.scrollLeft := Js.float pos.body_left;
  current_position := pos

(* UGLY HACK for Opera bug: Opera seem does not always take into
   account the content of the base element. If we touch it like that,
   it remember its presence... *)
let touch_base () =
  Js.Opt.iter
    (Js.Opt.bind
       Dom_html.document##(getElementById (Js.string Common_base.base_elt_id))
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
   * Request: in send_post_form
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
      Dom_html.window##(setInterval (Js.wrap_callback check)
                          (Js.float (0.2 *. 1000.)))
