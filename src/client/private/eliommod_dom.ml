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

open Eliom_pervasives

class type ['element] get_tag = object
  method getElementsByTagName : Js.js_string Js.t -> 'element Dom.nodeList Js.t Js.meth
end

(* We can't use Dom_html.document##head: it is not defined in ff3.6... *)
let get_head (page:'element #get_tag Js.t) : 'element Js.t =
  Js.Opt.get
    ((page##getElementsByTagName(Js.string "head"))##item(0))
    (fun () -> error "get_head")

let get_body (page:'element #get_tag Js.t) : 'element Js.t =
  Js.Opt.get
    ((page##getElementsByTagName(Js.string "body"))##item(0))
    (fun () -> error "get_body")

let iter_dom_array (f:'a -> unit)
    (a:<length : <get : int; ..> Js.gen_prop; item : int -> 'a Js.opt Js.meth; ..> Js.t) =
  let length = a##length in
  for i = 0 to length - 1 do
    Js.Opt.iter (a##item(i)) f;
  done

let copy_text t = Dom_html.document##createTextNode(t##data)

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
let add_childrens (elt:Dom_html.element Js.t) (sons:Dom.node Js.t list) =
  try
    List.iter (Dom.appendChild elt) sons
  with
    | exn ->
      (* this code is ie only, there are no reason for an appendChild
	 to fail normally *)
      let concat l =
	let rec concat acc = function
	  | [] -> acc
	  | t::q ->
	    let txt =
	      match (Dom.nodeType t) with
		| Dom.Text t -> t
		| _ -> error "add_childrens: not text node in tag %s" (Js.to_string (elt##tagName)) in
	    concat (acc##concat(txt##data)) q
	in
	concat (Js.string "") l
      in
      match Dom_html.tagged elt with
	| Dom_html.Script elt ->
	  elt##text <- concat sons
	| Dom_html.Style elt ->
	  (* we need to append the style node to something. If we
	     don't do that the styleSheet field is not created if we.
	     And we can't do it by creating it with the ie specific
	     document.createStyleSheet: the styleSheet field is not
	     initialised and it can't be set either. *)
	  let d = Dom_html.createHead Dom_html.document in
	  Dom.appendChild d elt;
	  (Js.Unsafe.coerce elt)##styleSheet##cssText <- concat sons
	| _ -> debug_exn "add children: can't appendChild" exn; raise exn

(* END IE HACK *)

let copy_element (e:Dom.element Js.t) : Dom_html.element Js.t =
  let rec aux (e:Dom.element Js.t) =
    let copy = Dom_html.document##createElement(e##tagName) in
    let add_attribute a =
      Js.Opt.iter (Dom.CoerceTo.attr a)
        (* we don't use copy##attributes##setNameditem:
           in ie 9 it fail setting types of buttons... *)
        (fun a -> copy##setAttribute(a##name,a##value)) in
    iter_dom_array add_attribute (e##attributes);
    let child_copies = List.map_filter
      (fun child ->
        match Dom.nodeType child with
          | Dom.Text t ->
            Some (copy_text t:>Dom.node Js.t)
          | Dom.Element child ->
            (aux child:>Dom.node Js.t option)
          | _ ->
            None)
      (Dom.list_of_nodeList (e##childNodes)) in
    add_childrens copy child_copies;
    Some copy
  in
  match aux e with
    | None -> error "copy_element"
    | Some e -> e

let html_document (src:Dom.element Dom.document Js.t) : Dom_html.element Js.t =
  let content = src##documentElement in
  match Js.Opt.to_option (Dom_html.CoerceTo.element content) with
    | Some e ->
      begin
	try Dom_html.document##adoptNode((e:>Dom.element Js.t)) with
	  | exn ->
	    debug_exn "can't addopt node, import instead" exn;
	    try Dom_html.document##importNode((e:>Dom.element Js.t),Js._true) with
	      | exn ->
		debug_exn "can't import node, copy instead" exn;
		copy_element content
      end
    | None -> copy_element content

(** CSS preloading. *)

let is_stylesheet e =
  Js.to_string e##nodeName = "LINK"
  && List.exists ((=) "stylesheet")
       (String.split ' '
	  (Js.Opt.case (e##getAttribute (Js.string "rel"))
	     (fun () -> "")
	     Js.to_string))
  && (Js.Opt.case (e##getAttribute (Js.string "type"))
	     (fun () -> true)
	     (fun s -> Js.to_string s = "text/css"))

let basedir_re = Regexp.regexp "^(([^/?]*/)*)[^/?]*(\\?.*)?$"
let basedir path =
  match Regexp.string_match basedir_re path 0 with
  | None -> "/"
  | Some res ->
     match Regexp.matched_group res 1 with
     | None -> "/"
     | Some dir -> dir

(* let current_baseurl () = *)
  (* (Js.to_string Dom_html.window##location##protocol) ^ *)
  (* "//" ^ *)
  (* (Js.to_string Dom_html.window##location##host) ^ *)
  (* basedir (Js.to_string Dom_html.window##location##pathname) *)

let fetch_linked_css e =
  let rec extract acc (e : Dom.node Js.t) =
    match Dom.nodeType e with
    | Dom.Element e when is_stylesheet e ->
        let href     = e##getAttribute (Js.string "href")
        and disabled = e##getAttribute (Js.string "disabled")
        and title    = e##getAttribute (Js.string "title")
        and media    = e##getAttribute (Js.string "media") in
	if Js.Opt.test disabled || Js.Opt.test title
	then acc
	else
	  Js.Opt.case href
	    (fun () -> acc)
	    (fun href ->
	       let css =
		 Eliom_request.http_get (Js.to_string href) []
		   Eliom_request.string_result in
	       acc @ [e, (media, css)] )
    | Dom.Element e ->
        List.fold_left extract acc (Dom.list_of_nodeList e##childNodes)
    | _ -> acc in
  extract [] (e :> Dom.node Js.t)

let abs_url_re = Regexp.regexp "^('|\")?(https?://|/)"
let prefix_relative_url ~prefix url =
  let len = String.length url in
  let url =
    if len > 0 && (url.[0] = '\'' || url.[0] = '\"') then
      String.sub url 1 (len - 2)
    else url
  in
  match Regexp.string_match abs_url_re url 0 with
  | Some _ -> url
  | None -> prefix ^ url

let url_content_raw    = "([^\"'\\)]\\\\(\"|'|\\)))*"
let dbl_quoted_url_raw = "\"" ^ url_content_raw ^ "[^\\\\\"]*\""
let quoted_url_raw     =  "'" ^ url_content_raw ^ "[^\\\\']*'"
let url_re =
  Regexp.regexp (Printf.sprintf "url\\(\\s*(%s|%s|%s)\\s*\\)\\s*"
		   dbl_quoted_url_raw
		   quoted_url_raw
		   (url_content_raw ^ "[^\\\\\\)]*"))
let raw_url_re =
  Regexp.regexp (Printf.sprintf "\\s*(%s|%s)\\s*"
		   dbl_quoted_url_raw
		   quoted_url_raw)

exception Incorrect_url
let parse_url ~prefix css pos =
  match Regexp.search url_re css pos with
  | Some (i, res) when i = pos ->
      ( i + String.length (Regexp.matched_string res),
	match Regexp.matched_group res 1 with
	| Some href -> prefix_relative_url ~prefix href
	| None -> raise Incorrect_url )
  | _ ->
      match Regexp.search raw_url_re css pos with
      | Some (i, res) when i = pos ->
	  ( i + String.length (Regexp.matched_string res),
	    match Regexp.matched_group res 1 with
	    | Some href -> prefix_relative_url ~prefix href
	    | None -> raise Incorrect_url )
      | _ -> raise Incorrect_url

let parse_media css pos =
  let i =
    try String.index_from css pos ';'
    with Not_found -> String.length css
  in
  (i+1, String.sub css pos (i - pos))

let import_re = Regexp.regexp "@import\\s*"
let url_re = Regexp.regexp "url\\("

let rewrite_css_url ~prefix css pos =
  let len = String.length css - pos in
  let buf = Buffer.create (len + len / 2) in
  let rec rewrite pos =
    if pos < String.length css then
      match Regexp.search url_re css pos with
      | None ->
	  Buffer.add_substring buf css pos (String.length css - pos)
      | Some (i, res) ->
	  Buffer.add_substring buf css pos (i - pos);
	  try
	    let i, href = parse_url ~prefix css i in
	    Printf.bprintf buf "url('%s')%!" href;
	    rewrite i
	  with Incorrect_url ->
	    Buffer.add_substring buf css i (String.length css - i)
  in
  rewrite pos;
  Buffer.contents buf


let rec rewrite_css ~max (media, css) =
  try_lwt
    css >>= function
    | _, None -> Lwt.return []
    | href, Some css ->
	lwt imports, css  =
	  rewrite_css_import ~max ~prefix:(basedir href) ~media css 0
	in
	Lwt.return (imports @ [(media,  css)])
  with e ->
    debug "Exc1: %s" (Printexc.to_string e);
    Lwt.return []

and rewrite_css_import ?(charset = "") ~max ~prefix ~media css pos =
  match Regexp.search import_re css pos with
  | None ->
      (* No @import anymore, rewrite url. *)
      Lwt.return ([], rewrite_css_url ~prefix css pos)
  | Some (i, res) ->
      (* Found @import rule, try to preload. *)
      let init = String.sub css pos (i - pos) in
      let charset = if pos = 0 then init else charset in
      try
	let i = i + String.length (Regexp.matched_string res) in
	let i, href = parse_url ~prefix css i in
	let i, media' = parse_media css i in
	lwt import =
	  if max = 0 then
	    (* Maximum imbrication of @import reached, rewrite url. *)
	    Lwt.return [(media,
			 Printf.sprintf "@import url('%s') %s;\n" href media')] 
	  else if Js.Opt.test media && media' <> "" then
	    (* TODO combine media if possible...
	       in the mean time keep explicit import. *)
	    Lwt.return [(media,
	                 Printf.sprintf "@import url('%s') %s;\n" href media')]
	  else
	    let media =
	      if Js.Opt.test media then media
	      else if media' = "" then Js.null
	      else Js.some (Js.string media')
	    in
	    let css =
	      Eliom_request.http_get href [] Eliom_request.string_result in
	    rewrite_css ~max:(max-1) (media, css)
	and imports, css =
	  rewrite_css_import ~charset ~max ~prefix ~media css i in
	Lwt.return (import @ imports, css)
      with
      | Incorrect_url ->
	  Lwt.return ([], rewrite_css_url ~prefix css pos)
      | e ->
	  debug "Exc2: %s" (Printexc.to_string e);
	  Lwt.return ([], rewrite_css_url ~prefix css pos)

let max_preload_depth = ref 4

let build_style (e, css) =
  lwt css = rewrite_css ~max:!max_preload_depth css in
  lwt css =
    Lwt_list.map_p
      (fun (media, css) ->
	 let style = Dom_html.createStyle Dom_html.document in
	 style##_type <- Js.string "text/css";
	 Js.Opt.iter media (fun m -> style##media <- m);
	 style##innerHTML <- Js.string css;
	 Lwt.return (style :> Dom.node Js.t))
      css in
  (* Noscript is used to group style. It's ignored by the parser when
     scripting is enabled, but does not seems to be ignore when
     inserted as a DOM element. *)
  let node = Dom_html.createNoscript Dom_html.document in
  List.iter (Dom.appendChild node) css;
  Lwt.return (e, node)

let preload_css (doc : Dom_html.element Js.t) =
  lwt css = Lwt_list.map_p build_style (fetch_linked_css doc) in
  List.iter (fun (e, css) ->
	       Dom.replaceChild (get_head doc) css e) css;
  Lwt.return ()
