
include Eliom_pervasives_base

module Filename = Ocsigen_pervasives.Filename
module Printexc = Ocsigen_pervasives.Printexc

(*****************************************************************************)

module Url = struct

  include Ocsigen_pervasives.Url

  let remove_internal_slash u =
    let rec aux = function
      | [] -> []
      | [a] -> [a]
      | ""::l -> aux l
      | a::l -> a::(aux l)
    in match u with
    | [] -> []
    | a::l -> a::(aux l)

  let change_empty_list = function
    | [] -> [""] (* It is not possible to register an empty URL *)
    | l -> l
  let make_encoded_parameters = Netencoding.Url.mk_url_encoded_parameters

  let encode = Ocsigen_pervasives.Url.encode
  let decode = Ocsigen_pervasives.Url.decode

end

(*****************************************************************************)

module String = struct

  include Ocsigen_pervasives.String

  (* Cut a string to the next separator *)
  let basic_sep char s =
    try
      let seppos = String.index s char in
      ((String.sub s 0 seppos),
       (String.sub s (seppos+1)
          ((String.length s) - seppos - 1)))
    with Invalid_argument _ -> raise Not_found

  (* returns the index of the first difference between s1 and s2,
     starting from n and ending at last.
     returns (last + 1) if no difference is found.
   *)
  let rec first_diff s1 s2 n last =
    try
      if s1.[n] = s2.[n]
      then
	if n = last
	then last+1
	else first_diff s1 s2 (n+1) last
      else n
    with Invalid_argument _ -> n

  let may_append s1 ~sep = function
    | "" -> s1
    | s2 -> s1^sep^s2

  let may_concat s1 ~sep s2 = match s1, s2 with
  | _, "" -> s1
  | "", _ -> s2
  | _ -> String.concat sep [s1;s2]

  let make_cryptographic_safe = Ocsigen_pervasives.String.make_cryptographic_safe

end

(*****************************************************************************)

module List = struct
  include Ocsigen_pervasives.List
  include Eliom_pervasives_base.List_base
  let rec remove_all_assoc a = function
    | [] -> []
    | (b, _)::l when a = b -> remove_all_assoc a l
    | b::l -> b::(remove_all_assoc a l)
  let rec remove_first_if_any a = function
    |  [] -> []
    | b::l when a = b -> l
    | b::l -> b::(remove_first_if_any a l)
  let rec remove_first_if_any_q a = function
    |  [] -> []
    | b::l when a == b -> l
    | b::l -> b::(remove_first_if_any_q a l)
end

(*****************************************************************************)

module Ip_address = struct

  include Ocsigen_pervasives.Ip_address

  let network_of_ip ip mask4 (mask61, mask62) = match ip with
  | IPv4 a -> IPv4 (Int32.logand a mask4)
  | IPv6 (a, b) -> IPv6 (Int64.logand a mask61, Int64.logand b mask62)

  let inet6_addr_loopback =
    fst (parse (Unix.string_of_inet_addr Unix.inet6_addr_loopback))

end

(*****************************************************************************)

module Int = struct

  module Table = Map.Make(struct
    type t = int
    let compare = compare
  end)

end

(*****************************************************************************)

let to_json ?typ v =
  match typ with
    | Some typ -> Deriving_Json.to_string typ v
    | None -> assert false (* implemented only client side *)

let of_json ?typ s =
  match typ with
    | Some typ -> Deriving_Json.from_string typ s
    | None -> assert false (* implemented only client side *)

module XML = struct

  include Eliom_pervasives_base.RawXML

  let cdata s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n<![CDATA[\n"^
      (Netstring_pcre.global_replace
	 (Netstring_pcre.regexp_string "]]>") "" s)
      ^"\n]]>\n" in
    encodedpcdata s'

  let cdata_script s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n//<![CDATA[\n"^
      (Netstring_pcre.global_replace
	 (Netstring_pcre.regexp_string "]]>") "" s)
      ^"\n//]]>\n" in
    encodedpcdata s'

  let cdata_style s = (* GK *)
    (* For security reasons, we do not allow "]]>" inside CDATA
       (as this string is to be considered as the end of the cdata)
     *)
    let s' = "\n/* <![CDATA[ */\n"^
      (Netstring_pcre.global_replace
	 (Netstring_pcre.regexp_string "]]>") "" s)
      ^"\n/* ]]> */\n" in
    encodedpcdata s'

  let make_unique ?copy elt =
    let id = match copy with
      | Some copy -> copy.unique_id
      | None -> Some (String.make_cryptographic_safe ()) in
    { elt with unique_id = id }

  (** Ref tree *)

  let cons_attrib att acc = match racontent att with
    | RACamlEventHandler (CE_registered_closure (id, client_expr)) ->
      ClosureMap.add id client_expr acc
    | _ -> acc

  let make_event_handler_table elt =
    let rec aux closure_acc elt =
      let make attribs =
	List.fold_right cons_attrib attribs closure_acc
      in
      match content elt with
	| Empty | EncodedPCDATA _ | PCDATA _
	| Entity _ | Comment _  -> closure_acc
	| Leaf (_, attribs) -> make attribs
	| Node (_, attribs, elts) ->
	  List.fold_left aux (make attribs) elts
    in
    aux ClosureMap.empty elt

  let filter_class (acc_class,acc_attr) = function
    | "class", RA value ->
      begin
	match value with
	  | AStr v ->
	    (v::acc_class,acc_attr)
	  | AStrL (Space,v) ->
	    (v@acc_class,acc_attr)
	  | _ -> failwith "attribute class is not a string"
      end
    | _, RACamlEventHandler (CE_registered_closure _) as attr ->
      (ce_registered_closure_class::acc_class,attr::acc_attr)
    | _, RACamlEventHandler (CE_call_service link_info) ->
      begin
	match Eliom_lazy.force link_info with
	  | None -> acc_class, acc_attr
	  | Some (kind,cookie_info) ->
	    ce_call_service_class::acc_class,
	    match cookie_info with
	      | None -> acc_attr
	      | Some v ->
		(ce_call_service_attrib, RA (AStr (Json.to_string<cookie_info> v)))
		::acc_attr
      end
    | attr -> (acc_class,attr::acc_attr)

  let filter_class_attribs unique_id attribs =
    let unique_id = match unique_id with
      | None -> [],[]
      | Some i -> [unique_class], [unique_attrib,RA (AStr i)]
    in
    let (classes,attribs) =
      List.fold_left filter_class (unique_id) attribs in
    match classes with
      | [] -> attribs
      | _ -> ("class",RA (AStrL(Space,classes)))::attribs

  let set_classes unique_id = function
    | Empty
    | Comment _
    | EncodedPCDATA _
    | PCDATA _
    | Entity _ as e -> e
    | Leaf (ename, attribs) ->
      Leaf (ename, filter_class_attribs unique_id attribs)
    | Node (ename, attribs, sons) ->
      Node (ename, filter_class_attribs unique_id attribs, sons)

  let content e =
    let c = match e.elt with
      | RE e -> e
      | RELazy e -> Eliom_lazy.force e
    in
    set_classes e.unique_id c

end

module SVG = struct
  module M = struct
      include SVG_f.Make(XML)
      let unique ?copy elt =
	tot (XML.make_unique ?copy:(map_option toelt copy) (toelt elt))
  end
  module P = XML_print.MakeTypedSimple(XML)(M)
end

module HTML5 = struct
  module M = struct
    include HTML5_f.Make(XML)(SVG.M)

    let unique ?copy elt =
      tot (XML.make_unique ?copy:(map_option toelt copy) (toelt elt))

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (XML.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

    let raw_a_onabort = a_onabort
    let a_onabort ev = raw_a_onabort (XML.Caml ev)

    let raw_a_onafterprint = a_onafterprint
    let a_onafterprint ev = raw_a_onafterprint (XML.Caml ev)

    let raw_a_onbeforeprint = a_onbeforeprint
    let a_onbeforeprint ev = raw_a_onbeforeprint (XML.Caml ev)

    let raw_a_onbeforeunload = a_onbeforeunload
    let a_onbeforeunload ev = raw_a_onbeforeunload (XML.Caml ev)

    let raw_a_onblur = a_onblur
    let a_onblur ev = raw_a_onblur (XML.Caml ev)

    let raw_a_oncanplay = a_oncanplay
    let a_oncanplay ev = raw_a_oncanplay (XML.Caml ev)

    let raw_a_oncanplaythrough = a_oncanplaythrough
    let a_oncanplaythrough ev = raw_a_oncanplaythrough (XML.Caml ev)

    let raw_a_onchange = a_onchange
    let a_onchange ev = raw_a_onchange (XML.Caml ev)

    let raw_a_onclick = a_onclick
    let a_onclick (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_onclick (XML.Caml (Obj.magic ev)) (* Typed by the syntax extension. *)

    let raw_a_oncontextmenu = a_oncontextmenu
    let a_oncontextmenu (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_oncontextmenu (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondblclick = a_ondblclick
    let a_ondblclick (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondblclick (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondrag = a_ondrag
    let a_ondrag (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondrag (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondragend = a_ondragend
    let a_ondragend (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondragend (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondragenter = a_ondragenter
    let a_ondragenter (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondragenter (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondragleave = a_ondragleave
    let a_ondragleave (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondragleave (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondragover = a_ondragover
    let a_ondragover (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondragover (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondragstart = a_ondragstart
    let a_ondragstart (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondragstart (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondrop = a_ondrop
    let a_ondrop (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_ondrop (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_ondurationchange = a_ondurationchange
    let a_ondurationchange ev = raw_a_ondurationchange (XML.Caml ev)

    let raw_a_onemptied = a_onemptied
    let a_onemptied ev = raw_a_onemptied (XML.Caml ev)

    let raw_a_onended = a_onended
    let a_onended ev = raw_a_onended (XML.Caml ev)

    let raw_a_onerror = a_onerror
    let a_onerror ev = raw_a_onerror (XML.Caml ev)

    let raw_a_onfocus = a_onfocus
    let a_onfocus ev = raw_a_onfocus (XML.Caml ev)

    let raw_a_onformchange = a_onformchange
    let a_onformchange ev = raw_a_onformchange (XML.Caml ev)

    let raw_a_onforminput = a_onforminput
    let a_onforminput ev = raw_a_onforminput (XML.Caml ev)

    let raw_a_onhashchange = a_onhashchange
    let a_onhashchange ev = raw_a_onhashchange (XML.Caml ev)

    let raw_a_oninput = a_oninput
    let a_oninput ev = raw_a_oninput (XML.Caml ev)

    let raw_a_oninvalid = a_oninvalid
    let a_oninvalid ev = raw_a_oninvalid (XML.Caml ev)

    let raw_a_onmousedown = a_onmousedown
    let a_onmousedown (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_onmousedown (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onmouseup = a_onmouseup
    let a_onmouseup (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_onmouseup (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onmouseover = a_onmouseover
    let a_onmouseover (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_onmouseover (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onmousemove = a_onmousemove
    let a_onmousemove (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_onmousemove (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onmouseout = a_onmouseout
    let a_onmouseout (ev : Dom_html.mouseEvent XML.caml_event_handler) =
      raw_a_onmouseout (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onmousewheel = a_onmousewheel
    let a_onmousewheel ev = raw_a_onmousewheel (XML.Caml ev)

    let raw_a_onoffline = a_onoffline
    let a_onoffline ev = raw_a_onoffline (XML.Caml ev)

    let raw_a_ononline = a_ononline
    let a_ononline ev = raw_a_ononline (XML.Caml ev)

    let raw_a_onpause = a_onpause
    let a_onpause ev = raw_a_onpause (XML.Caml ev)

    let raw_a_onplay = a_onplay
    let a_onplay ev = raw_a_onplay (XML.Caml ev)

    let raw_a_onplaying = a_onplaying
    let a_onplaying ev = raw_a_onplaying (XML.Caml ev)

    let raw_a_onpagehide = a_onpagehide
    let a_onpagehide ev = raw_a_onpagehide (XML.Caml ev)

    let raw_a_onpageshow = a_onpageshow
    let a_onpageshow ev = raw_a_onpageshow (XML.Caml ev)

    let raw_a_onpopstate = a_onpopstate
    let a_onpopstate ev = raw_a_onpopstate (XML.Caml ev)

    let raw_a_onprogress = a_onprogress
    let a_onprogress ev = raw_a_onprogress (XML.Caml ev)

    let raw_a_onratechange = a_onratechange
    let a_onratechange ev = raw_a_onratechange (XML.Caml ev)

    let raw_a_onreadystatechange = a_onreadystatechange
    let a_onreadystatechange ev = raw_a_onreadystatechange (XML.Caml ev)

    let raw_a_onredo = a_onredo
    let a_onredo ev = raw_a_onredo (XML.Caml ev)

    let raw_a_onresize = a_onresize
    let a_onresize ev = raw_a_onresize (XML.Caml ev)

    let raw_a_onscroll = a_onscroll
    let a_onscroll ev = raw_a_onscroll (XML.Caml ev)

    let raw_a_onseeked = a_onseeked
    let a_onseeked ev = raw_a_onseeked (XML.Caml ev)

    let raw_a_onseeking = a_onseeking
    let a_onseeking ev = raw_a_onseeking (XML.Caml ev)

    let raw_a_onselect = a_onselect
    let a_onselect ev = raw_a_onselect (XML.Caml ev)

    let raw_a_onshow = a_onshow
    let a_onshow ev = raw_a_onshow (XML.Caml ev)

    let raw_a_onstalled = a_onstalled
    let a_onstalled ev = raw_a_onstalled (XML.Caml ev)

    let raw_a_onstorage = a_onstorage
    let a_onstorage ev = raw_a_onstorage (XML.Caml ev)

    let raw_a_onsubmit = a_onsubmit
    let a_onsubmit ev = raw_a_onsubmit (XML.Caml ev)

    let raw_a_onsuspend = a_onsuspend
    let a_onsuspend ev = raw_a_onsuspend (XML.Caml ev)

    let raw_a_ontimeupdate = a_ontimeupdate
    let a_ontimeupdate ev = raw_a_ontimeupdate (XML.Caml ev)

    let raw_a_onundo = a_onundo
    let a_onundo ev = raw_a_onundo (XML.Caml ev)

    let raw_a_onunload = a_onunload
    let a_onunload ev = raw_a_onunload (XML.Caml ev)

    let raw_a_onvolumechange = a_onvolumechange
    let a_onvolumechange ev = raw_a_onvolumechange (XML.Caml ev)

    let raw_a_onwaiting = a_onwaiting
    let a_onwaiting ev = raw_a_onwaiting (XML.Caml ev)

    let raw_a_onkeypress = a_onkeypress
    let a_onkeypress (ev : Dom_html.keyboardEvent XML.caml_event_handler) =
      raw_a_onkeypress (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onkeydown = a_onkeydown
    let a_onkeydown (ev : Dom_html.keyboardEvent XML.caml_event_handler) =
      raw_a_onkeydown (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onkeyup = a_onkeyup
    let a_onkeyup (ev : Dom_html.keyboardEvent XML.caml_event_handler) =
      raw_a_onkeyup (XML.Caml (Obj.magic ev)) (* Typed with the syntax extension *)

    let raw_a_onload = a_onload
    let a_onload ev = raw_a_onload (XML.Caml ev)

    let raw_a_onloadeddata = a_onloadeddata
    let a_onloadeddata ev = raw_a_onloadeddata (XML.Caml ev)

    let raw_a_onloadedmetadata = a_onloadedmetadata
    let a_onloadedmetadata ev = raw_a_onloadedmetadata (XML.Caml ev)

    let raw_a_onloadstart = a_onloadstart
    let a_onloadstart ev = raw_a_onloadstart (XML.Caml ev)

    let raw_a_onmessage = a_onmessage
    let a_onmessage ev = raw_a_onmessage (XML.Caml ev)

  end
  module P = XML_print.MakeTypedSimple(XML)(M)
end

module XHTML = struct

  module M = struct

    include XHTML_f.Make(XML)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (XML.lazy_node ~a:(XML.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module M_01_00 = struct

    include XHTML_f.Make_01_00(XML)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (XML.lazy_node ~a:(XML.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module M_01_01 = struct

    include XHTML_f.Make_01_01(XML)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (XML.lazy_node ~a:(XML.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module P = XML_print.MakeTypedSimple(XML)(M)
  module P_01_01 = XML_print.MakeTypedSimple(XML)(M_01_01)
  module P_01_00 = XML_print.MakeTypedSimple(XML)(M_01_00)

end

type file_info = Ocsigen_extensions.file_info


let debug f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f
