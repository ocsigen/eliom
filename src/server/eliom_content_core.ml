
open Eliom_lib

(* This the core of [Eliom_content] without its dependencies to [Eliom_service] et al.
   Its name is not [Eliom_content_base] because this would suggest the sharing
   between server and client. *)

(*****************************************************************************)

module Xml = struct

  include RawXML

  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  and recontent =
    | RELazy of econtent Eliom_lazy.request
    | RE of econtent
  and elt = {
    elt : recontent;
    node_id : node_id;
    unwrapper_mark: Eliom_wrap.unwrapper;
  }

  let content e = match e.elt with
    | RE e -> e
    | RELazy e -> Eliom_lazy.force e

  let rcontent e = e.elt

  let get_node_id elt = elt.node_id

  let tyxml_unwrap_id = Eliom_wrap.id_of_int tyxml_unwrap_id_int

  let make elt =
    { elt = RE elt;
      node_id = NoId;
      unwrapper_mark = Eliom_wrap.create_unwrapper tyxml_unwrap_id; }

  let make_lazy elt =
    { elt = RELazy elt;
      node_id = NoId;
      unwrapper_mark = Eliom_wrap.create_unwrapper tyxml_unwrap_id; }

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?(a = []) name children =
    make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

  let event_handler_of_js id args =
    let closure_id = make_cryptographic_safe_string () in
    CE_registered_closure (closure_id, (id, args))
  let event_of_js = event_handler_of_js

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

  let make_node_name ?(global = true) () =
    (if global then "global_" else "")
    ^ "server_" ^ make_cryptographic_safe_string ()

  let make_process_node ?(id = make_node_name ~global:true ()) elt =
    { elt with node_id = ProcessId id }

  let make_request_node elt =
    { elt with
      node_id = RequestId (make_node_name ()) }

  (** Ref tree *)

  let cons_attrib att acc = match racontent att with
    | RACamlEventHandler (CE_registered_closure (closure_id, client_expr)) ->
      ClosureMap.add closure_id client_expr acc
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
    | _, RACamlEventHandler (CE_registered_closure (closure_id, _)) as attr ->
      (ce_registered_closure_class :: acc_class, attr :: acc_attr)
    | _, RACamlEventHandler (CE_call_service link_info) ->
      begin
	match Eliom_lazy.force link_info with
	  | None -> acc_class, acc_attr
	  | Some (kind,cookie_info,tmpl) ->
              ce_call_service_class::acc_class,
              let acc_attr =
	        match cookie_info with
                | None -> acc_attr
                | Some v ->
                    (ce_call_service_attrib, RA (AStr (Json.to_string<cookie_info> v)))
                    :: acc_attr
              in
              match tmpl with
              | None -> acc_attr
              | Some tmpl -> (ce_template_attrib, RA (AStr tmpl)) :: acc_attr
      end
    | attr -> (acc_class,attr::acc_attr)

  let filter_class_attribs node_id attribs =
    let node_id = match node_id with
      | NoId -> [],[]
      | ProcessId i -> [process_node_class], [node_id_attrib,RA (AStr i)]
      | RequestId i -> [request_node_class], [node_id_attrib,RA (AStr i)]
    in
    let (classes,attribs) =
      List.fold_left filter_class (node_id) attribs in
    match classes with
      | [] -> attribs
      | _ -> ("class",RA (AStrL(Space,classes)))::attribs

  let set_classes node_id = function
    | Empty
    | Comment _
    | EncodedPCDATA _
    | PCDATA _
    | Entity _ as e -> e
    | Leaf (ename, attribs) ->
      Leaf (ename, filter_class_attribs node_id attribs)
    | Node (ename, attribs, sons) ->
      Node (ename, filter_class_attribs node_id attribs, sons)

  let content e =
    let c = match e.elt with
      | RE e -> e
      | RELazy e -> Eliom_lazy.force e
    in
    set_classes e.node_id c

end

module Svg = struct

  module D = Svg_f.Make(struct
    include Xml

    let make elt = make_request_node (make elt)
    let make_lazy elt = make_request_node (make_lazy elt)

    let empty () = make Empty

    let comment c = make (Comment c)
    let pcdata d = make (PCDATA d)
    let encodedpcdata d = make (EncodedPCDATA d)
    let entity e = make (Entity e)

    let leaf ?(a = []) name =  make (Leaf (name, a))
    let node ?(a = []) name children = make (Node (name, a, children))
    let lazy_node ?(a = []) name children =
      make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

  end)

  module F = Svg_f.Make(Xml)

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)
    let new_elt_id: ?global:bool -> unit -> 'a id = Xml.make_node_name
    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))
    let create_global_elt elt =
      D.tot (Xml.make_process_node (D.toelt elt))
  end

  module Printer = Xml_print.Make_typed_simple(Xml)(F)
end

module Html5 = struct

  module D = struct

    include Html5_f.Make(struct
      include Xml

      let make elt = make_request_node (make elt)
      let make_lazy elt = make_request_node (make_lazy elt)

      let empty () = make Empty

      let comment c = make (Comment c)
      let pcdata d = make (PCDATA d)
      let encodedpcdata d = make (EncodedPCDATA d)
      let entity e = make (Entity e)

      let leaf ?(a = []) name =  make (Leaf (name, a))
      let node ?(a = []) name children = make (Node (name, a, children))
      let lazy_node ?(a = []) name children =
        make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

    end)(Svg.D)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (Xml.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

    let raw_a_onabort = a_onabort
    let raw_a_onafterprint = a_onafterprint
    let raw_a_onbeforeprint = a_onbeforeprint
    let raw_a_onbeforeunload = a_onbeforeunload
    let raw_a_onblur = a_onblur
    let raw_a_oncanplay = a_oncanplay
    let raw_a_oncanplaythrough = a_oncanplaythrough
    let raw_a_onchange = a_onchange
    let raw_a_onclick = a_onclick
    let raw_a_oncontextmenu = a_oncontextmenu
    let raw_a_ondblclick = a_ondblclick
    let raw_a_ondrag = a_ondrag
    let raw_a_ondragend = a_ondragend
    let raw_a_ondragenter = a_ondragenter
    let raw_a_ondragleave = a_ondragleave
    let raw_a_ondragover = a_ondragover
    let raw_a_ondragstart = a_ondragstart
    let raw_a_ondrop = a_ondrop
    let raw_a_ondurationchange = a_ondurationchange
    let raw_a_onemptied = a_onemptied
    let raw_a_onended = a_onended
    let raw_a_onerror = a_onerror
    let raw_a_onfocus = a_onfocus
    let raw_a_onformchange = a_onformchange
    let raw_a_onforminput = a_onforminput
    let raw_a_onhashchange = a_onhashchange
    let raw_a_oninput = a_oninput
    let raw_a_oninvalid = a_oninvalid
    let raw_a_onmousedown = a_onmousedown
    let raw_a_onmouseup = a_onmouseup
    let raw_a_onmouseover = a_onmouseover
    let raw_a_onmousemove = a_onmousemove
    let raw_a_onmouseout = a_onmouseout
    let raw_a_onmousewheel = a_onmousewheel
    let raw_a_onoffline = a_onoffline
    let raw_a_ononline = a_ononline
    let raw_a_onpause = a_onpause
    let raw_a_onplay = a_onplay
    let raw_a_onplaying = a_onplaying
    let raw_a_onpagehide = a_onpagehide
    let raw_a_onpageshow = a_onpageshow
    let raw_a_onpopstate = a_onpopstate
    let raw_a_onprogress = a_onprogress
    let raw_a_onratechange = a_onratechange
    let raw_a_onreadystatechange = a_onreadystatechange
    let raw_a_onredo = a_onredo
    let raw_a_onresize = a_onresize
    let raw_a_onscroll = a_onscroll
    let raw_a_onseeked = a_onseeked
    let raw_a_onseeking = a_onseeking
    let raw_a_onselect = a_onselect
    let raw_a_onshow = a_onshow
    let raw_a_onstalled = a_onstalled
    let raw_a_onstorage = a_onstorage
    let raw_a_onsubmit = a_onsubmit
    let raw_a_onsuspend = a_onsuspend
    let raw_a_ontimeupdate = a_ontimeupdate
    let raw_a_onundo = a_onundo
    let raw_a_onunload = a_onunload
    let raw_a_onvolumechange = a_onvolumechange
    let raw_a_onwaiting = a_onwaiting
    let raw_a_onkeypress = a_onkeypress
    let raw_a_onkeydown = a_onkeydown
    let raw_a_onkeyup = a_onkeyup
    let raw_a_onload = a_onload
    let raw_a_onloadeddata = a_onloadeddata
    let raw_a_onloadedmetadata = a_onloadedmetadata
    let raw_a_onloadstart = a_onloadstart
    let raw_a_onmessage = a_onmessage

    let a_onabort ev = raw_a_onabort (Xml.Caml ev)
    let a_onafterprint ev = raw_a_onafterprint (Xml.Caml ev)
    let a_onbeforeprint ev = raw_a_onbeforeprint (Xml.Caml ev)
    let a_onbeforeunload ev = raw_a_onbeforeunload (Xml.Caml ev)
    let a_onblur ev = raw_a_onblur (Xml.Caml ev)
    let a_oncanplay ev = raw_a_oncanplay (Xml.Caml ev)
    let a_oncanplaythrough ev = raw_a_oncanplaythrough (Xml.Caml ev)
    let a_onchange ev = raw_a_onchange (Xml.Caml ev)
    let a_onclick (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_onclick (Xml.Caml (Obj.magic ev)) (* Typed by the syntax extension. *)
    let a_oncontextmenu (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_oncontextmenu (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondblclick (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondblclick (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrag (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondrag (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragend (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondragend (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragenter (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondragenter (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragleave (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondragleave (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragover (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondragover (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragstart (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondragstart (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrop (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_ondrop (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondurationchange ev = raw_a_ondurationchange (Xml.Caml ev)
    let a_onemptied ev = raw_a_onemptied (Xml.Caml ev)
    let a_onended ev = raw_a_onended (Xml.Caml ev)
    let a_onerror ev = raw_a_onerror (Xml.Caml ev)
    let a_onfocus ev = raw_a_onfocus (Xml.Caml ev)
    let a_onformchange ev = raw_a_onformchange (Xml.Caml ev)
    let a_onforminput ev = raw_a_onforminput (Xml.Caml ev)
    let a_onhashchange ev = raw_a_onhashchange (Xml.Caml ev)
    let a_oninput ev = raw_a_oninput (Xml.Caml ev)
    let a_oninvalid ev = raw_a_oninvalid (Xml.Caml ev)
    let a_onmousedown (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_onmousedown (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseup (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_onmouseup (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseover (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_onmouseover (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousemove (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_onmousemove (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseout (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      raw_a_onmouseout (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousewheel ev = raw_a_onmousewheel (Xml.Caml ev)
    let a_onoffline ev = raw_a_onoffline (Xml.Caml ev)
    let a_ononline ev = raw_a_ononline (Xml.Caml ev)
    let a_onpause ev = raw_a_onpause (Xml.Caml ev)
    let a_onplay ev = raw_a_onplay (Xml.Caml ev)
    let a_onplaying ev = raw_a_onplaying (Xml.Caml ev)
    let a_onpagehide ev = raw_a_onpagehide (Xml.Caml ev)
    let a_onpageshow ev = raw_a_onpageshow (Xml.Caml ev)
    let a_onpopstate ev = raw_a_onpopstate (Xml.Caml ev)
    let a_onprogress ev = raw_a_onprogress (Xml.Caml ev)
    let a_onratechange ev = raw_a_onratechange (Xml.Caml ev)
    let a_onreadystatechange ev = raw_a_onreadystatechange (Xml.Caml ev)
    let a_onredo ev = raw_a_onredo (Xml.Caml ev)
    let a_onresize ev = raw_a_onresize (Xml.Caml ev)
    let a_onscroll ev = raw_a_onscroll (Xml.Caml ev)
    let a_onseeked ev = raw_a_onseeked (Xml.Caml ev)
    let a_onseeking ev = raw_a_onseeking (Xml.Caml ev)
    let a_onselect ev = raw_a_onselect (Xml.Caml ev)
    let a_onshow ev = raw_a_onshow (Xml.Caml ev)
    let a_onstalled ev = raw_a_onstalled (Xml.Caml ev)
    let a_onstorage ev = raw_a_onstorage (Xml.Caml ev)
    let a_onsubmit ev = raw_a_onsubmit (Xml.Caml ev)
    let a_onsuspend ev = raw_a_onsuspend (Xml.Caml ev)
    let a_ontimeupdate ev = raw_a_ontimeupdate (Xml.Caml ev)
    let a_onundo ev = raw_a_onundo (Xml.Caml ev)
    let a_onunload ev = raw_a_onunload (Xml.Caml ev)
    let a_onvolumechange ev = raw_a_onvolumechange (Xml.Caml ev)
    let a_onwaiting ev = raw_a_onwaiting (Xml.Caml ev)
    let a_onkeypress (ev : Dom_html.keyboardEvent Xml.caml_event_handler) =
      raw_a_onkeypress (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeydown (ev : Dom_html.keyboardEvent Xml.caml_event_handler) =
      raw_a_onkeydown (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeyup (ev : Dom_html.keyboardEvent Xml.caml_event_handler) =
      raw_a_onkeyup (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onload ev = raw_a_onload (Xml.Caml ev)
    let a_onloadeddata ev = raw_a_onloadeddata (Xml.Caml ev)
    let a_onloadedmetadata ev = raw_a_onloadedmetadata (Xml.Caml ev)
    let a_onloadstart ev = raw_a_onloadstart (Xml.Caml ev)
    let a_onmessage ev = raw_a_onmessage (Xml.Caml ev)

  end

  module F = struct
    include Html5_f.Make(Xml)(Svg.F)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (Xml.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

    let raw_a_onabort = a_onabort
    let raw_a_onafterprint = a_onafterprint
    let raw_a_onbeforeprint = a_onbeforeprint
    let raw_a_onbeforeunload = a_onbeforeunload
    let raw_a_onblur = a_onblur
    let raw_a_oncanplay = a_oncanplay
    let raw_a_oncanplaythrough = a_oncanplaythrough
    let raw_a_onchange = a_onchange
    let raw_a_onclick = a_onclick
    let raw_a_oncontextmenu = a_oncontextmenu
    let raw_a_ondblclick = a_ondblclick
    let raw_a_ondrag = a_ondrag
    let raw_a_ondragend = a_ondragend
    let raw_a_ondragenter = a_ondragenter
    let raw_a_ondragleave = a_ondragleave
    let raw_a_ondragover = a_ondragover
    let raw_a_ondragstart = a_ondragstart
    let raw_a_ondrop = a_ondrop
    let raw_a_ondurationchange = a_ondurationchange
    let raw_a_onemptied = a_onemptied
    let raw_a_onended = a_onended
    let raw_a_onerror = a_onerror
    let raw_a_onfocus = a_onfocus
    let raw_a_onformchange = a_onformchange
    let raw_a_onforminput = a_onforminput
    let raw_a_onhashchange = a_onhashchange
    let raw_a_oninput = a_oninput
    let raw_a_oninvalid = a_oninvalid
    let raw_a_onmousedown = a_onmousedown
    let raw_a_onmouseup = a_onmouseup
    let raw_a_onmouseover = a_onmouseover
    let raw_a_onmousemove = a_onmousemove
    let raw_a_onmouseout = a_onmouseout
    let raw_a_onmousewheel = a_onmousewheel
    let raw_a_onoffline = a_onoffline
    let raw_a_ononline = a_ononline
    let raw_a_onpause = a_onpause
    let raw_a_onplay = a_onplay
    let raw_a_onplaying = a_onplaying
    let raw_a_onpagehide = a_onpagehide
    let raw_a_onpageshow = a_onpageshow
    let raw_a_onpopstate = a_onpopstate
    let raw_a_onprogress = a_onprogress
    let raw_a_onratechange = a_onratechange
    let raw_a_onreadystatechange = a_onreadystatechange
    let raw_a_onredo = a_onredo
    let raw_a_onresize = a_onresize
    let raw_a_onscroll = a_onscroll
    let raw_a_onseeked = a_onseeked
    let raw_a_onseeking = a_onseeking
    let raw_a_onselect = a_onselect
    let raw_a_onshow = a_onshow
    let raw_a_onstalled = a_onstalled
    let raw_a_onstorage = a_onstorage
    let raw_a_onsubmit = a_onsubmit
    let raw_a_onsuspend = a_onsuspend
    let raw_a_ontimeupdate = a_ontimeupdate
    let raw_a_onundo = a_onundo
    let raw_a_onunload = a_onunload
    let raw_a_onvolumechange = a_onvolumechange
    let raw_a_onwaiting = a_onwaiting
    let raw_a_onkeypress = a_onkeypress
    let raw_a_onkeydown = a_onkeydown
    let raw_a_onkeyup = a_onkeyup
    let raw_a_onload = a_onload
    let raw_a_onloadeddata = a_onloadeddata
    let raw_a_onloadedmetadata = a_onloadedmetadata
    let raw_a_onloadstart = a_onloadstart
    let raw_a_onmessage = a_onmessage

    let a_onabort ev = a_onabort (Xml.Caml ev)
    let a_onafterprint ev = a_onafterprint (Xml.Caml ev)
    let a_onbeforeprint ev = a_onbeforeprint (Xml.Caml ev)
    let a_onbeforeunload ev = a_onbeforeunload (Xml.Caml ev)
    let a_onblur ev = a_onblur (Xml.Caml ev)
    let a_oncanplay ev = a_oncanplay (Xml.Caml ev)
    let a_oncanplaythrough ev = a_oncanplaythrough (Xml.Caml ev)
    let a_onchange ev = a_onchange (Xml.Caml ev)
    let a_onclick (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_onclick (Xml.Caml (Obj.magic ev)) (* Typed by the syntax extension. *)
    let a_oncontextmenu (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_oncontextmenu (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondblclick (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondblclick (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrag (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondrag (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragend (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondragend (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragenter (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondragenter (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragleave (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondragleave (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragover (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondragover (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragstart (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondragstart (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrop (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_ondrop (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondurationchange ev = a_ondurationchange (Xml.Caml ev)
    let a_onemptied ev = a_onemptied (Xml.Caml ev)
    let a_onended ev = a_onended (Xml.Caml ev)
    let a_onerror ev = a_onerror (Xml.Caml ev)
    let a_onfocus ev = a_onfocus (Xml.Caml ev)
    let a_onformchange ev = a_onformchange (Xml.Caml ev)
    let a_onforminput ev = a_onforminput (Xml.Caml ev)
    let a_onhashchange ev = a_onhashchange (Xml.Caml ev)
    let a_oninput ev = a_oninput (Xml.Caml ev)
    let a_oninvalid ev = a_oninvalid (Xml.Caml ev)
    let a_onmousedown (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_onmousedown (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseup (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_onmouseup (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseover (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_onmouseover (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousemove (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_onmousemove (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseout (ev : Dom_html.mouseEvent Xml.caml_event_handler) =
      a_onmouseout (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousewheel ev = a_onmousewheel (Xml.Caml ev)
    let a_onoffline ev = a_onoffline (Xml.Caml ev)
    let a_ononline ev = a_ononline (Xml.Caml ev)
    let a_onpause ev = a_onpause (Xml.Caml ev)
    let a_onplay ev = a_onplay (Xml.Caml ev)
    let a_onplaying ev = a_onplaying (Xml.Caml ev)
    let a_onpagehide ev = a_onpagehide (Xml.Caml ev)
    let a_onpageshow ev = a_onpageshow (Xml.Caml ev)
    let a_onpopstate ev = a_onpopstate (Xml.Caml ev)
    let a_onprogress ev = a_onprogress (Xml.Caml ev)
    let a_onratechange ev = a_onratechange (Xml.Caml ev)
    let a_onreadystatechange ev = a_onreadystatechange (Xml.Caml ev)
    let a_onredo ev = a_onredo (Xml.Caml ev)
    let a_onresize ev = a_onresize (Xml.Caml ev)
    let a_onscroll ev = a_onscroll (Xml.Caml ev)
    let a_onseeked ev = a_onseeked (Xml.Caml ev)
    let a_onseeking ev = a_onseeking (Xml.Caml ev)
    let a_onselect ev = a_onselect (Xml.Caml ev)
    let a_onshow ev = a_onshow (Xml.Caml ev)
    let a_onstalled ev = a_onstalled (Xml.Caml ev)
    let a_onstorage ev = a_onstorage (Xml.Caml ev)
    let a_onsubmit ev = a_onsubmit (Xml.Caml ev)
    let a_onsuspend ev = a_onsuspend (Xml.Caml ev)
    let a_ontimeupdate ev = a_ontimeupdate (Xml.Caml ev)
    let a_onundo ev = a_onundo (Xml.Caml ev)
    let a_onunload ev = a_onunload (Xml.Caml ev)
    let a_onvolumechange ev = a_onvolumechange (Xml.Caml ev)
    let a_onwaiting ev = a_onwaiting (Xml.Caml ev)
    let a_onkeypress (ev : Dom_html.keyboardEvent Xml.caml_event_handler) =
      a_onkeypress (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeydown (ev : Dom_html.keyboardEvent Xml.caml_event_handler) =
      a_onkeydown (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeyup (ev : Dom_html.keyboardEvent Xml.caml_event_handler) =
      a_onkeyup (Xml.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onload ev = a_onload (Xml.Caml ev)
    let a_onloadeddata ev = a_onloadeddata (Xml.Caml ev)
    let a_onloadedmetadata ev = a_onloadedmetadata (Xml.Caml ev)
    let a_onloadstart ev = a_onloadstart (Xml.Caml ev)
    let a_onmessage ev = a_onmessage (Xml.Caml ev)

  end

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)
    let new_elt_id: ?global:bool -> unit -> 'a id = Xml.make_node_name
    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))
    let create_global_elt elt =
      D.tot (Xml.make_process_node (D.toelt elt))
    let have_id name elt = Xml.get_node_id (D.toelt elt) = Xml.ProcessId name
  end

  module Printer = Xml_print.Make_typed_simple(Xml)(F)

end

module Xhtml = struct

  module F = struct

    include Xhtml_f.Make(Xml)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (Xml.lazy_node ~a:(Xml.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module F_01_00 = struct

    include Xhtml_f.Make_01_00(Xml)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (Xml.lazy_node ~a:(Xml.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module F_01_01 = struct

    include Xhtml_f.Make_01_01(Xml)

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (Xml.lazy_node ~a:(Xml.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module Printer = Xml_print.Make_typed_simple(Xml)(F)
  module Printer_01_01 = Xml_print.Make_typed_simple(Xml)(F_01_01)
  module Printer_01_00 = Xml_print.Make_typed_simple(Xml)(F_01_00)

end

