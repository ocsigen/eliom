
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

module X = Xml

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

    module Raw = Html5_f.Make(struct
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

    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (X.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

    let a_onabort ev = Raw.a_onabort (X.Caml ev)
    let a_onafterprint ev = Raw.a_onafterprint (X.Caml ev)
    let a_onbeforeprint ev = Raw.a_onbeforeprint (X.Caml ev)
    let a_onbeforeunload ev = Raw.a_onbeforeunload (X.Caml ev)
    let a_onblur ev = Raw.a_onblur (X.Caml ev)
    let a_oncanplay ev = Raw.a_oncanplay (X.Caml ev)
    let a_oncanplaythrough ev = Raw.a_oncanplaythrough (X.Caml ev)
    let a_onchange ev = Raw.a_onchange (X.Caml ev)
    let a_onclick (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_onclick (X.Caml (Obj.magic ev)) (* Typed by the syntax extension. *)
    let a_oncontextmenu (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_oncontextmenu (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondblclick (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondblclick (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrag (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondrag (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragend (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondragend (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragenter (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondragenter (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragleave (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondragleave (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragover (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondragover (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragstart (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondragstart (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrop (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_ondrop (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondurationchange ev = Raw.a_ondurationchange (X.Caml ev)
    let a_onemptied ev = Raw.a_onemptied (X.Caml ev)
    let a_onended ev = Raw.a_onended (X.Caml ev)
    let a_onerror ev = Raw.a_onerror (X.Caml ev)
    let a_onfocus ev = Raw.a_onfocus (X.Caml ev)
    let a_onformchange ev = Raw.a_onformchange (X.Caml ev)
    let a_onforminput ev = Raw.a_onforminput (X.Caml ev)
    let a_onhashchange ev = Raw.a_onhashchange (X.Caml ev)
    let a_oninput ev = Raw.a_oninput (X.Caml ev)
    let a_oninvalid ev = Raw.a_oninvalid (X.Caml ev)
    let a_onmousedown (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_onmousedown (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseup (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_onmouseup (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseover (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_onmouseover (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousemove (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_onmousemove (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseout (ev : Dom_html.mouseEvent X.caml_event_handler) =
      Raw.a_onmouseout (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousewheel ev = Raw.a_onmousewheel (X.Caml ev)
    let a_onoffline ev = Raw.a_onoffline (X.Caml ev)
    let a_ononline ev = Raw.a_ononline (X.Caml ev)
    let a_onpause ev = Raw.a_onpause (X.Caml ev)
    let a_onplay ev = Raw.a_onplay (X.Caml ev)
    let a_onplaying ev = Raw.a_onplaying (X.Caml ev)
    let a_onpagehide ev = Raw.a_onpagehide (X.Caml ev)
    let a_onpageshow ev = Raw.a_onpageshow (X.Caml ev)
    let a_onpopstate ev = Raw.a_onpopstate (X.Caml ev)
    let a_onprogress ev = Raw.a_onprogress (X.Caml ev)
    let a_onratechange ev = Raw.a_onratechange (X.Caml ev)
    let a_onreadystatechange ev = Raw.a_onreadystatechange (X.Caml ev)
    let a_onredo ev = Raw.a_onredo (X.Caml ev)
    let a_onresize ev = Raw.a_onresize (X.Caml ev)
    let a_onscroll ev = Raw.a_onscroll (X.Caml ev)
    let a_onseeked ev = Raw.a_onseeked (X.Caml ev)
    let a_onseeking ev = Raw.a_onseeking (X.Caml ev)
    let a_onselect ev = Raw.a_onselect (X.Caml ev)
    let a_onshow ev = Raw.a_onshow (X.Caml ev)
    let a_onstalled ev = Raw.a_onstalled (X.Caml ev)
    let a_onstorage ev = Raw.a_onstorage (X.Caml ev)
    let a_onsubmit ev = Raw.a_onsubmit (X.Caml ev)
    let a_onsuspend ev = Raw.a_onsuspend (X.Caml ev)
    let a_ontimeupdate ev = Raw.a_ontimeupdate (X.Caml ev)
    let a_onundo ev = Raw.a_onundo (X.Caml ev)
    let a_onunload ev = Raw.a_onunload (X.Caml ev)
    let a_onvolumechange ev = Raw.a_onvolumechange (X.Caml ev)
    let a_onwaiting ev = Raw.a_onwaiting (X.Caml ev)
    let a_onkeypress (ev : Dom_html.keyboardEvent X.caml_event_handler) =
      Raw.a_onkeypress (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeydown (ev : Dom_html.keyboardEvent X.caml_event_handler) =
      Raw.a_onkeydown (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeyup (ev : Dom_html.keyboardEvent X.caml_event_handler) =
      Raw.a_onkeyup (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onload ev = Raw.a_onload (X.Caml ev)
    let a_onloadeddata ev = Raw.a_onloadeddata (X.Caml ev)
    let a_onloadedmetadata ev = Raw.a_onloadedmetadata (X.Caml ev)
    let a_onloadstart ev = Raw.a_onloadstart (X.Caml ev)
    let a_onmessage ev = Raw.a_onmessage (X.Caml ev)

  end

  module F = struct

    module Raw = Html5_f.Make(Xml)(Svg.F)
    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (X.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

    let a_onabort ev = a_onabort (X.Caml ev)
    let a_onafterprint ev = a_onafterprint (X.Caml ev)
    let a_onbeforeprint ev = a_onbeforeprint (X.Caml ev)
    let a_onbeforeunload ev = a_onbeforeunload (X.Caml ev)
    let a_onblur ev = a_onblur (X.Caml ev)
    let a_oncanplay ev = a_oncanplay (X.Caml ev)
    let a_oncanplaythrough ev = a_oncanplaythrough (X.Caml ev)
    let a_onchange ev = a_onchange (X.Caml ev)
    let a_onclick (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_onclick (X.Caml (Obj.magic ev)) (* Typed by the syntax extension. *)
    let a_oncontextmenu (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_oncontextmenu (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondblclick (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondblclick (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrag (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondrag (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragend (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondragend (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragenter (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondragenter (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragleave (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondragleave (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragover (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondragover (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondragstart (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondragstart (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondrop (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_ondrop (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_ondurationchange ev = a_ondurationchange (X.Caml ev)
    let a_onemptied ev = a_onemptied (X.Caml ev)
    let a_onended ev = a_onended (X.Caml ev)
    let a_onerror ev = a_onerror (X.Caml ev)
    let a_onfocus ev = a_onfocus (X.Caml ev)
    let a_onformchange ev = a_onformchange (X.Caml ev)
    let a_onforminput ev = a_onforminput (X.Caml ev)
    let a_onhashchange ev = a_onhashchange (X.Caml ev)
    let a_oninput ev = a_oninput (X.Caml ev)
    let a_oninvalid ev = a_oninvalid (X.Caml ev)
    let a_onmousedown (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_onmousedown (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseup (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_onmouseup (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseover (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_onmouseover (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousemove (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_onmousemove (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmouseout (ev : Dom_html.mouseEvent X.caml_event_handler) =
      a_onmouseout (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onmousewheel ev = a_onmousewheel (X.Caml ev)
    let a_onoffline ev = a_onoffline (X.Caml ev)
    let a_ononline ev = a_ononline (X.Caml ev)
    let a_onpause ev = a_onpause (X.Caml ev)
    let a_onplay ev = a_onplay (X.Caml ev)
    let a_onplaying ev = a_onplaying (X.Caml ev)
    let a_onpagehide ev = a_onpagehide (X.Caml ev)
    let a_onpageshow ev = a_onpageshow (X.Caml ev)
    let a_onpopstate ev = a_onpopstate (X.Caml ev)
    let a_onprogress ev = a_onprogress (X.Caml ev)
    let a_onratechange ev = a_onratechange (X.Caml ev)
    let a_onreadystatechange ev = a_onreadystatechange (X.Caml ev)
    let a_onredo ev = a_onredo (X.Caml ev)
    let a_onresize ev = a_onresize (X.Caml ev)
    let a_onscroll ev = a_onscroll (X.Caml ev)
    let a_onseeked ev = a_onseeked (X.Caml ev)
    let a_onseeking ev = a_onseeking (X.Caml ev)
    let a_onselect ev = a_onselect (X.Caml ev)
    let a_onshow ev = a_onshow (X.Caml ev)
    let a_onstalled ev = a_onstalled (X.Caml ev)
    let a_onstorage ev = a_onstorage (X.Caml ev)
    let a_onsubmit ev = a_onsubmit (X.Caml ev)
    let a_onsuspend ev = a_onsuspend (X.Caml ev)
    let a_ontimeupdate ev = a_ontimeupdate (X.Caml ev)
    let a_onundo ev = a_onundo (X.Caml ev)
    let a_onunload ev = a_onunload (X.Caml ev)
    let a_onvolumechange ev = a_onvolumechange (X.Caml ev)
    let a_onwaiting ev = a_onwaiting (X.Caml ev)
    let a_onkeypress (ev : Dom_html.keyboardEvent X.caml_event_handler) =
      a_onkeypress (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeydown (ev : Dom_html.keyboardEvent X.caml_event_handler) =
      a_onkeydown (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onkeyup (ev : Dom_html.keyboardEvent X.caml_event_handler) =
      a_onkeyup (X.Caml (Obj.magic ev)) (* Typed with the syntax extension *)
    let a_onload ev = a_onload (X.Caml ev)
    let a_onloadeddata ev = a_onloadeddata (X.Caml ev)
    let a_onloadedmetadata ev = a_onloadedmetadata (X.Caml ev)
    let a_onloadstart ev = a_onloadstart (X.Caml ev)
    let a_onmessage ev = a_onmessage (X.Caml ev)

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

    module Raw = Xhtml_f.Make(Xml)
    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (X.lazy_node ~a:(Xml.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module F_01_00 = struct

    module Raw = Xhtml_f.Make_01_00(Xml)
    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (X.lazy_node ~a:(Xml.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module F_01_01 = struct

    module Raw = Xhtml_f.Make_01_01(Xml)
    include Raw

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ~action ?(a = []) elt elts =
      tot (X.lazy_node ~a:(Xml.uri_attrib "action" action :: to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
		(fun () ->
		  toelt (Eliom_lazy.force elt)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  module Printer = Xml_print.Make_typed_simple(Xml)(F)
  module Printer_01_01 = Xml_print.Make_typed_simple(Xml)(F_01_01)
  module Printer_01_00 = Xml_print.Make_typed_simple(Xml)(F_01_00)

end

