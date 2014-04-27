(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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


(* This the core of [Eliom_content] without its dependencies to [Eliom_service],
   [Eliom_client] et al.  Its name is not [Eliom_content_base] because this would
   suggest the sharing between server and client. *)

open Eliom_lib

type content_ns = [ `HTML5 | `SVG ]

module XmlNoWrap = struct

  include RawXML

  type econtent =
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  and node =
    | DomNode of Dom.node Js.t
    | TyXMLNode of econtent
    | ReactNode of elt React.signal
  and elt = {
    (* See Eliom_content.Html5.To_dom for the 'unwrap' function that convert
       the server's tree representation into the client one. *)
    mutable elt : node lazy_t;
    node_id : node_id;
  }

  let content e =
    match Lazy.force e.elt with
    | ReactNode _
    | DomNode _ -> assert false (* TODO *)
    | TyXMLNode elt -> elt
  let get_node e = Lazy.force e.elt
  let set_dom_node elt node = elt.elt <- Lazy.lazy_from_val (DomNode node)
  let get_node_id elt = elt.node_id

  let make ?(id = NoId) elt =
    { elt = Lazy.lazy_from_val (TyXMLNode elt); node_id = id; }
  let make_dom ?(id = NoId) node =
    { elt = Lazy.lazy_from_val (DomNode node); node_id = id; }
  let make_lazy ?(id = NoId) lazy_elt =
    let f () =
       let elt = Lazy.force lazy_elt in
       assert (elt.node_id = id);
       Lazy.force elt.elt
    in
    { node_id = id; elt = Lazy.lazy_from_fun f }
  let force_lazy { elt } = ignore (Lazy.force elt)

  let make_react ?(id = NoId) signal =
    {elt = Lazy.lazy_from_val (ReactNode signal); node_id = id; }

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?a name children = node ?a name (Eliom_lazy.force children)

  let event_handler_of_function ev =
    Caml (CE_client_closure (Obj.magic ev))

  let end_re = Regexp.regexp_string "]]>"

  let make_node_name =
    let node_id_counter = ref 0 in
    (fun ?(global = true) () ->
      incr node_id_counter;
      (if global then "global_" else "")
      ^ "client_" ^ (string_of_int !node_id_counter))

  let make_process_node ?(id = make_node_name ~global:true ()) elt =
    { elt with node_id = ProcessId id }

  let make_request_node elt =
    { elt with
      node_id = RequestId (make_node_name ()) }

  let cdata s =
    let s' =
      "\n<![CDATA[\n" ^ Regexp.global_replace end_re s "" ^ "\n]]>\n" in
    encodedpcdata s'

  let cdata_script s =
    let s' =
      "\n//<![CDATA[\n" ^ Regexp.global_replace end_re s "" ^ "\n//]]>\n" in
    encodedpcdata s'

  let cdata_style s =
    let s' =
      "\n/* <![CDATA[ */\n" ^ Regexp.global_replace end_re s "" ^ "\n/* ]]> */\n" in
    encodedpcdata s'

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

  let set_classes_of_elt elt =
     match Lazy.force elt.elt with
      | DomNode _ -> failwith "Eliom_content_core.set_classes_of_elt"
      | ReactNode _ -> failwith "Eliom_content_core.set_classes_of_elt"
      | TyXMLNode econtent ->
          { elt with elt = Lazy.lazy_from_val (TyXMLNode (set_classes elt.node_id econtent)) }

  let string_of_node_id = function
    | NoId -> "NoId"
    | ProcessId s -> "ProcessId "^s
    | RequestId s -> "RequestId "^s

end

module Xml = struct
  include XmlNoWrap
  type 'a wrap = 'a
end

module X = Xml


module Xml_w = struct
  type 'a t = 'a React.signal
  let return x = Lwt_react.S.return x
  let bind x = Lwt_react.S.bind x
  let fmap x = React.S.map x
  let fmap2 x = React.S.l2 x
  let fmap3 x = React.S.l3 x
  let fmap4 x = React.S.l4 x
  let fmap5 x = React.S.l5 x
end
module Xml_wed =
struct
  type 'a wrap = 'a Xml_w.t
  type uri = Xml.uri
  let string_of_uri = Xml.string_of_uri
  let uri_of_string = Xml.uri_of_string
  type aname = Xml.aname
  type event_handler = Xml.event_handler
  type attrib = Xml.attrib

  let float_attrib name s : attrib =
    name, Xml.RAReact (Xml_w.fmap (fun f -> Some (Xml.AFloat f)) s)
  let int_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AInt f)) s)
  let string_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AStr f)) s)
  let space_sep_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some(Xml.AStrL (Xml.Space,f))) s)
  let comma_sep_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AStrL (Xml.Comma,f))) s)
  let event_handler_attrib = Xml.event_handler_attrib
  let uri_attrib name value =
    name, Xml.RAReact (React.S.map
                         (fun f -> Some (Xml.AStr (Eliom_lazy.force f))) value)
  let uris_attrib name value =
    name,
    Xml.RAReact (React.S.map
                   (fun f -> Some (Xml.AStrL (Xml.Space,Eliom_lazy.force f)))
                   value)

  type elt = Xml.elt
  type ename = Xml.ename

  let empty = Xml.empty
  let comment = Xml.comment
  let pcdata s = Xml.make_react (React.S.map Xml.pcdata s)
  let encodedpcdata s = Xml.make_react (React.S.map Xml.encodedpcdata s)
  let entity = Xml.entity
  let leaf = Xml.leaf
  let node ?a name l = Xml.make_react (React.S.map (fun l -> Xml.node ?a name l) l)
  let cdata = Xml.cdata
  let cdata_script = Xml.cdata_script
  let cdata_style = Xml.cdata_style
end



module Svg = struct

  module D = struct

    module Raw = Svg_f.Make(struct
        include Xml

        let make elt = make_request_node (make elt)
        let make_lazy elt = make_request_node (make (Lazy.force elt))

        let empty () = make Empty

        let comment c = make (Comment c)
        let pcdata d = make (PCDATA d)
        let encodedpcdata d = make (EncodedPCDATA d)
        let entity e = make (Entity e)

        let leaf ?(a = []) name =  make (Leaf (name, a))
        let node ?(a = []) name children = make (Node (name, a, children))
        let lazy_node ?(a = []) name children =
          make (Node (name, a, Eliom_lazy.force children))

      end)

    include Raw

    let a_onabort ev = a_onabort (X.event_handler_of_function ev)
    let a_onclick ev = a_onclick (X.event_handler_of_function ev)
    let a_onmousedown ev = a_onmousedown (X.event_handler_of_function ev)
    let a_onmouseup ev = a_onmouseup (X.event_handler_of_function ev)
    let a_onmouseover ev = a_onmouseover (X.event_handler_of_function ev)
    let a_onmousemove ev = a_onmousemove (X.event_handler_of_function ev)
    let a_onmouseout ev = a_onmouseout (X.event_handler_of_function ev)
    let a_onscroll ev = a_onscroll (X.event_handler_of_function ev)
    let a_onload ev = a_onload (X.event_handler_of_function ev)
    let a_onresize ev = a_onresize (X.event_handler_of_function ev)

  end

  module F = struct

    module Raw = Svg_f.Make(Xml)

    include Raw

    let a_onabort ev = a_onabort (X.event_handler_of_function ev)
    let a_onclick ev = a_onclick (X.event_handler_of_function ev)
    let a_onmousedown ev = a_onmousedown (X.event_handler_of_function ev)
    let a_onmouseup ev = a_onmouseup (X.event_handler_of_function ev)
    let a_onmouseover ev = a_onmouseover (X.event_handler_of_function ev)
    let a_onmousemove ev = a_onmousemove (X.event_handler_of_function ev)
    let a_onmouseout ev = a_onmouseout (X.event_handler_of_function ev)
    let a_onresize ev = a_onresize (X.event_handler_of_function ev)
    let a_onscroll ev = a_onscroll (X.event_handler_of_function ev)
    let a_onload ev = a_onload (X.event_handler_of_function ev)

  end

  module R = struct
    module Raw = Svg_f.MakeWrapped(Xml_w)(Xml_wed)
    include Raw

    let a_onabort ev = a_onabort (X.event_handler_of_function ev)
    let a_onclick ev = a_onclick (X.event_handler_of_function ev)
    let a_onmousedown ev = a_onmousedown (X.event_handler_of_function ev)
    let a_onmouseup ev = a_onmouseup (X.event_handler_of_function ev)
    let a_onmouseover ev = a_onmouseover (X.event_handler_of_function ev)
    let a_onmousemove ev = a_onmousemove (X.event_handler_of_function ev)
    let a_onmouseout ev = a_onmouseout (X.event_handler_of_function ev)
    let a_onresize ev = a_onresize (X.event_handler_of_function ev)
    let a_onscroll ev = a_onscroll (X.event_handler_of_function ev)
    let a_onload ev = a_onload (X.event_handler_of_function ev)

  end

  type 'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type 'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)
    let new_elt_id: ?global:bool -> unit -> 'a id = Xml.make_node_name
    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))
    let create_global_elt elt =
      D.tot (Xml.make_process_node (D.toelt elt))
    let string_of_id x = x
  end


  module Of_dom = struct
    let rebuild_xml (node: 'a Js.t) : 'a F.elt =
      Obj.magic { Xml.elt = Lazy.lazy_from_val (Xml.DomNode (node :> Dom.node Js.t)); node_id = Xml.NoId }
    let of_element : Dom_html.element Js.t -> 'a elt = rebuild_xml
  end


end

module Html5 = struct

  module D = struct

    module Raw = Html5_f.Make(struct
      include Xml

      let make elt = make_request_node (make elt)
      let make_lazy elt = make_request_node (make (Lazy.force elt))

      let empty () = make Empty

      let comment c = make (Comment c)
      let pcdata d = make (PCDATA d)
      let encodedpcdata d = make (EncodedPCDATA d)
      let entity e = make (Entity e)

      let leaf ?(a = []) name =  make (Leaf (name, a))
      let node ?(a = []) name children = make (Node (name, a, children))
      let lazy_node ?(a = []) name children =
        make (Node (name, a, Eliom_lazy.force children))

    end)(Svg.D.Raw)

    include Raw

    let a_onabort ev = a_onabort (X.event_handler_of_function ev)
    let a_onafterprint ev = a_onafterprint (X.event_handler_of_function ev)
    let a_onbeforeprint ev = a_onbeforeprint (X.event_handler_of_function ev)
    let a_onbeforeunload ev = a_onbeforeunload (X.event_handler_of_function ev)
    let a_onblur ev = a_onblur (X.event_handler_of_function ev)
    let a_oncanplay ev = a_oncanplay (X.event_handler_of_function ev)
    let a_oncanplaythrough ev = a_oncanplaythrough (X.event_handler_of_function ev)
    let a_onchange ev = a_onchange (X.event_handler_of_function ev)
    let a_onclick ev = a_onclick (X.event_handler_of_function ev)
    let a_oncontextmenu ev = a_oncontextmenu (X.event_handler_of_function ev)
    let a_ondblclick ev = a_ondblclick (X.event_handler_of_function ev)
    let a_ondrag ev = a_ondrag (X.event_handler_of_function ev)
    let a_ondragend ev = a_ondragend (X.event_handler_of_function ev)
    let a_ondragenter ev = a_ondragenter (X.event_handler_of_function ev)
    let a_ondragleave ev = a_ondragleave (X.event_handler_of_function ev)
    let a_ondragover ev = a_ondragover (X.event_handler_of_function ev)
    let a_ondragstart ev = a_ondragstart (X.event_handler_of_function ev)
    let a_ondrop ev = a_ondrop (X.event_handler_of_function ev)
    let a_ondurationchange ev = a_ondurationchange (X.event_handler_of_function ev)
    let a_onemptied ev = a_onemptied (X.event_handler_of_function ev)
    let a_onended ev = a_onended (X.event_handler_of_function ev)
    let a_onerror ev = a_onerror (X.event_handler_of_function ev)
    let a_onfocus ev = a_onfocus (X.event_handler_of_function ev)
    let a_onformchange ev = a_onformchange (X.event_handler_of_function ev)
    let a_onforminput ev = a_onforminput (X.event_handler_of_function ev)
    let a_onhashchange ev = a_onhashchange (X.event_handler_of_function ev)
    let a_oninput ev = a_oninput (X.event_handler_of_function ev)
    let a_oninvalid ev = a_oninvalid (X.event_handler_of_function ev)
    let a_onmousedown ev = a_onmousedown (X.event_handler_of_function ev)
    let a_onmouseup ev = a_onmouseup (X.event_handler_of_function ev)
    let a_onmouseover ev = a_onmouseover (X.event_handler_of_function ev)
    let a_onmousemove ev = a_onmousemove (X.event_handler_of_function ev)
    let a_onmouseout ev = a_onmouseout (X.event_handler_of_function ev)
    let a_onmousewheel ev = a_onmousewheel (X.event_handler_of_function ev)
    let a_onoffline ev = a_onoffline (X.event_handler_of_function ev)
    let a_ononline ev = a_ononline (X.event_handler_of_function ev)
    let a_onpause ev = a_onpause (X.event_handler_of_function ev)
    let a_onplay ev = a_onplay (X.event_handler_of_function ev)
    let a_onplaying ev = a_onplaying (X.event_handler_of_function ev)
    let a_onpagehide ev = a_onpagehide (X.event_handler_of_function ev)
    let a_onpageshow ev = a_onpageshow (X.event_handler_of_function ev)
    let a_onpopstate ev = a_onpopstate (X.event_handler_of_function ev)
    let a_onprogress ev = a_onprogress (X.event_handler_of_function ev)
    let a_onratechange ev = a_onratechange (X.event_handler_of_function ev)
    let a_onreadystatechange ev = a_onreadystatechange (X.event_handler_of_function ev)
    let a_onredo ev = a_onredo (X.event_handler_of_function ev)
    let a_onresize ev = a_onresize (X.event_handler_of_function ev)
    let a_onscroll ev = a_onscroll (X.event_handler_of_function ev)
    let a_onseeked ev = a_onseeked (X.event_handler_of_function ev)
    let a_onseeking ev = a_onseeking (X.event_handler_of_function ev)
    let a_onselect ev = a_onselect (X.event_handler_of_function ev)
    let a_onshow ev = a_onshow (X.event_handler_of_function ev)
    let a_onstalled ev = a_onstalled (X.event_handler_of_function ev)
    let a_onstorage ev = a_onstorage (X.event_handler_of_function ev)
    let a_onsubmit ev = a_onsubmit (X.event_handler_of_function ev)
    let a_onsuspend ev = a_onsuspend (X.event_handler_of_function ev)
    let a_ontimeupdate ev = a_ontimeupdate (X.event_handler_of_function ev)
    let a_onundo ev = a_onundo (X.event_handler_of_function ev)
    let a_onunload ev = a_onunload (X.event_handler_of_function ev)
    let a_onvolumechange ev = a_onvolumechange (X.event_handler_of_function ev)
    let a_onwaiting ev = a_onwaiting (X.event_handler_of_function ev)
    let a_onkeypress ev = a_onkeypress (X.event_handler_of_function ev)
    let a_onkeydown ev = a_onkeydown (X.event_handler_of_function ev)
    let a_onkeyup ev = a_onkeyup (X.event_handler_of_function ev)
    let a_onload ev = a_onload (X.event_handler_of_function ev)
    let a_onloadeddata ev = a_onloadeddata (X.event_handler_of_function ev)
    let a_onloadedmetadata ev = a_onloadedmetadata (X.event_handler_of_function ev)
    let a_onloadstart ev = a_onloadstart (X.event_handler_of_function ev)
    let a_onmessage ev = a_onmessage (X.event_handler_of_function ev)

    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (X.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
	        (fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

  end



  module R = struct

    let node s = Xml.make_react s

    module Raw = Html5_f.MakeWrapped(Xml_w)(Xml_wed)(Svg.R)
    include Raw
  end

  module F = struct

    module Raw = Html5_f.Make(Xml)(Svg.F.Raw)
    include Raw

    let a_onabort ev = a_onabort (X.event_handler_of_function ev)
    let a_onafterprint ev = a_onafterprint (X.event_handler_of_function ev)
    let a_onbeforeprint ev = a_onbeforeprint (X.event_handler_of_function ev)
    let a_onbeforeunload ev = a_onbeforeunload (X.event_handler_of_function ev)
    let a_onblur ev = a_onblur (X.event_handler_of_function ev)
    let a_oncanplay ev = a_oncanplay (X.event_handler_of_function ev)
    let a_oncanplaythrough ev = a_oncanplaythrough (X.event_handler_of_function ev)
    let a_onchange ev = a_onchange (X.event_handler_of_function ev)
    let a_onclick ev = a_onclick (X.event_handler_of_function ev)
    let a_oncontextmenu ev = a_oncontextmenu (X.event_handler_of_function ev)
    let a_ondblclick ev = a_ondblclick (X.event_handler_of_function ev)
    let a_ondrag ev = a_ondrag (X.event_handler_of_function ev)
    let a_ondragend ev = a_ondragend (X.event_handler_of_function ev)
    let a_ondragenter ev = a_ondragenter (X.event_handler_of_function ev)
    let a_ondragleave ev = a_ondragleave (X.event_handler_of_function ev)
    let a_ondragover ev = a_ondragover (X.event_handler_of_function ev)
    let a_ondragstart ev = a_ondragstart (X.event_handler_of_function ev)
    let a_ondrop ev = a_ondrop (X.event_handler_of_function ev)
    let a_ondurationchange ev = a_ondurationchange (X.event_handler_of_function ev)
    let a_onemptied ev = a_onemptied (X.event_handler_of_function ev)
    let a_onended ev = a_onended (X.event_handler_of_function ev)
    let a_onerror ev = a_onerror (X.event_handler_of_function ev)
    let a_onfocus ev = a_onfocus (X.event_handler_of_function ev)
    let a_onformchange ev = a_onformchange (X.event_handler_of_function ev)
    let a_onforminput ev = a_onforminput (X.event_handler_of_function ev)
    let a_onhashchange ev = a_onhashchange (X.event_handler_of_function ev)
    let a_oninput ev = a_oninput (X.event_handler_of_function ev)
    let a_oninvalid ev = a_oninvalid (X.event_handler_of_function ev)
    let a_onmousedown ev = a_onmousedown (X.event_handler_of_function ev)
    let a_onmouseup ev = a_onmouseup (X.event_handler_of_function ev)
    let a_onmouseover ev = a_onmouseover (X.event_handler_of_function ev)
    let a_onmousemove ev = a_onmousemove (X.event_handler_of_function ev)
    let a_onmouseout ev = a_onmouseout (X.event_handler_of_function ev)
    let a_onmousewheel ev = a_onmousewheel (X.event_handler_of_function ev)
    let a_onoffline ev = a_onoffline (X.event_handler_of_function ev)
    let a_ononline ev = a_ononline (X.event_handler_of_function ev)
    let a_onpause ev = a_onpause (X.event_handler_of_function ev)
    let a_onplay ev = a_onplay (X.event_handler_of_function ev)
    let a_onplaying ev = a_onplaying (X.event_handler_of_function ev)
    let a_onpagehide ev = a_onpagehide (X.event_handler_of_function ev)
    let a_onpageshow ev = a_onpageshow (X.event_handler_of_function ev)
    let a_onpopstate ev = a_onpopstate (X.event_handler_of_function ev)
    let a_onprogress ev = a_onprogress (X.event_handler_of_function ev)
    let a_onratechange ev = a_onratechange (X.event_handler_of_function ev)
    let a_onreadystatechange ev = a_onreadystatechange (X.event_handler_of_function ev)
    let a_onredo ev = a_onredo (X.event_handler_of_function ev)
    let a_onresize ev = a_onresize (X.event_handler_of_function ev)
    let a_onscroll ev = a_onscroll (X.event_handler_of_function ev)
    let a_onseeked ev = a_onseeked (X.event_handler_of_function ev)
    let a_onseeking ev = a_onseeking (X.event_handler_of_function ev)
    let a_onselect ev = a_onselect (X.event_handler_of_function ev)
    let a_onshow ev = a_onshow (X.event_handler_of_function ev)
    let a_onstalled ev = a_onstalled (X.event_handler_of_function ev)
    let a_onstorage ev = a_onstorage (X.event_handler_of_function ev)
    let a_onsubmit ev = a_onsubmit (X.event_handler_of_function ev)
    let a_onsuspend ev = a_onsuspend (X.event_handler_of_function ev)
    let a_ontimeupdate ev = a_ontimeupdate (X.event_handler_of_function ev)
    let a_onundo ev = a_onundo (X.event_handler_of_function ev)
    let a_onunload ev = a_onunload (X.event_handler_of_function ev)
    let a_onvolumechange ev = a_onvolumechange (X.event_handler_of_function ev)
    let a_onwaiting ev = a_onwaiting (X.event_handler_of_function ev)
    let a_onkeypress ev = a_onkeypress (X.event_handler_of_function ev)
    let a_onkeydown ev = a_onkeydown (X.event_handler_of_function ev)
    let a_onkeyup ev = a_onkeyup (X.event_handler_of_function ev)
    let a_onload ev = a_onload (X.event_handler_of_function ev)
    let a_onloadeddata ev = a_onloadeddata (X.event_handler_of_function ev)
    let a_onloadedmetadata ev = a_onloadedmetadata (X.event_handler_of_function ev)
    let a_onloadstart ev = a_onloadstart (X.event_handler_of_function ev)
    let a_onmessage ev = a_onmessage (X.event_handler_of_function ev)

    type ('a, 'b, 'c) lazy_plus =
        ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elt1 elts =
      tot (X.lazy_node ~a:(to_xmlattribs a) "form"
	     (Eliom_lazy.from_fun
	        (fun () ->
		  toelt (Eliom_lazy.force elt1)
		  :: toeltl (Eliom_lazy.force elts))))

  end

  type +'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)
    let new_elt_id: ?global:bool -> unit -> 'a id = Xml.make_node_name
    let new_global_elt_id () = new_elt_id ()
    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))
    let create_global_elt elt =
      D.tot (Xml.make_process_node (D.toelt elt))

    let string_of_id x = x
  end

  module Custom_data = struct

    type 'a t = {
      name : string;
      to_string : 'a -> string;
      of_string : string -> 'a;
      default : 'a option;
    }

    let create ~name ?default ~to_string ~of_string () =
      { name ; of_string ; to_string; default }

    let create_json ~name ?default typ =
      { name ; of_string = of_json ~typ ; to_string = to_json ~typ; default }

    let attrib custom_data value =
      F.a_user_data
        custom_data.name
        (custom_data.to_string value)

    let attribute_name name =
      "data-"^name

    let get_dom (element : Dom_html.element Js.t) custom_data =
      Js.Opt.case
        (element##getAttribute(Js.string (attribute_name custom_data.name)))
        (fun () ->
           match custom_data.default with
             | Some value -> value
             | None -> raise Not_found)
        (fun str -> custom_data.of_string (Js.to_string str))

    let set_dom element custom_data value =
      element##setAttribute(Js.string (attribute_name custom_data.name),
                            Js.string (custom_data.to_string value))

  end

  module Of_dom = struct
    let rebuild_xml (node: 'a Js.t) : 'a F.elt =
      Obj.magic { Xml.elt = Lazy.lazy_from_val (Xml.DomNode (node :> Dom.node Js.t)); node_id = Xml.NoId }
    let of_element : Dom_html.element Js.t -> 'a elt = rebuild_xml
    let of_html : Dom_html.htmlElement Js.t -> Html5_types.html elt = rebuild_xml
    let of_head : Dom_html.headElement Js.t -> Html5_types.head elt = rebuild_xml
    let of_link : Dom_html.linkElement Js.t -> Html5_types.link elt = rebuild_xml
    let of_title : Dom_html.titleElement Js.t -> Html5_types.title elt = rebuild_xml
    let of_meta : Dom_html.metaElement Js.t -> Html5_types.meta elt = rebuild_xml
    let of_base : Dom_html.baseElement Js.t -> Html5_types.base elt = rebuild_xml
    let of_style : Dom_html.styleElement Js.t -> Html5_types.style elt = rebuild_xml
    let of_body : Dom_html.bodyElement Js.t -> Html5_types.body elt = rebuild_xml
    let of_form : Dom_html.formElement Js.t -> Html5_types.form elt = rebuild_xml
    let of_optGroup : Dom_html.optGroupElement Js.t -> Html5_types.optgroup elt = rebuild_xml
    let of_option : Dom_html.optionElement Js.t -> Html5_types.selectoption elt = rebuild_xml
    let of_select : Dom_html.selectElement Js.t -> Html5_types.select elt = rebuild_xml
    let of_input : Dom_html.inputElement Js.t -> Html5_types.input elt = rebuild_xml
    let of_textArea : Dom_html.textAreaElement Js.t -> Html5_types.textarea elt = rebuild_xml
    let of_button : Dom_html.buttonElement Js.t -> Html5_types.button elt = rebuild_xml
    let of_label : Dom_html.labelElement Js.t -> Html5_types.label elt = rebuild_xml
    let of_fieldSet : Dom_html.fieldSetElement Js.t -> Html5_types.fieldset elt = rebuild_xml
    let of_legend : Dom_html.legendElement Js.t -> Html5_types.legend elt = rebuild_xml
    let of_uList : Dom_html.uListElement Js.t -> Html5_types.ul elt = rebuild_xml
    let of_oList : Dom_html.oListElement Js.t -> Html5_types.ol elt = rebuild_xml
    let of_dList : Dom_html.dListElement Js.t -> [`Dl] elt = rebuild_xml
    let of_li : Dom_html.liElement Js.t -> Html5_types.li elt = rebuild_xml
    let of_div : Dom_html.divElement Js.t -> Html5_types.div elt = rebuild_xml
    let of_paragraph : Dom_html.paragraphElement Js.t -> Html5_types.p elt = rebuild_xml
    let of_heading : Dom_html.headingElement Js.t -> Html5_types.heading elt = rebuild_xml
    let of_quote : Dom_html.quoteElement Js.t -> Html5_types.blockquote elt = rebuild_xml
    let of_pre : Dom_html.preElement Js.t -> Html5_types.pre elt = rebuild_xml
    let of_br : Dom_html.brElement Js.t -> Html5_types.br elt = rebuild_xml
    let of_hr : Dom_html.hrElement Js.t -> Html5_types.hr elt = rebuild_xml
    let of_anchor : Dom_html.anchorElement Js.t -> 'a Html5_types.a elt = rebuild_xml
    let of_image : Dom_html.imageElement Js.t -> [`Img] elt = rebuild_xml
    let of_object : Dom_html.objectElement Js.t -> 'a Html5_types.object_ elt = rebuild_xml
    let of_param : Dom_html.paramElement Js.t -> Html5_types.param elt = rebuild_xml
    let of_area : Dom_html.areaElement Js.t -> Html5_types.area elt = rebuild_xml
    let of_map : Dom_html.mapElement Js.t -> 'a Html5_types.map elt = rebuild_xml
    let of_script : Dom_html.scriptElement Js.t -> Html5_types.script elt = rebuild_xml
    let of_tableCell : Dom_html.tableCellElement Js.t -> [ Html5_types.td | Html5_types.td ] elt = rebuild_xml
    let of_tableRow : Dom_html.tableRowElement Js.t -> Html5_types.tr elt = rebuild_xml
    let of_tableCol : Dom_html.tableColElement Js.t -> Html5_types.col elt = rebuild_xml
    let of_tableSection : Dom_html.tableSectionElement Js.t -> [ Html5_types.tfoot | Html5_types.thead | Html5_types.tbody ] elt = rebuild_xml
    let of_tableCaption : Dom_html.tableCaptionElement Js.t -> Html5_types.caption elt = rebuild_xml
    let of_table : Dom_html.tableElement Js.t -> Html5_types.table elt = rebuild_xml
    let of_canvas : Dom_html.canvasElement Js.t -> 'a Html5_types.canvas elt = rebuild_xml
    let of_iFrame : Dom_html.iFrameElement Js.t -> Html5_types.iframe elt = rebuild_xml
  end

  let set_classes_of_elt elt = F.tot (X.set_classes_of_elt (F.toelt elt))

end
