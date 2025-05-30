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
   [Eliom_client] et al.
   Its name is not [Eliom_content_base] because this would
   suggest the sharing between server and client. *)

open Js_of_ocaml
open Eliom_lib

module Xml = struct
  include Eliom_runtime.RawXML
  module W = Xml_wrap.NoWrap

  type 'a wrap = 'a
  type 'a list_wrap = 'a list

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
    | ReactChildren of econtent * elt ReactiveData.RList.t

  and elt =
    { (* See Eliom_content.Html.To_dom for the 'unwrap' function that convert
       the server's tree representation into the client one. *)
      mutable elt : node lazy_t
    ; node_id : node_id }

  let content e =
    match Lazy.force e.elt with
    | ReactChildren _ | ReactNode _ | DomNode _ -> assert false (* TODO *)
    | TyXMLNode elt -> elt

  let get_node e = Lazy.force e.elt
  let set_dom_node elt node = elt.elt <- Lazy.from_val (DomNode node)
  let get_node_id elt = elt.node_id
  let make ?(id = NoId) elt = {elt = Lazy.from_val (TyXMLNode elt); node_id = id}

  let make_dom ?(id = NoId) node =
    {elt = Lazy.from_val (DomNode node); node_id = id}

  let make_lazy ?(id = NoId) lazy_elt =
    let f () =
      let elt = Lazy.force lazy_elt in
      assert (elt.node_id = id);
      Lazy.force elt.elt
    in
    {node_id = id; elt = Lazy.from_fun f}

  let force_lazy {elt; _} = ignore (Lazy.force elt)

  let make_react ?(id = NoId) signal =
    {elt = Lazy.from_val (ReactNode signal); node_id = id}

  let empty () = make Empty
  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)
  let leaf ?(a = []) name = make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?a name children = node ?a name (Eliom_lazy.force children)

  type event_handler = Dom_html.event Js.t -> unit
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit
  type touch_event_handler = Dom_html.touchEvent Js.t -> unit

  let event_handler_attrib name (value : event_handler) =
    internal_event_handler_attrib name (Caml (CE_client_closure value))

  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    internal_event_handler_attrib name (Caml (CE_client_closure_mouse value))

  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    internal_event_handler_attrib name (Caml (CE_client_closure_keyboard value))

  let touch_event_handler_attrib name (value : touch_event_handler) =
    internal_event_handler_attrib name (Caml (CE_client_closure_touch value))

  let node_react_children ?(a = []) name children =
    { elt = Lazy.from_val (ReactChildren (Node (name, a, []), children))
    ; node_id = NoId }

  let end_re = Regexp.regexp_string "]]>"

  let make_node_name =
    let node_id_counter = ref 0 in
    fun ?(global = true) () ->
      incr node_id_counter;
      (if global then "global_" else "")
      ^ "client_"
      ^ string_of_int !node_id_counter

  let make_process_node ?(id = make_node_name ~global:true ()) elt =
    {elt with node_id = ProcessId id}

  let make_request_node ?(reset = true) elt =
    let f () =
      let id = RequestId (make_node_name ~global:false ()) in
      {elt with node_id = id}
    in
    if reset
    then f ()
    else match elt.node_id with Eliom_runtime.RawXML.NoId -> f () | _ -> elt

  let cdata s =
    let s' = "\n<![CDATA[\n" ^ Regexp.global_replace end_re s "" ^ "\n]]>\n" in
    encodedpcdata s'

  let cdata_script s =
    let s' =
      "\n//<![CDATA[\n" ^ Regexp.global_replace end_re s "" ^ "\n//]]>\n"
    in
    encodedpcdata s'

  let cdata_style s =
    let s' =
      "\n/* <![CDATA[ */\n"
      ^ Regexp.global_replace end_re s ""
      ^ "\n/* ]]> */\n"
    in
    encodedpcdata s'

  let set_classes node_id = function
    | (Empty | Comment _ | EncodedPCDATA _ | PCDATA _ | Entity _) as e -> e
    | Leaf (ename, attribs) -> Leaf (ename, filter_class_attribs node_id attribs)
    | Node (ename, attribs, sons) ->
        Node (ename, filter_class_attribs node_id attribs, sons)

  let set_classes_of_elt elt =
    match Lazy.force elt.elt with
    | DomNode _ -> failwith "Eliom_content_core.set_classes_of_elt"
    | ReactNode _ -> failwith "Eliom_content_core.set_classes_of_elt"
    | ReactChildren _ -> failwith "Eliom_content_core.set_classes_of_elt"
    | TyXMLNode econtent ->
        { elt with
          elt = Lazy.from_val (TyXMLNode (set_classes elt.node_id econtent)) }

  let string_of_node_id = function
    | NoId -> "NoId"
    | ProcessId s -> "ProcessId " ^ s
    | RequestId s -> "RequestId " ^ s
end

module Xml_wed = struct
  module W = Js_of_ocaml_tyxml.Tyxml_js.Wrap

  type 'a wrap = 'a W.t
  type 'a list_wrap = 'a W.tlist
  type uri = Xml.uri

  let string_of_uri = Xml.string_of_uri
  let uri_of_string = Xml.uri_of_string

  type aname = Xml.aname
  type event_handler = Xml.event_handler
  type mouse_event_handler = Xml.mouse_event_handler
  type keyboard_event_handler = Xml.keyboard_event_handler
  type touch_event_handler = Xml.touch_event_handler
  type attrib = Xml.attrib

  let float_attrib name s : attrib =
    ( name
    , Xml.RAReact
        (Js_of_ocaml_tyxml.Tyxml_js.Wrap.fmap (fun f -> Some (Xml.AFloat f)) s)
    )

  let int_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AInt f)) s)

  let string_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AStr f)) s)

  let space_sep_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AStrL (Xml.Space, f))) s)

  let comma_sep_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AStrL (Xml.Comma, f))) s)

  let event_handler_attrib = Xml.event_handler_attrib
  let mouse_event_handler_attrib = Xml.mouse_event_handler_attrib
  let keyboard_event_handler_attrib = Xml.keyboard_event_handler_attrib
  let touch_event_handler_attrib = Xml.touch_event_handler_attrib

  let uri_attrib name value =
    ( name
    , Xml.RAReact
        (React.S.map (fun f -> Some (Xml.AStr (Eliom_lazy.force f))) value) )

  let uris_attrib name value =
    ( name
    , Xml.RAReact
        (React.S.map
           (fun f -> Some (Xml.AStrL (Xml.Space, Eliom_lazy.force f)))
           value) )

  type elt = Xml.elt
  type ename = Xml.ename

  let empty = Xml.empty
  let comment = Xml.comment
  let pcdata s = Xml.make_react (React.S.map Xml.pcdata s)
  let encodedpcdata s = Xml.make_react (React.S.map Xml.encodedpcdata s)
  let entity = Xml.entity
  let leaf = Xml.leaf
  let node ?a name l = Xml.node_react_children ?a name l
  let cdata = Xml.cdata
  let cdata_script = Xml.cdata_script
  let cdata_style = Xml.cdata_style
end

module Svg = struct
  module Ev' (A : sig
      type 'a attrib

      module Unsafe : sig
        val string_attrib : string -> string -> 'a attrib
      end
    end) =
  struct
    let a_onabort s = A.Unsafe.string_attrib "onabort" s
    let a_onactivate s = A.Unsafe.string_attrib "onactivate" s
    let a_onbegin s = A.Unsafe.string_attrib "onbegin" s
    let a_onend s = A.Unsafe.string_attrib "onend" s
    let a_onerror s = A.Unsafe.string_attrib "onerror" s
    let a_onfocusin s = A.Unsafe.string_attrib "onfocusin" s
    let a_onfocusout s = A.Unsafe.string_attrib "onfocusout" s
    let a_onload s = A.Unsafe.string_attrib "onload" s
    let a_onrepeat s = A.Unsafe.string_attrib "onrepeat" s
    let a_onresize s = A.Unsafe.string_attrib "onresize" s
    let a_onscroll s = A.Unsafe.string_attrib "onscroll" s
    let a_onunload s = A.Unsafe.string_attrib "onunload" s
    let a_onzoom s = A.Unsafe.string_attrib "onzoom" s
    let a_onclick s = A.Unsafe.string_attrib "onclick" s
    let a_onmousedown s = A.Unsafe.string_attrib "onmousedown" s
    let a_onmouseup s = A.Unsafe.string_attrib "onmouseup" s
    let a_onmouseover s = A.Unsafe.string_attrib "onmouseover" s
    let a_onmouseout s = A.Unsafe.string_attrib "onmouseout" s
    let a_onmousemove s = A.Unsafe.string_attrib "onmousemove" s
    let a_ontouchstart s = A.Unsafe.string_attrib "ontouchstart" s
    let a_ontouchend s = A.Unsafe.string_attrib "ontouchend" s
    let a_ontouchmove s = A.Unsafe.string_attrib "ontouchmove" s
    let a_ontouchcancel s = A.Unsafe.string_attrib "ontouchcancel" s
  end

  module D = struct
    module Raw' = Svg_f.Make (struct
        include Xml

        let make elt = make_request_node (make elt)
        let empty () = make Empty
        let comment c = make (Comment c)
        let pcdata d = make (PCDATA d)
        let encodedpcdata d = make (EncodedPCDATA d)
        let entity e = make (Entity e)
        let leaf ?(a = []) name = make (Leaf (name, a))
        let node ?(a = []) name children = make (Node (name, a, children))
      end)

    module Raw = struct
      include Raw'
      include Ev' (Raw')
    end

    include Raw'
  end

  module F = struct
    module Raw' = Svg_f.Make (Xml)

    module Raw = struct
      include Raw'
      include Ev' (Raw')
    end

    include Raw'
  end

  module R = struct
    let node s = Xml.make_react s

    module Raw = Svg_f.Make (Xml_wed)
    include Raw
  end

  type +'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type 'a list_wrap = 'a F.list_wrap
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)

    let new_elt_id : ?global:bool -> unit -> 'a id = Xml.make_node_name

    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))

    let create_global_elt elt = D.tot (Xml.make_process_node (D.toelt elt))

    let create_request_elt ?(reset = true) elt =
      D.tot (Xml.make_request_node ~reset (D.toelt elt))

    let string_of_id x = x
  end

  module Of_dom = struct
    let rebuild_xml (node : 'a Js.t) : 'a F.elt =
      Xml.make_dom (node :> Dom.node Js.t)

    let of_element : Dom_html.element Js.t -> 'a elt = rebuild_xml
  end
end

module Html = struct
  module Ev' (A : sig
      type 'a attrib

      module Unsafe : sig
        val string_attrib : string -> string -> 'a attrib
      end
    end) =
  struct
    let a_onabort s = A.Unsafe.string_attrib "onabort" s
    let a_onafterprint s = A.Unsafe.string_attrib "onafterprint" s
    let a_onbeforeprint s = A.Unsafe.string_attrib "onbeforeprint" s
    let a_onbeforeunload s = A.Unsafe.string_attrib "onbeforeunload" s
    let a_onblur s = A.Unsafe.string_attrib "onblur" s
    let a_oncanplay s = A.Unsafe.string_attrib "oncanplay" s
    let a_oncanplaythrough s = A.Unsafe.string_attrib "oncanplaythrough" s
    let a_onchange s = A.Unsafe.string_attrib "onchange" s
    let a_onclose s = A.Unsafe.string_attrib "onclose" s
    let a_ondurationchange s = A.Unsafe.string_attrib "ondurationchange" s
    let a_onemptied s = A.Unsafe.string_attrib "onemptied" s
    let a_onended s = A.Unsafe.string_attrib "onended" s
    let a_onerror s = A.Unsafe.string_attrib "onerror" s
    let a_onfocus s = A.Unsafe.string_attrib "onfocus" s
    let a_onformchange s = A.Unsafe.string_attrib "onformchange" s
    let a_onforminput s = A.Unsafe.string_attrib "onforminput" s
    let a_onhashchange s = A.Unsafe.string_attrib "onhashchange" s
    let a_oninput s = A.Unsafe.string_attrib "oninput" s
    let a_oninvalid s = A.Unsafe.string_attrib "oninvalid" s
    let a_onmousewheel s = A.Unsafe.string_attrib "onmousewheel" s
    let a_onoffline s = A.Unsafe.string_attrib "onoffline" s
    let a_ononline s = A.Unsafe.string_attrib "ononline" s
    let a_onpause s = A.Unsafe.string_attrib "onpause" s
    let a_onplay s = A.Unsafe.string_attrib "onplay" s
    let a_onplaying s = A.Unsafe.string_attrib "onplaying" s
    let a_onpagehide s = A.Unsafe.string_attrib "onpagehide" s
    let a_onpageshow s = A.Unsafe.string_attrib "onpageshow" s
    let a_onpopstate s = A.Unsafe.string_attrib "onpopstate" s
    let a_onprogress s = A.Unsafe.string_attrib "onprogress" s
    let a_onratechange s = A.Unsafe.string_attrib "onratechange" s
    let a_onreadystatechange s = A.Unsafe.string_attrib "onreadystatechange" s
    let a_onredo s = A.Unsafe.string_attrib "onredo" s
    let a_onresize s = A.Unsafe.string_attrib "onresize" s
    let a_onscroll s = A.Unsafe.string_attrib "onscroll" s
    let a_onseeked s = A.Unsafe.string_attrib "onseeked" s
    let a_onseeking s = A.Unsafe.string_attrib "onseeking" s
    let a_onselect s = A.Unsafe.string_attrib "onselect" s
    let a_onshow s = A.Unsafe.string_attrib "onshow" s
    let a_onstalled s = A.Unsafe.string_attrib "onstalled" s
    let a_onstorage s = A.Unsafe.string_attrib "onstorage" s
    let a_onsubmit s = A.Unsafe.string_attrib "onsubmit" s
    let a_onsuspend s = A.Unsafe.string_attrib "onsuspend" s
    let a_ontimeupdate s = A.Unsafe.string_attrib "ontimeupdate" s
    let a_onundo s = A.Unsafe.string_attrib "onundo" s
    let a_onunload s = A.Unsafe.string_attrib "onunload" s
    let a_onvolumechange s = A.Unsafe.string_attrib "onvolumechange" s
    let a_onwaiting s = A.Unsafe.string_attrib "onwaiting" s
    let a_onload s = A.Unsafe.string_attrib "onload" s
    let a_onloadeddata s = A.Unsafe.string_attrib "onloadeddata" s
    let a_onloadedmetadata s = A.Unsafe.string_attrib "onloadedmetadata" s
    let a_onloadstart s = A.Unsafe.string_attrib "onloadstart" s
    let a_onmessage s = A.Unsafe.string_attrib "onmessage" s
    let a_onclick s = A.Unsafe.string_attrib "onclick" s
    let a_oncontextmenu s = A.Unsafe.string_attrib "oncontextmenu" s
    let a_ondblclick s = A.Unsafe.string_attrib "ondblclick" s
    let a_ondrag s = A.Unsafe.string_attrib "ondrag" s
    let a_ondragend s = A.Unsafe.string_attrib "ondragend" s
    let a_ondragenter s = A.Unsafe.string_attrib "ondragenter" s
    let a_ondragleave s = A.Unsafe.string_attrib "ondragleave" s
    let a_ondragover s = A.Unsafe.string_attrib "ondragover" s
    let a_ondragstart s = A.Unsafe.string_attrib "ondragstart" s
    let a_ondrop s = A.Unsafe.string_attrib "ondrop" s
    let a_onmousedown s = A.Unsafe.string_attrib "onmousedown" s
    let a_onmouseup s = A.Unsafe.string_attrib "onmouseup" s
    let a_onmouseover s = A.Unsafe.string_attrib "onmouseover" s
    let a_onmousemove s = A.Unsafe.string_attrib "onmousemove" s
    let a_onmouseout s = A.Unsafe.string_attrib "onmouseout" s
    let a_ontouchstart s = A.Unsafe.string_attrib "ontouchstart" s
    let a_ontouchend s = A.Unsafe.string_attrib "ontouchend" s
    let a_ontouchmove s = A.Unsafe.string_attrib "ontouchmove" s
    let a_ontouchcancel s = A.Unsafe.string_attrib "ontouchcancel" s
    let a_onkeypress s = A.Unsafe.string_attrib "onkeypress" s
    let a_onkeydown s = A.Unsafe.string_attrib "onkeydown" s
    let a_onkeyup s = A.Unsafe.string_attrib "onkeyup" s
  end

  module D = struct
    module Xml' = struct
      include Xml

      let make elt = make_request_node (make elt)
      let empty () = make Empty
      let comment c = make (Comment c)
      let pcdata d = make (PCDATA d)
      let encodedpcdata d = make (EncodedPCDATA d)
      let entity e = make (Entity e)
      let leaf ?(a = []) name = make (Leaf (name, a))
      let node ?(a = []) name children = make (Node (name, a, children))

      let lazy_node ?(a = []) name children =
        make (Node (name, a, Eliom_lazy.force children))
    end

    module Raw' = Html_f.Make (Xml') (Svg.D.Raw')

    module Raw = struct
      include Raw'
      include Ev' (Raw')
    end

    include Raw'

    type ('a, 'b, 'c) lazy_star =
      ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elts =
      tot
        (Xml'.lazy_node ~a:(to_xmlattribs a) "form"
           (Eliom_lazy.from_fun (fun () -> toeltl (Eliom_lazy.force elts))))
  end

  module R = struct
    let node s = Xml.make_react s

    module Raw = Html_f.Make (Xml_wed) (Svg.R)

    let filter_attrib (name, a) on =
      let v =
        match a with
        | Xml.RA a ->
            Xml.RAReact
              (React.S.map (function true -> Some a | false -> None) on)
        | Xml.RAReact s ->
            Xml.RAReact (React.S.l2 (fun v b -> if b then v else None) s on)
        | Xml.RALazyStr s ->
            Xml.RAReact
              (React.S.map
                 (function
                   | true -> Some (Xml.AStr (Eliom_lazy.force s))
                   | false -> None)
                 on)
        | Xml.RALazyStrL (sep, l) ->
            Xml.RAReact
              (React.S.map
                 (function
                   | true -> Some (Xml.AStrL (sep, List.map Eliom_lazy.force l))
                   | false -> None)
                 on)
        | Xml.RACamlEventHandler _ ->
            failwith "R.filter_attrib not implemented for event handler"
        | Xml.RAClient _ -> assert false
      in
      name, v

    include Raw
  end

  module F = struct
    module Xml' = Xml
    module Raw' = Html_f.Make (Xml') (Svg.F.Raw')

    module Raw = struct
      include Raw'
      include Ev' (Raw')
    end

    include Raw'

    type ('a, 'b, 'c) lazy_star =
      ?a:'a attrib list -> 'b elt list Eliom_lazy.request -> 'c elt

    let lazy_form ?(a = []) elts =
      tot
        (Xml'.lazy_node ~a:(to_xmlattribs a) "form"
           (Eliom_lazy.from_fun (fun () -> toeltl (Eliom_lazy.force elts))))
  end

  type +'a elt = 'a F.elt
  type 'a wrap = 'a F.wrap
  type 'a list_wrap = 'a F.list_wrap
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Id = struct
    type 'a id = string (* FIXME invariant type parameter ? *)

    let new_elt_id : ?global:bool -> unit -> 'a id = Xml.make_node_name

    let create_named_elt ~(id : 'a id) elt =
      D.tot (Xml.make_process_node ~id (D.toelt elt))

    let create_global_elt elt = D.tot (Xml.make_process_node (D.toelt elt))

    let create_request_elt ?(reset = true) elt =
      D.tot (Xml.make_request_node ~reset (D.toelt elt))

    let string_of_id x = x
  end

  module Custom_data = struct
    type 'a t =
      { name : string
      ; to_string : 'a -> string
      ; of_string : string -> 'a
      ; default : 'a option }

    let create ~name ?default ~to_string ~of_string () =
      {name; of_string; to_string; default}

    let create_json ~name ?default typ =
      {name; of_string = of_json ~typ; to_string = to_json ~typ; default}

    let attrib custom_data value =
      F.a_user_data custom_data.name (custom_data.to_string value)

    let attribute_name name = "data-" ^ name

    let get_dom (element : Dom_html.element Js.t) custom_data =
      Js.Opt.case
        element##(getAttribute (Js.string (attribute_name custom_data.name)))
        (fun () ->
           match custom_data.default with
           | Some value -> value
           | None -> raise Not_found)
        (fun str -> custom_data.of_string (Js.to_string str))

    let set_dom element custom_data value =
      element##(setAttribute
                  (Js.string (attribute_name custom_data.name))
                  (Js.string (custom_data.to_string value)))
  end

  module Of_dom = Js_of_ocaml_tyxml.Tyxml_cast.MakeOf (struct
      type 'a elt = 'a F.elt

      let elt (node : 'a Js.t) : 'a elt = Xml.make_dom (node :> Dom.node Js.t)
    end)

  let set_classes_of_elt elt = F.tot (Xml.set_classes_of_elt (F.toelt elt))
end
