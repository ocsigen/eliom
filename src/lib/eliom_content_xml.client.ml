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
  and elt = {
    (* See Eliom_content.Html.To_dom for the 'unwrap' function that convert
       the server's tree representation into the client one. *)
    mutable elt : node lazy_t;
    node_id : node_id;
  }

  let content e =
    match Lazy.force e.elt with
    | ReactChildren _
    | ReactNode _
    | DomNode _ -> assert false (* TODO *)
    | TyXMLNode elt -> elt
  let get_node e = Lazy.force e.elt
  let set_dom_node elt node = elt.elt <- Lazy.from_val (DomNode node)
  let get_node_id elt = elt.node_id

  let make ?(id = NoId) elt =
    { elt = Lazy.from_val (TyXMLNode elt); node_id = id; }
  let make_dom ?(id = NoId) node =
    { elt = Lazy.from_val (DomNode node); node_id = id; }
  let make_lazy ?(id = NoId) lazy_elt =
    let f () =
       let elt = Lazy.force lazy_elt in
       assert (elt.node_id = id);
       Lazy.force elt.elt
    in
    { node_id = id; elt = Lazy.from_fun f }
  let force_lazy { elt } = ignore (Lazy.force elt)

  let make_react ?(id = NoId) signal =
    {elt = Lazy.from_val (ReactNode signal); node_id = id; }

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?a name children = node ?a name (Eliom_lazy.force children)

  type event_handler = Dom_html.event Js.t -> unit
  type mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
  type keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit

  let event_handler_attrib name (value : event_handler) =
    internal_event_handler_attrib name
      (Caml (CE_client_closure value))
  let mouse_event_handler_attrib name (value : mouse_event_handler) =
    internal_event_handler_attrib name
      (Caml (CE_client_closure_mouse value))
  let keyboard_event_handler_attrib name (value : keyboard_event_handler) =
    internal_event_handler_attrib name
      (Caml (CE_client_closure_keyboard value))

  let node_react_children ?(a = []) name children =
    {elt = Lazy.from_val (ReactChildren (Node (name,a,[]),children)); node_id=NoId}

  let end_re = Regexp.regexp_string "]]>"

  let make_node_name =
    let node_id_counter = ref 0 in
    (fun ?(global = true) () ->
      incr node_id_counter;
      (if global then "global_" else "")
      ^ "client_" ^ (string_of_int !node_id_counter))

  let make_process_node ?(id = make_node_name ~global:true ()) elt =
    { elt with node_id = ProcessId id }

  let make_request_node ?(reset = true) elt =
    let f () =
      let id = RequestId (make_node_name ~global:false ()) in
      { elt with node_id = id }
    in
    if reset then
      f ()
    else
      match elt.node_id with
      | Eliom_runtime.RawXML.NoId ->
        f ()
      | _ ->
        elt

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
      | ReactChildren _ -> failwith "Eliom_content_core.set_classes_of_elt"
      | TyXMLNode econtent ->
          { elt with elt = Lazy.from_val (TyXMLNode (set_classes elt.node_id econtent)) }

  let string_of_node_id = function
    | NoId -> "NoId"
    | ProcessId s -> "ProcessId "^s
    | RequestId s -> "RequestId "^s

end

module Xml_wed =
struct
  module W = Tyxml_js.Wrap
  type 'a wrap = 'a W.t
  type 'a list_wrap = 'a W.tlist
  type uri = Xml.uri
  let string_of_uri = Xml.string_of_uri
  let uri_of_string = Xml.uri_of_string
  type aname = Xml.aname
  type event_handler = Xml.event_handler
  type mouse_event_handler = Xml.mouse_event_handler
  type keyboard_event_handler = Xml.keyboard_event_handler
  type attrib = Xml.attrib

  let float_attrib name s : attrib =
    name, Xml.RAReact (Tyxml_js.Wrap.fmap (fun f -> Some (Xml.AFloat f)) s)
  let int_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AInt f)) s)
  let string_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AStr f)) s)
  let space_sep_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some(Xml.AStrL (Xml.Space,f))) s)
  let comma_sep_attrib name s =
    name, Xml.RAReact (React.S.map (fun f -> Some (Xml.AStrL (Xml.Comma,f))) s)
  let event_handler_attrib = Xml.event_handler_attrib
  let mouse_event_handler_attrib = Xml.mouse_event_handler_attrib
  let keyboard_event_handler_attrib = Xml.keyboard_event_handler_attrib
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
  let node ?a name l = Xml.node_react_children ?a name l
  let cdata = Xml.cdata
  let cdata_script = Xml.cdata_script
  let cdata_style = Xml.cdata_style
end
