type 'a elt = Eliom_content_xml.Xml.elt
type 'a attrib = Eliom_content_xml.Xml.attrib

module type RAW =
  Svg_sigs.T
  with type 'a Xml.W.t = 'a
   and type 'a Xml.W.tlist = 'a list
   and type ('a,'b) Xml.W.ft = 'a -> 'b
   and type Xml.uri = string
   and type Xml.event_handler = Dom_html.event Js.t -> unit
   and type Xml.mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
   and type Xml.keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit
   and type Xml.elt = Eliom_content_xml.Xml.elt
   and type Xml.attrib = Eliom_content_xml.Xml.attrib
   and type 'a elt = 'a elt
   and type 'a attrib = 'a attrib

module F = Svg_f.Make(Eliom_content_xml.Xml)

module D = Svg_f.Make(struct
    include Eliom_content_xml.Xml

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

module R = Svg_f.Make(Eliom_content_xml.Xml_wed)
