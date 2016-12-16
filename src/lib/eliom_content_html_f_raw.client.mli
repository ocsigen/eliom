open Js_of_ocaml

include
  Html_sigs.T
  with type 'a Xml.W.t = 'a
   and type 'a Xml.W.tlist = 'a list
   and type ('a,'b) Xml.W.ft = 'a -> 'b
   and type Xml.uri = string
   and type Xml.event_handler = Dom_html.event Js.t -> unit
   and type Xml.mouse_event_handler = Dom_html.mouseEvent Js.t -> unit
   and type Xml.keyboard_event_handler = Dom_html.keyboardEvent Js.t -> unit
   and type Xml.touch_event_handler = Dom_html.touchEvent Js.t -> unit
   and type Xml.attrib = Eliom_content_xml.Xml.attrib
   and type Xml.elt = Eliom_content_xml.Xml.elt
   and type 'a elt = 'a Eliom_content_html_types.elt
   and type 'a attrib = 'a Eliom_content_html_types.attrib
   and module Svg := Eliom_content_svg_f_raw
