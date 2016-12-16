open Js_of_ocaml

include
  Html_sigs.T
  with type 'a Xml.W.t = 'a Eliom_shared.React.S.t
   and type 'a Xml.W.tlist = 'a Eliom_shared.ReactiveData.RList.t
   and type ('a, 'b) Xml.W.ft = unit -> ('a -> 'b) Eliom_shared.Value.t
   and type Xml.uri = Eliom_content_xml.Xml.uri
   and type Xml.event_handler =
         (Dom_html.event Js.t -> unit) Eliom_client_value.t
   and type Xml.mouse_event_handler =
         (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
   and type Xml.keyboard_event_handler =
         (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t
   and type Xml.touch_event_handler =
         (Dom_html.touchEvent Js.t -> unit) Eliom_client_value.t
   and type Xml.elt = Eliom_content_xml.Xml.elt
   and type Xml.attrib = Eliom_content_xml.Xml.attrib
   and type 'a elt = 'a Eliom_content_html_raw.elt
   and type 'a attrib = 'a Eliom_content_html_raw.attrib
   and module Svg := Eliom_content_svg_raw.R

(** [pcdata s] produces a node of type
    [\[> Html_types.span\] elt]
    out of the string signal [s]. *)
val pcdata :
  string Eliom_shared.React.S.t -> [> Html_types.span] elt

(** [node s] produces an ['a elt] out of the shared reactive
    signal [s]. *)
val node : 'a elt Eliom_shared.React.S.t -> 'a elt

(** [filter_attrib a b] amounts to the attribute [a] while [b] is
    [true], and to no attribute while [b] is [false]. *)
val filter_attrib :
  'a attrib -> bool Eliom_shared.React.S.t -> 'a attrib
