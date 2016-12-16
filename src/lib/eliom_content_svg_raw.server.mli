open Js_of_ocaml

type +'a elt
type +'a attrib

module type RAW =
  Svg_sigs.T
  with type 'a Xml.W.t = 'a
   and type 'a Xml.W.tlist = 'a list
   and type ('a,'b) Xml.W.ft = 'a -> 'b
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
   and type 'a elt = 'a elt
   and type 'a attrib = 'a attrib

module F : RAW

module D : RAW

module R :
  sig
    include
      Svg_sigs.T
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
       and type 'a elt = 'a elt
       and type 'a attrib = 'a attrib

    (** [node s] produces an ['a elt] out of the shared reactive
        signal [s]. *)
    val node : 'a elt Eliom_shared.React.S.t -> 'a elt
  end
