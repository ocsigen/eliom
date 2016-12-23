
[%%shared.start]

type 'a elt = 'a Eliom_content_svg_raw.elt
type 'a attrib = 'a Eliom_content_svg_raw.attrib
module F = Eliom_content_svg_f
module D = Eliom_content_svg_d
module R = Eliom_content_svg_r

[%%client.start]

type uri = string

module Of_dom = struct
  let of_element elt =
    Eliom_content_svg_raw.F.tot
      (Eliom_content_xml.Xml.make_dom (elt :> Dom.node Js.t))
end

module To_dom = struct
  open Eliom_client_core

  let of_element elt = rebuild_node_svg "of_element" elt
  let of_node elt = rebuild_node_svg "of_node" elt

  let of_pcdata elt = rebuild_node_svg "of_pcdata" elt
end

module Id = struct
  type 'a id = string (* FIXME invariant type parameter ? *)
  let new_elt_id: ?global:bool -> unit -> 'a id =
    Eliom_content_xml.Xml.make_node_name
  let create_named_elt ~(id : 'a id) elt =
    D.tot (Eliom_content_xml.Xml.make_process_node ~id (D.toelt elt))
  let create_global_elt elt =
    D.tot (Eliom_content_xml.Xml.make_process_node (D.toelt elt))
  let create_request_elt ?reset:(reset = true) elt =
    D.tot (Eliom_content_xml.Xml.make_request_node ~reset (D.toelt elt))
  let string_of_id x = x

  let get_element' id =
    let id = string_of_id id in
    let node = Eliom_client_core.getElementById id in
    Js.Opt.case
      (Dom_html.CoerceTo.element node)
      (fun () -> failwith (Printf.sprintf "Non element node (%s)" id))
      (fun x -> x)

  let get_element id =
    try Some (Of_dom.of_element (get_element' id))
    with Failure _ -> None
end

module Manip = struct

  include
    Eliom_content_manip.Make(Eliom_content_svg_f)(To_dom)(Of_dom)
      (struct
        type 'a id = 'a Id.id
        let get_element' id = (Id.get_element' id :> Dom.node Js.t)
      end)
      (struct
        let content_ns = `SVG
      end)
   module Named = RawNamed

end

module C = struct
  let node ?init x = x
  let attr ?init x = x
end

[%%shared.start]

type boxed
external boxed : 'a Eliom_client_value.t
  -> boxed Eliom_client_value.t
  = "%identity"
external unboxed : boxed Eliom_client_value.t
  -> 'a Eliom_client_value.t
  = "%identity"

[%%server.start]

type uri = Eliom_content_xml.Xml.uri

module C = struct
  let node ?(init=D.Unsafe.node "g" []) x =
    let dummy_elt = D.toelt init in
    (* We need to box / unbox the client_value to convince eliom it's not polymorphic *)
    let client_boxed = boxed x in
    let _ = [%client (
        let dummy_dom =
          To_dom.of_element
            (D.tot ~%((dummy_elt : Eliom_content_xml.Xml.elt))) in
        let client_boxed = ~%client_boxed in
        let real = To_dom.of_element (unboxed client_boxed) in
        Js.Opt.iter
          (dummy_dom##.parentNode)
          (fun parent -> parent##(replaceChild real dummy_dom));
      : unit)] in
    init

  let attr ?init x : 'a attrib =
    Eliom_content_svg_raw.F.to_attrib
      (Eliom_content_xml.Xml.client_attrib ?init x)
end

module Id = struct
  type 'a id = string (* FIXME invariant type parameter ? *)
  let new_elt_id: ?global:bool -> unit -> 'a id =
    fun ?(global=true) () -> Eliom_content_xml.Xml.make_node_name ~global ()
  let create_named_elt ~(id : 'a id) elt =
    D.tot (Eliom_content_xml.Xml.make_process_node ~id (D.toelt elt))
  let create_global_elt elt =
    D.tot (Eliom_content_xml.Xml.make_process_node (D.toelt elt))
  let create_request_elt ?reset elt =
    D.tot (Eliom_content_xml.Xml.make_request_node ?reset (D.toelt elt))
end

module Printer = Xml_print.Make_typed_fmt(Eliom_content_xml.Xml)(F)
