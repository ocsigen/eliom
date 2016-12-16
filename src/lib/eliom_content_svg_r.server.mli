module Raw = Eliom_content_svg_raw.R
include module type of Raw

(** [pcdata] is not implemented reactively for SVG. *)
val pcdata : string Xml.W.t -> [> `Unimplemented ]

(** [node s] produces an ['a elt] out of the shared reactive
    signal [s]. *)
val node : 'a elt Eliom_shared.React.S.t -> 'a elt
