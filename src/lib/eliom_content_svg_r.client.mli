(** The function [node s] creates an SVG [elt] from a signal [s].
    The resulting SVG [elt] can then be used like any other SVG
    [elt]. *)
val node :
  'a Eliom_content_svg_types.elt React.signal -> 'a Eliom_content_svg_types.elt

module Raw = Eliom_content_svg_r_raw
include module type of Raw
