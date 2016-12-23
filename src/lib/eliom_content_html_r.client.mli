
module Raw = Eliom_content_html_raw.R
include module type of Raw

(** the function [node s] create an HTML5 [elt] from a signal [s].
The resulting HTML5 [elt] can then be used like anyother HTML5 [elt] *)
val node : 'a elt React.signal -> 'a elt

val filter_attrib : 'a attrib -> bool React.signal -> 'a attrib
