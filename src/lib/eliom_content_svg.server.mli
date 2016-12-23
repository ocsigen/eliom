(** See the Eliom manual for more information on{% <<a_manual
    chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
    semantics>> %} for SVG tree manipulated by client/server
    application. *)

type +'a elt = 'a Eliom_content_svg_raw.elt
type +'a attrib = 'a Eliom_content_svg_raw.attrib
type uri = Eliom_content_xml.Xml.uri

(** Typed interface for building valid SVG tree (functional
    semantics). See {% <<a_api project="tyxml" | module
    Svg_sigs.T >> %}. *)
module F = Eliom_content_svg_f

(** Typed interface for building valid SVG tree (DOM semantics). See
    {% <<a_api project="tyxml" | module Svg_sigs.T >> %}. *)
module D = Eliom_content_svg_d

(** Creation of SVG content from shared reactive signals and data
    ({% <<a_api project="eliom" subproject="server"|module Eliom_shared>> %}).
    For the operations provided, see
    {% <<a_api project="tyxml" | module Svg_sigs.T >> %}. *)
module R = Eliom_content_svg_r

(** Creation of content from client-side values.  This makes
    possible to insert in server side generated pages some nodes
    that will be computed on client side (for example reactive
    nodes). *)
module C : sig

  val node : ?init:'a elt -> 'a elt Eliom_client_value.t -> 'a elt
  (** [node e] is a server-side node corresponding to the
      client-side node [e] . [node e] can be used like any other
      server-side node.

      The implementation uses an initial placeholder node that is
      later replaced by the client node. By default, the placeholder
      node is [span]. The [~init] argument can be used to provide a
      custom placeholder node (e.g., one with the same tag as the
      client node). This can be useful in contexts where [span] is
      not allowed. *)

(*XXXXXXXXXXXXXXXXXX
  val attr : ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib
*)

end

(** Node identifiers. *)
module Id : sig

  (** The type of global SVG element identifier. *)
  type +'a id

  (** The function [new_elt_id ()] creates a new HTML5 element
      identifier. (see the Eliom manual for more information on {%
      <<a_manual project="eliom" chapter="clientserver-html"
      fragment="global"|global element>>%}).*)
  val new_elt_id: ?global:bool -> unit -> 'a id

  (** The function [create_named_elt ~id elt] create a copy of the
      element [elt] that will be accessible through the name [id]. *)
  val create_named_elt: id:'a id -> 'a elt -> 'a elt

  (** The function [create_named_elt elt] is equivalent to
      [create_named_elt ~id:(new_elt_id ()) elt]. *)
  val create_global_elt: 'a elt -> 'a elt

  (** [create_request_elt ?reset elt] creates a referable copy of
      [elt]. If [~reset = true] is provided (default: false), a new
      ID is created even if [elt] has an ID already. *)
  val create_request_elt: ?reset:bool -> 'a elt -> 'a elt

end

(** SVG printer. See
    {% <<a_api project="tyxml" | module Xml_sigs.Typed_pp >> %}. *)
module Printer : Xml_sigs.Typed_pp
  with type +'a elt := 'a elt
   and type doc := F.doc
