(** Building valid SVG . *)

(** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

type +'a elt = 'a Eliom_content_svg_types.elt
type +'a attrib = 'a Eliom_content_svg_types.attrib
type uri = string

(** Creation of {e f}unctional content (copy-able but not referable). *)
module F = Eliom_content_svg_f

(** Creation of content with {e D}OM semantics (referable

     See {% <<a_api project="tyxml" | module Svg_sigs.T >> %} *)
module D = Eliom_content_svg_d

(** Creation of reactive content *)
module R = Eliom_content_svg_r

(** Creation of content from client-side values. *)
module C : sig
  val node : ?init:'a D.elt -> 'a elt Eliom_client_value.t -> 'a D.elt
  val attr : ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib
end

(** Node identifiers *)
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

  (** [get_element id] returns
      the HTML element in the DOM with the given [id] if it exists. *)
  val get_element : 'a id -> 'a elt option
end

(** DOM-like manipulation functions.

    In this module, all the functions apply only to SVG element with
    {% <<a_manual chapter="clientserver-html" fragment="unique"|Dom semantics>>
    %}.
*)
module Manip : sig

  (** [appendChild e1 e2] inserts the element [e2] as last
      child of [e1]. If the optional parameter [~before:e3] is present
      and if [e3] is a child of [e1], then [e2] is inserted before [e3]
      in the list of [e1] children. *)
  val appendChild: ?before:'a elt -> 'b elt ->  'c elt -> unit

  (** [appendChildren e1 elts] inserts [elts] as last children
      of [e1]. If the optional parameter [~before:e3] is present and if
      [e3] is a child of [e1], then [elts] are inserted before [e3] in
      the list of [e1] children. *)
  val appendChildren: ?before:'a elt -> 'b elt ->  'c elt list -> unit

  (** [insertFirstChild p c] inserts [c] as first child of [p] *)
  val insertFirstChild: 'b elt ->  'c elt -> unit

  (** [nth e n] returns the nth child of [e] (first is 0) *)
  val nth : 'a elt -> int -> 'b elt option

  (** [childLength e] returns the number of children of [e] *)
  val childLength : 'a elt -> int

  (** [removeChild e1 e2] removes for [e2] from the list of
      children of [e1]. *)
  val removeChild: 'a elt -> 'b elt -> unit

  (** [replace e1 e2 e3] replaces [e3] by [e2] in the
      list of children of [e1]. *)
  val replaceChild: 'a elt -> 'b elt -> 'c elt -> unit

  (** [removeChildren e1] removes all children of [e1]. *)
  val removeChildren: 'a elt -> unit

  (** [removeSelf e] removes element e from the DOM. *)
  val removeSelf: 'a elt -> unit

  (** [replaceChildren e1 elts] replaces all the children of
      [e1] by [elt]. *)
  val replaceChildren: 'a elt -> 'b elt list -> unit

  (** [parentNode elt] returns the parent of [elt], if any. *)
  val parentNode: 'a elt -> 'b elt option

  (** [nextSibling elt] returns the next element that has the same parent,
      if [elt] is not the last. *)
  val nextSibling: 'a elt -> 'b elt option

  (** [previousSibling elt] returns the previous element
      that has the same parent,
      if [elt] is not the first. *)
  val previousSibling: 'a elt -> 'b elt option

  (** [insertBefore ~before elt] insert [elt] before [before]. *)
  val insertBefore: before:'a elt -> 'b elt -> unit

  (** [insertAfter ~after elt] insert [elt] after [after]. *)
  val insertAfter: after:'a elt -> 'b elt -> unit

  (** [replaceSelf elt1 elt2] replaces [elt1] by [elt2]. *)
  val replaceSelf: 'a elt -> 'b elt -> unit

  (* (\** The function [addEventListener elt evt handler] attach the *)
      (* [handler] for the event [evt] on the element [elt]. See the *)
      (* Js_of_ocaml manual, for a list of {% <<a_api project="js_of_ocaml" *)
      (* text="available events"| module Dom_html.Event >>%}. *\) *)
  (* val addEventListener: *)
    (* ?capture:bool -> *)
    (* 'a elt -> *)
    (* (#Dom_html.event as 'b) Js.t Dom_html.Event.typ -> *)
    (* ('a elt -> 'b Js.t -> bool) -> *)
    (* Dom_html.event_listener_id *)

  (** Dom manipulation by element identifier. *)
  module Named: sig

    (** The module [Named] defines the same functions as
        [Eliom_dom]. They take as parameter an element identifier
        instead of an element with Dom semantics. Those functions only
        works if the element is available in the application (sent in
        the page or along the page). If the element is not available,
        those functions raise with [Not_found]. *)

    (** see [appendChild] *)
    val appendChild: ?before:'a elt -> 'b Id.id -> 'c elt -> unit

    (** see [appendChildren] *)
    val appendChildren: ?before:'a elt -> 'b Id.id ->  'c elt list -> unit

    (** see [removeChild] *)
    val removeChild: 'a Id.id -> 'b elt -> unit

    (** see [replaceChild] *)
    val replaceChild: 'a Id.id -> 'b elt -> 'c elt -> unit

    (** see [removeChildren] *)
    val removeChildren: 'a Id.id -> unit

    (** see [replaceChildren] *)
    val replaceChildren: 'a Id.id -> 'b elt list -> unit

    (* (\** see [addEventListener] *\) *)
    (* val addEventListener: *)
      (* ?capture:bool -> *)
      (* 'a Id.id -> *)
      (* (#Dom_html.event as 'b) Js.t Dom_html.Event.typ -> *)
      (* ('a elt -> 'b Js.t -> bool) -> *)
      (* Dom_html.event_listener_id *)

  end

  (**/**)
  val childNodes: 'a elt -> Dom.node Js.t list
  val childElements: 'a elt -> Dom.element Js.t list
  (**/**)

  module Class : sig
    val contain : 'a elt -> string -> bool
    val remove : 'a elt -> string -> unit
    val removes :'a elt -> string list -> unit
    val add :'a elt -> string -> unit
    val adds :'a elt -> string list -> unit
    val replace :  'a elt ->  string -> string -> unit
    val clear : 'a elt -> unit
    val toggle : 'a elt -> string -> unit
    val toggle2 : 'a elt -> string -> string -> unit
  end
end

(** Conversion from Svg [elt]s to Javascript DOM elements ([<:] {% <<a_api
    project="js_of_ocaml"| class Dom_html.element >> %}).
    One conversion function per source type (stressed by the [of_] prefix). *)
module To_dom : sig

  val of_element : 'a elt -> Dom_html.element Js.t
  val of_node : 'a elt -> Dom.node Js.t

  val of_pcdata : [> `Pcdata] elt -> Dom.text Js.t

end

(** Conversion functions from DOM nodes ({% <<a_api project="js_of_ocaml"| type Dom_html.element>> %} {% <<a_api
    project="js_of_ocaml"| type Js.t>> %}) to Eliom nodes ({% <<a_api | type Eliom_content.Html.elt>> %}). *)
module Of_dom : sig
  val of_element: #Dom_html.element Js.t -> _ elt
end
