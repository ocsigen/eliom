(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js_of_ocaml

(** This module provides the creation of valid XML content, i.e. XML, SVG,
    and (X)HTML5.

    {b Please read
    {% <<a_manual chapter="clientserver-html"|Eliom's manual>>%}
    for more information on HTML generation. }
    You can also have a look at the server API of
    {% <<a_api subproject="server" | module Eliom_content >> %}
    for an explication of the modules [F] and [D].

  *)

module Xml : module type of Eliom_content_core.Xml
(** Low-level XML manipulation. *)

(** Building valid SVG . *)
module Svg : sig
  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt
  type +'a attrib
  type uri = Xml.uri

  (**/**)

  module Ev' = Eliom_content_core.Svg.Ev'

  (**/**)

  (** Creation of {e f}unctional content (copy-able but not referable).

       See {% <<a_api project="tyxml" | module Svg_sigs.T >> %} *)
  module F : sig
    (** Cf. {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)

    (**/**)

    module Raw' :
      Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'
  end

  (** Creation of content with {e D}OM semantics (referable

       See {% <<a_api project="tyxml" | module Svg_sigs.T >> %} *)
  module D : sig
    (** Cf. {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)

    (**/**)

    module Raw' :
      Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'
  end

  (** Creation of reactive content *)
  module R : sig
    val node : 'a elt React.signal -> 'a elt
    (** The function [node s] creates an SVG [elt] from a signal [s].
        The resulting SVG [elt] can then be used like any other SVG
        [elt]. *)

    module Raw :
      Svg_sigs.Make(Eliom_content_core.Xml_wed).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw
  end

  (** Creation of content from client-side values. *)
  module C : sig
    val node : ?init:'a D.elt -> 'a elt Eliom_client_value.t -> 'a D.elt
    val attr : ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib
  end

  (** Node identifiers *)
  module Id : sig
    type +'a id
    (** The type of global SVG element identifier. *)

    val new_elt_id : ?global:bool -> unit -> 'a id
    (** The function [new_elt_id ()] creates a new HTML5 element
        identifier. (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="clientserver-html"
        fragment="global"|global element>>%}).*)

    val create_named_elt : id:'a id -> 'a elt -> 'a elt
    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be accessible through the name [id]. *)

    val create_global_elt : 'a elt -> 'a elt
    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)

    val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
    (** [create_request_elt ?reset elt] creates a referable copy of
        [elt]. If [~reset = true] is provided (default: false), a new
        ID is created even if [elt] has an ID already. *)

    val get_element : 'a id -> 'a elt option
    (** [get_element id] returns
        the HTML element in the DOM with the given [id] if it exists. *)
  end

  (** DOM-like manipulation functions.

      In this module, all the functions apply only to SVG element with
      {% <<a_manual chapter="clientserver-html" fragment="unique"|Dom semantics>>
      %}.
  *)
  module Manip : sig
    val appendChild : ?before:'a elt -> 'b elt -> 'c elt -> unit
    (** [appendChild e1 e2] inserts the element [e2] as last
        child of [e1]. If the optional parameter [~before:e3] is present
        and if [e3] is a child of [e1], then [e2] is inserted before [e3]
        in the list of [e1] children. *)

    val appendChildren : ?before:'a elt -> 'b elt -> 'c elt list -> unit
    (** [appendChildren e1 elts] inserts [elts] as last children
        of [e1]. If the optional parameter [~before:e3] is present and if
        [e3] is a child of [e1], then [elts] are inserted before [e3] in
        the list of [e1] children. *)

    val insertFirstChild : 'b elt -> 'c elt -> unit
    (** [insertFirstChild p c] inserts [c] as first child of [p] *)

    val nth : 'a elt -> int -> 'b elt option
    (** [nth e n] returns the nth child of [e] (first is 0) *)

    val childLength : 'a elt -> int
    (** [childLength e] returns the number of children of [e] *)

    val removeChild : 'a elt -> 'b elt -> unit
    (** [removeChild e1 e2] removes for [e2] from the list of
        children of [e1]. *)

    val replaceChild : 'a elt -> 'b elt -> 'c elt -> unit
    (** [replace e1 e2 e3] replaces [e3] by [e2] in the
        list of children of [e1]. *)

    val removeChildren : 'a elt -> unit
    (** [removeChildren e1] removes all children of [e1]. *)

    val removeSelf : 'a elt -> unit
    (** [removeSelf e] removes element e from the DOM. *)

    val replaceChildren : 'a elt -> 'b elt list -> unit
    (** [replaceChildren e1 elts] replaces all the children of
        [e1] by [elt]. *)

    val parentNode : 'a elt -> 'b elt option
    (** [parentNode elt] returns the parent of [elt], if any. *)

    val nextSibling : 'a elt -> 'b elt option
    (** [nextSibling elt] returns the next element that has the same parent,
        if [elt] is not the last. *)

    val previousSibling : 'a elt -> 'b elt option
    (** [previousSibling elt] returns the previous element
        that has the same parent,
        if [elt] is not the first. *)

    val insertBefore : before:'a elt -> 'b elt -> unit
    (** [insertBefore ~before elt] insert [elt] before [before]. *)

    val insertAfter : after:'a elt -> 'b elt -> unit
    (** [insertAfter ~after elt] insert [elt] after [after]. *)

    val replaceSelf : 'a elt -> 'b elt -> unit
    (** [replaceSelf elt1 elt2] replaces [elt1] by [elt2]. *)

    (* (\** The function [addEventListener elt evt handler] attach the *)
    (* [handler] for the event [evt] on the element [elt]. See the *)
    (* Js_of_ocaml manual, for a list of {% <<a_api project="js_of_ocaml" *)
    (* text="available events"| module Js_of_ocaml.Dom_html.Event >>%}. *\) *)
    (* val addEventListener: *)
    (* ?capture:bool -> *)
    (* 'a elt -> *)
    (* (#Dom_html.event as 'b) Js.t Dom_html.Event.typ -> *)
    (* ('a elt -> 'b Js.t -> bool) -> *)
    (* Dom_html.event_listener_id *)

    (** Dom manipulation by element identifier. *)
    module Named : sig
      (** The module [Named] defines the same functions as
          [Eliom_dom]. They take as parameter an element identifier
          instead of an element with Dom semantics. Those functions only
          works if the element is available in the application (sent in
          the page or along the page). If the element is not available,
          those functions raise with [Not_found]. *)

      val appendChild : ?before:'a elt -> 'b Id.id -> 'c elt -> unit
      (** see [appendChild] *)

      val appendChildren : ?before:'a elt -> 'b Id.id -> 'c elt list -> unit
      (** see [appendChildren] *)

      val removeChild : 'a Id.id -> 'b elt -> unit
      (** see [removeChild] *)

      val replaceChild : 'a Id.id -> 'b elt -> 'c elt -> unit
      (** see [replaceChild] *)

      val removeChildren : 'a Id.id -> unit
      (** see [removeChildren] *)

      val replaceChildren : 'a Id.id -> 'b elt list -> unit
      (** see [replaceChildren] *)

      (* (\** see [addEventListener] *\) *)
      (* val addEventListener: *)
      (* ?capture:bool -> *)
      (* 'a Id.id -> *)
      (* (#Dom_html.event as 'b) Js.t Dom_html.Event.typ -> *)
      (* ('a elt -> 'b Js.t -> bool) -> *)
      (* Dom_html.event_listener_id *)
    end

    (**/**)

    val childNodes : 'a elt -> Dom.node Js.t list
    val childElements : 'a elt -> Dom.element Js.t list

    (**/**)

    module Class : sig
      val contain : 'a elt -> string -> bool
      val remove : 'a elt -> string -> unit
      val removes : 'a elt -> string list -> unit
      val add : 'a elt -> string -> unit
      val adds : 'a elt -> string list -> unit
      val replace : 'a elt -> string -> string -> unit
      val clear : 'a elt -> unit
      val toggle : 'a elt -> string -> unit
      val toggle2 : 'a elt -> string -> string -> unit
    end
  end

  (** Conversion from Svg [elt]s to Javascript DOM elements ([<:] {% <<a_api
      project="js_of_ocaml"| class Js_of_ocaml.Dom_html.element >> %}).
      One conversion function per source type (stressed by the [of_] prefix). *)
  module To_dom : sig
    val of_element : 'a elt -> Dom_html.element Js.t
    val of_node : 'a elt -> Dom.node Js.t
    val of_pcdata : [> `Pcdata] elt -> Dom.text Js.t
  end

  (** Conversion functions from DOM nodes ({% <<a_api project="js_of_ocaml"| type Js_of_ocaml.Dom_html.element>> %} {% <<a_api
      project="js_of_ocaml"| type Js_of_ocaml.Js.t>> %}) to Eliom nodes ({% <<a_api | type Eliom_content.Html.elt>> %}). *)
  module Of_dom : sig
    val of_element : Dom_html.element Js.t -> 'a elt
  end
end

(** Building valid (X)HTML5. *)
module Html : sig
  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt
  type +'a attrib
  type uri = Xml.uri
  type 'a form_param

  (**/**)

  module Ev' = Eliom_content_core.Html.Ev'

  (**/**)

  (** Creation of {e f}unctional HTML5 content (copy-able but not referable). *)
  module F : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html_sigs.T >> %} *)

    (** Cf. {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)

    (**/**)

    module Raw' :
      Html_sigs.Make(Xml)(Svg.F.Raw').T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'

    include
      Eliom_content_sigs.LINKS_AND_FORMS
      with type +'a elt := 'a elt
       and type +'a attrib := 'a attrib
       and type uri := uri
       and type ('a, 'b, 'c) star := ('a, 'b, 'c) star
       and type 'a form_param := 'a form_param
  end

  (** Creation of HTML5 content with {e D}OM semantics (referable) *)
  module D : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html_sigs.T >> %} *)

    (** Cf. {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)

    (**/**)

    module Raw' :
      Html_sigs.Make(Xml)(Svg.D.Raw').T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    (**/**)

    module Raw : sig
      include module type of Raw'
      include module type of Ev' (Raw')
    end

    include module type of Raw'

    include
      Eliom_content_sigs.LINKS_AND_FORMS
      with type +'a elt := 'a elt
       and type +'a attrib := 'a attrib
       and type uri := uri
       and type ('a, 'b, 'c) star := ('a, 'b, 'c) star
       and type 'a form_param := 'a form_param
  end

  (** Creation of HTML5 content from
      {{:http://erratique.ch/software/react} React} signals.
      HTML5's trees are automatically updated whenever
      corresponding signals change.  *)
  module R : sig
    (** {2 Content creation}
        See {% <<a_api project="tyxml" | module Html_sigs.T >> %},
        If you want to create an untyped form,
        you will have to use {% <<a_api|module Eliom_content.Html.D.Raw>> %}
        otherwise, use the form module.
        For more information,
        see {% <<a_manual chapter="server-links" fragment="forms"|the manual>> %}. *)

    val node : 'a elt React.signal Eliom_client_value.t -> 'a elt
    (** Function [node s] create an HTML5 [elt] from a signal [s].
        The resulting HTML5 [elt] can then be used like any other HTML5 [elt] *)

    val filter_attrib : 'a attrib -> bool React.signal -> 'a attrib
    (** [filter_attrib att on] returns an attrib that
        behave like [att] when [on] is [true]
        and behave like if there was no attribute when [on] is [false] *)

    (** Cf. {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
    module Raw :
      Html_sigs.Make(Eliom_content_core.Xml_wed)(Svg.R.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw
  end

  (** Creation of HTML5 content from client-side values.
      This module is available on client side only to make possible
      to use C-nodes in shared sections.
  *)
  module C : sig
    (** {2 Content injection} *)

    val node : ?init:'a D.elt -> 'a elt Eliom_client_value.t -> 'a D.elt
    (** Those two functions are the identity on client-side
        (the [init] argument is ignored).
        See Eliom manual for more detail on
        {% <<a_manual chapter="clientserver-html" fragment="inject" | Dom & Client-values >>%}. *)

    val attr : ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib
  end

  (** Node identifiers *)
  module Id : sig
    type +'a id
    (** The type of global HTML5 element identifier. *)

    val new_elt_id : ?global:bool -> unit -> 'a id
    (** The function [new_elt_id ()] creates a new global HTML5 element
        identifier (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="clientserver-html"
        fragment="global"|global element>>%}).*)

    val create_named_elt : id:'a id -> 'a elt -> 'a elt
    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be sent to client with the reference
        [id]. *)

    val create_global_elt : 'a elt -> 'a elt
    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)

    val create_request_elt : ?reset:bool -> 'a elt -> 'a elt
    (** [create_request_elt ?reset elt] creates a referable copy of
        [elt]. If [~reset = true] is provided (default: false), a new
        ID is created even if [elt] has an ID already. *)

    val get_element : 'a id -> 'a elt option
    (** [get_element id] returns
        the HTML element in the DOM with the given [id] if it exists. *)
  end

  module Custom_data : sig
    type 'a t
    (** Custom data with values of type ['a]. *)

    val create :
       name:string
      -> ?default:'a
      -> to_string:('a -> string)
      -> of_string:(string -> 'a)
      -> unit
      -> 'a t
    (** Create a custom data field by providing string conversion functions.
        If the [default] is provided, calls to {% <<a_api project="eliom" subproject="client" |
        val Eliom_content.Html.Custom_data.get_dom>> %} return that instead of throwing an
        exception [Not_found].  *)

    val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t
    (** Create a custom data from a Json-deriving type.  *)

    val attrib : 'a t -> 'a -> [> `User_data] attrib
    (** [attrib my_data value ] creates a HTML5 attribute for the custom-data
        type [my_data] with value [value] for injecting it into an a HTML5 tree
        ({% <<a_api | type Eliom_content.Html.elt >> %}). *)

    val get_dom : Dom_html.element Js.t -> 'a t -> 'a
    val set_dom : Dom_html.element Js.t -> 'a t -> 'a -> unit
  end

  module To_dom : Js_of_ocaml_tyxml.Tyxml_cast_sigs.TO with type 'a elt = 'a elt
  (** Conversion from HTML5 [elt]s to Javascript DOM elements ([<:] {% <<a_api
      project="js_of_ocaml"| class Js_of_ocaml.Dom_html.element >> %}).
      One conversion function per source type (stressed by the [of_] prefix). *)

  (** DOM-like manipulation functions.

      In this module, all the functions apply only to HTML5 element with
      {% <<a_manual chapter="clientserver-html" fragment="unique"|Dom semantics>>
      %}.
  *)
  module Manip : sig
    val appendChild : ?before:'a elt -> 'b elt -> 'c elt -> unit
    (** [appendChild e1 e2] inserts the element [e2] as last
        child of [e1]. If the optional parameter [~before:e3] is present
        and if [e3] is a child of [e1], then [e2] is inserted before [e3]
        in the list of [e1] children. *)

    val appendToBody : ?before:'a elt -> 'c elt -> unit
    (** Append to the body of the document. *)

    val appendChildren : ?before:'a elt -> 'b elt -> 'c elt list -> unit
    (** [appendChildren e1 elts] inserts [elts] as last children
        of [e1]. If the optional parameter [~before:e3] is present and if
        [e3] is a child of [e1], then [elts] are inserted before [e3] in
        the list of [e1] children. *)

    val insertFirstChild : 'b elt -> 'c elt -> unit
    (** [insertFirstChild p c] inserts [c] as first child of [p] *)

    val nth : 'a elt -> int -> 'b elt option
    (** [nth e n] returns the nth child of [e] (first is 0) *)

    val childLength : 'a elt -> int
    (** [childLength e] returns the number of children of [e] *)

    val removeChild : 'a elt -> 'b elt -> unit
    (** The function [removeChild e1 e2] removes for [e2] from the list of
        [e1] children. *)

    val replaceChild : 'a elt -> 'b elt -> 'c elt -> unit
    (** The function [replace e1 e2 e3] replaces for [e2] by [e3] in the
        list of [e1] children. *)

    val removeChildren : 'a elt -> unit
    (** The function [removeChildren e1] removes [e1] children. *)

    val removeSelf : 'a elt -> unit
    (** [removeSelf e] removes element e from the DOM. *)

    val replaceChildren : 'a elt -> 'b elt list -> unit
    (** The function [replaceChildren e1 elts] replaces all the children of
        [e1] by [elt]. *)

    val parentNode : 'a elt -> 'b elt option
    (** [parentNode elt] returns the parent of [elt], if any. *)

    val nextSibling : 'a elt -> 'b elt option
    (** [nextSibling elt] returns the next element that has the same parent,
        if [elt] is not the last. *)

    val previousSibling : 'a elt -> 'b elt option
    (** [previousSibling elt] returns the previous element
        that has the same parent,
        if [elt] is not the first. *)

    val insertBefore : before:'a elt -> 'b elt -> unit
    (** [insertBefore ~before elt] insert [elt] before [before]. *)

    val insertAfter : after:'a elt -> 'b elt -> unit
    (** [insertAfter ~after elt] insert [elt] after [after]. *)

    val replaceSelf : 'a elt -> 'b elt -> unit
    (** [replaceSelf elt1 elt2] replaces [elt1] by [elt2]. *)

    val children : 'a elt -> 'b elt list
    (** [children elt] returns the list of html children of [elt]. *)

    val addEventListener :
       ?capture:bool
      -> 'a elt
      -> (#Dom_html.event as 'b) Js.t Dom_html.Event.typ
      -> ('a elt -> 'b Js.t -> bool)
      -> Dom_html.event_listener_id
    (** The function [addEventListener elt evt handler] attach the
        [handler] for the event [evt] on the element [elt]. See the
        Js_of_ocaml manual, for a list of {% <<a_api project="js_of_ocaml"
        text="available events"| module Js_of_ocaml.Dom_html.Event >>%}. *)

    (** Dom manipulation by element identifier. *)
    module Named : sig
      (** The module [Named] defines the same functions as
          [Eliom_dom]. They take as parameter an element identifier
          instead of an element with Dom semantics. Those functions only
          works if the element is available in the application (sent in
          the page or along the page). If the element is not available,
          those functions raise with [Not_found]. *)

      val appendChild : ?before:'a elt -> 'b Id.id -> 'c elt -> unit
      (** see [appendChild] *)

      val appendChildren : ?before:'a elt -> 'b Id.id -> 'c elt list -> unit
      (** see [appendChildren] *)

      val removeChild : 'a Id.id -> 'b elt -> unit
      (** see [removeChild] *)

      val replaceChild : 'a Id.id -> 'b elt -> 'c elt -> unit
      (** see [replaceChild] *)

      val removeChildren : 'a Id.id -> unit
      (** see [removeChildren] *)

      val replaceChildren : 'a Id.id -> 'b elt list -> unit
      (** see [replaceChildren] *)

      val addEventListener :
         ?capture:bool
        -> 'a Id.id
        -> (#Dom_html.event as 'b) Js.t Dom_html.Event.typ
        -> ('a elt -> 'b Js.t -> bool)
        -> Dom_html.event_listener_id
      (** see [addEventListener] *)
    end

    val scrollIntoView : ?bottom:bool -> 'a elt -> unit
    (** The function [scrollIntoView elt] scroll the page to a position
        where [elt] is displayed at the top of the window. If the optional
        parameter [~bottom:true] is present, the page is scrolled to a
        position where [elt] is displayed at the bottom of the window. *)

    (**/**)

    val childNodes : 'a elt -> Dom.node Js.t list
    val childElements : 'a elt -> Dom.element Js.t list

    (**/**)

    (*
       val get_custom_data : _ elt -> 'a Custom_data.t -> 'a
    val set_custom_data : _ elt -> 'a Custom_data.t -> 'a -> unit
    *)

    module Class : sig
      val contain : 'a elt -> string -> bool
      val remove : 'a elt -> string -> unit
      val removes : 'a elt -> string list -> unit
      val add : 'a elt -> string -> unit
      val adds : 'a elt -> string list -> unit
      val replace : 'a elt -> string -> string -> unit
      val clear : 'a elt -> unit
      val toggle : 'a elt -> string -> unit
      val toggle2 : 'a elt -> string -> string -> unit
    end

    module Elt : sig
      val body : unit -> [`Body] elt
    end

    module Ev : sig
      type ('a, 'b) ev = 'a elt -> ('b Js.t -> bool) -> unit
      type ('a, 'b) ev_unit = 'a elt -> ('b Js.t -> unit) -> unit

      val onkeyup : ('a, Dom_html.keyboardEvent) ev
      val onkeydown : ('a, Dom_html.keyboardEvent) ev
      val onmouseup : ('a, Dom_html.mouseEvent) ev
      val onmousedown : ('a, Dom_html.mouseEvent) ev
      val onmouseout : ('a, Dom_html.mouseEvent) ev
      val onmouseover : ('a, Dom_html.mouseEvent) ev
      val onclick : ('a, Dom_html.mouseEvent) ev
      val ondblclick : ('a, Dom_html.mouseEvent) ev
      val onload : ('a, Dom_html.event) ev
      val onerror : ('a, Dom_html.event) ev
      val onabort : ('a, Dom_html.event) ev
      val onfocus : ('a, Dom_html.event) ev
      val onblur : ('a, Dom_html.event) ev
      val onfocus_textarea : ('a, Dom_html.event) ev
      val onblur_textarea : ('a, Dom_html.event) ev
      val onscroll : ('a, Dom_html.event) ev
      val onreturn : ('a, Dom_html.keyboardEvent) ev_unit
      val onchange : ('a, Dom_html.event) ev
      val onchange_select : ('a, Dom_html.event) ev
      val onbeforetoggle : ('a, Dom_html.toggleEvent) ev
      val ontoggle : ('a, Dom_html.toggleEvent) ev
    end

    module Attr : sig
      val clientWidth : 'a elt -> int
      val clientHeight : 'a elt -> int
      val offsetWidth : 'a elt -> int
      val offsetHeight : 'a elt -> int
      val clientLeft : 'a elt -> int
      val clientTop : 'a elt -> int
    end

    (** Read the CSS properties of DOM elements. *)
    module Css : sig
      val background : 'a elt -> string
      val backgroundAttachment : 'a elt -> string
      val backgroundColor : 'a elt -> string
      val backgroundImage : 'a elt -> string
      val backgroundPosition : 'a elt -> string
      val backgroundRepeat : 'a elt -> string
      val border : 'a elt -> string
      val borderBottom : 'a elt -> string
      val borderBottomColor : 'a elt -> string
      val borderBottomStyle : 'a elt -> string
      val borderBottomWidth : 'a elt -> string
      val borderBottomWidthPx : 'a elt -> int
      val borderCollapse : 'a elt -> string
      val borderColor : 'a elt -> string
      val borderLeft : 'a elt -> string
      val borderLeftColor : 'a elt -> string
      val borderLeftStyle : 'a elt -> string
      val borderLeftWidth : 'a elt -> string
      val borderLeftWidthPx : 'a elt -> int
      val borderRight : 'a elt -> string
      val borderRightColor : 'a elt -> string
      val borderRightStyle : 'a elt -> string
      val borderRightWidth : 'a elt -> string
      val borderRightWidthPx : 'a elt -> int
      val borderSpacing : 'a elt -> string
      val borderStyle : 'a elt -> string
      val borderTop : 'a elt -> string
      val borderTopColor : 'a elt -> string
      val borderTopStyle : 'a elt -> string
      val borderTopWidth : 'a elt -> string
      val borderTopWidthPx : 'a elt -> int
      val borderWidth : 'a elt -> string
      val bottom : 'a elt -> string
      val captionSide : 'a elt -> string
      val clear : 'a elt -> string
      val clip : 'a elt -> string
      val color : 'a elt -> string
      val content : 'a elt -> string
      val counterIncrement : 'a elt -> string
      val counterReset : 'a elt -> string
      val cssFloat : 'a elt -> string
      val cssText : 'a elt -> string
      val cursor : 'a elt -> string
      val direction : 'a elt -> string
      val display : 'a elt -> string
      val emptyCells : 'a elt -> string
      val font : 'a elt -> string
      val fontFamily : 'a elt -> string
      val fontSize : 'a elt -> string
      val fontStyle : 'a elt -> string
      val fontVariant : 'a elt -> string
      val fontWeight : 'a elt -> string
      val height : 'a elt -> string
      val heightPx : 'a elt -> int
      val left : 'a elt -> string
      val leftPx : 'a elt -> int
      val letterSpacing : 'a elt -> string
      val lineHeight : 'a elt -> string
      val listStyle : 'a elt -> string
      val listStyleImage : 'a elt -> string
      val listStylePosition : 'a elt -> string
      val listStyleType : 'a elt -> string
      val margin : 'a elt -> string
      val marginBottom : 'a elt -> string
      val marginBottomPx : 'a elt -> int
      val marginLeft : 'a elt -> string
      val marginLeftPx : 'a elt -> int
      val marginRight : 'a elt -> string
      val marginRightPx : 'a elt -> int
      val marginTop : 'a elt -> string
      val marginTopPx : 'a elt -> int
      val maxHeight : 'a elt -> string
      val maxHeightPx : 'a elt -> int
      val maxWidth : 'a elt -> string
      val maxWidthPx : 'a elt -> int
      val minHeight : 'a elt -> string
      val minHeightPx : 'a elt -> int
      val minWidth : 'a elt -> string
      val minWidthPx : 'a elt -> int
      val opacity : 'a elt -> string option
      val outline : 'a elt -> string
      val outlineColor : 'a elt -> string
      val outlineOffset : 'a elt -> string
      val outlineStyle : 'a elt -> string
      val outlineWidth : 'a elt -> string
      val overflow : 'a elt -> string
      val overflowX : 'a elt -> string
      val overflowY : 'a elt -> string
      val padding : 'a elt -> string
      val paddingBottom : 'a elt -> string
      val paddingBottomPx : 'a elt -> int
      val paddingLeft : 'a elt -> string
      val paddingLeftPx : 'a elt -> int
      val paddingRight : 'a elt -> string
      val paddingRightPx : 'a elt -> int
      val paddingTop : 'a elt -> string
      val paddingTopPx : 'a elt -> int
      val pageBreakAfter : 'a elt -> string
      val pageBreakBefore : 'a elt -> string
      val position : 'a elt -> string
      val right : 'a elt -> string
      val rightPx : 'a elt -> int
      val tableLayout : 'a elt -> string
      val textAlign : 'a elt -> string
      val textDecoration : 'a elt -> string
      val textIndent : 'a elt -> string
      val textTransform : 'a elt -> string
      val top : 'a elt -> string
      val topPx : 'a elt -> int
      val verticalAlign : 'a elt -> string
      val visibility : 'a elt -> string
      val whiteSpace : 'a elt -> string
      val width : 'a elt -> string
      val widthPx : 'a elt -> int
      val wordSpacing : 'a elt -> string
      val zIndex : 'a elt -> string
    end

    (** Modify the CSS properties of DOM elements. *)
    module SetCss : sig
      val background : 'a elt -> string -> unit
      val backgroundAttachment : 'a elt -> string -> unit
      val backgroundColor : 'a elt -> string -> unit
      val backgroundImage : 'a elt -> string -> unit
      val backgroundPosition : 'a elt -> string -> unit
      val backgroundRepeat : 'a elt -> string -> unit
      val border : 'a elt -> string -> unit
      val borderBottom : 'a elt -> string -> unit
      val borderBottomColor : 'a elt -> string -> unit
      val borderBottomStyle : 'a elt -> string -> unit
      val borderBottomWidth : 'a elt -> string -> unit
      val borderBottomWidthPx : 'a elt -> int -> unit
      val borderCollapse : 'a elt -> string -> unit
      val borderColor : 'a elt -> string -> unit
      val borderLeft : 'a elt -> string -> unit
      val borderLeftColor : 'a elt -> string -> unit
      val borderLeftStyle : 'a elt -> string -> unit
      val borderLeftWidth : 'a elt -> string -> unit
      val borderLeftWidthPx : 'a elt -> int -> unit
      val borderRight : 'a elt -> string -> unit
      val borderRightColor : 'a elt -> string -> unit
      val borderRightStyle : 'a elt -> string -> unit
      val borderRightWidth : 'a elt -> string -> unit
      val borderRightWidthPx : 'a elt -> int -> unit
      val borderSpacing : 'a elt -> string -> unit
      val borderStyle : 'a elt -> string -> unit
      val borderTop : 'a elt -> string -> unit
      val borderTopColor : 'a elt -> string -> unit
      val borderTopStyle : 'a elt -> string -> unit
      val borderTopWidth : 'a elt -> string -> unit
      val borderTopWidthPx : 'a elt -> int -> unit
      val borderWidth : 'a elt -> string -> unit
      val bottom : 'a elt -> string -> unit
      val bottomPx : 'a elt -> int -> unit
      val captionSide : 'a elt -> string -> unit
      val clear : 'a elt -> string -> unit
      val clip : 'a elt -> string -> unit
      val color : 'a elt -> string -> unit
      val content : 'a elt -> string -> unit
      val counterIncrement : 'a elt -> string -> unit
      val counterReset : 'a elt -> string -> unit
      val cssFloat : 'a elt -> string -> unit
      val cssText : 'a elt -> string -> unit
      val cursor : 'a elt -> string -> unit
      val direction : 'a elt -> string -> unit
      val display : 'a elt -> string -> unit
      val emptyCells : 'a elt -> string -> unit
      val font : 'a elt -> string -> unit
      val fontFamily : 'a elt -> string -> unit
      val fontSize : 'a elt -> string -> unit
      val fontStyle : 'a elt -> string -> unit
      val fontVariant : 'a elt -> string -> unit
      val fontWeight : 'a elt -> string -> unit
      val height : 'a elt -> string -> unit
      val heightPx : 'a elt -> int -> unit
      val left : 'a elt -> string -> unit
      val leftPx : 'a elt -> int -> unit
      val letterSpacing : 'a elt -> string -> unit
      val lineHeight : 'a elt -> string -> unit
      val listStyle : 'a elt -> string -> unit
      val listStyleImage : 'a elt -> string -> unit
      val listStylePosition : 'a elt -> string -> unit
      val listStyleType : 'a elt -> string -> unit
      val margin : 'a elt -> string -> unit
      val marginBottom : 'a elt -> string -> unit
      val marginBottomPx : 'a elt -> int -> unit
      val marginLeft : 'a elt -> string -> unit
      val marginLeftPx : 'a elt -> int -> unit
      val marginRight : 'a elt -> string -> unit
      val marginRightPx : 'a elt -> int -> unit
      val marginTop : 'a elt -> string -> unit
      val marginTopPx : 'a elt -> int -> unit
      val maxHeight : 'a elt -> string -> unit
      val maxHeightPx : 'a elt -> int -> unit
      val maxWidth : 'a elt -> string -> unit
      val maxWidthPx : 'a elt -> int -> unit
      val minHeight : 'a elt -> string -> unit
      val minHeightPx : 'a elt -> int -> unit
      val minWidth : 'a elt -> string -> unit
      val minWidthPx : 'a elt -> int -> unit
      val opacity : 'a elt -> string -> unit
      val outline : 'a elt -> string -> unit
      val outlineColor : 'a elt -> string -> unit
      val outlineOffset : 'a elt -> string -> unit
      val outlineStyle : 'a elt -> string -> unit
      val outlineWidth : 'a elt -> string -> unit
      val overflow : 'a elt -> string -> unit
      val overflowX : 'a elt -> string -> unit
      val overflowY : 'a elt -> string -> unit
      val padding : 'a elt -> string -> unit
      val paddingBottom : 'a elt -> string -> unit
      val paddingBottomPx : 'a elt -> int -> unit
      val paddingLeft : 'a elt -> string -> unit
      val paddingLeftPx : 'a elt -> int -> unit
      val paddingRight : 'a elt -> string -> unit
      val paddingRightPx : 'a elt -> int -> unit
      val paddingTop : 'a elt -> string -> unit
      val paddingTopPx : 'a elt -> int -> unit
      val pageBreakAfter : 'a elt -> string -> unit
      val pageBreakBefore : 'a elt -> string -> unit
      val position : 'a elt -> string -> unit
      val right : 'a elt -> string -> unit
      val rightPx : 'a elt -> int -> unit
      val tableLayout : 'a elt -> string -> unit
      val textAlign : 'a elt -> string -> unit
      val textDecoration : 'a elt -> string -> unit
      val textIndent : 'a elt -> string -> unit
      val textTransform : 'a elt -> string -> unit
      val top : 'a elt -> string -> unit
      val topPx : 'a elt -> int -> unit
      val verticalAlign : 'a elt -> string -> unit
      val visibility : 'a elt -> string -> unit
      val whiteSpace : 'a elt -> string -> unit
      val width : 'a elt -> string -> unit
      val widthPx : 'a elt -> int -> unit
      val wordSpacing : 'a elt -> string -> unit
      val zIndex : 'a elt -> string -> unit
    end
  end

  module Of_dom : Js_of_ocaml_tyxml.Tyxml_cast_sigs.OF with type 'a elt = 'a elt
  (** Conversion functions from DOM nodes ({% <<a_api project="js_of_ocaml"| type Js_of_ocaml.Dom_html.element>> %} {% <<a_api
      project="js_of_ocaml"| type Js_of_ocaml.Js.t>> %}) to Eliom nodes ({% <<a_api | type Eliom_content.Html.elt>> %}). *)
end

val force_link : unit

(**/**)

val set_client_fun :
   ?app:string
  -> service:('a, 'b, _, _, _, _, _, _, _, _, _) Eliom_service.t
  -> ('a -> 'b -> Eliom_service.result Lwt.t)
  -> unit

val set_form_error_handler : (unit -> bool Lwt.t) -> unit
(** With [set_form_error_handler f], [f] becomes the action to be
    called when we are unable to call a client-side service due to
    invalid form data.

    If the handler returns [true], nothing happens.

    If the handler returns [false], we proceed to call the server-side
    service.

    The default handler throws an exception (via [Lwt.fail_with]). *)
