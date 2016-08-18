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

(** This module allows creating valid HTML content, or other XML formats. *)


(**
    XML tree manipulation within Eliom is based on the TyXML library
    but Eliom is using a custom representation for XML values (see
    {!Xml}). Then, [Eliom_content] redefines the two high level
    interfaces ({!Svg}, {!Html}) that are provided by
    TyXML for valid XML tree creation and printing.

    - If you want to generate typed HTML, use {!Eliom_content.Html},
    - If you want to write untyped html, use {!Eliom_content.Html_text},
    - If you want to generate typed svg, use {!Eliom_content.Svg}.

    Modules {!Eliom_content.Html}, {!Eliom_content.Svg} contain two
    sub-modules: {!Eliom_content.Html.F}, {!Eliom_content.Html.D}
    corresponding to tow different semantics.
    They also contain a module {!Eliom_content.Html.C} that allows to
    inject client-side content into server-side content.

    {5 Functional semantics}

    The [F] modules provides functions to create elements with {e f}unctional
    semantics: they are standard OCaml values.

    Use this module:
    - if your application does not have a client-side part
    (server-side generated Web site)
    - or if the client-side is not written with Eliom,
    - or if you do not need to use this node from the client-side program
    (no injection [%n] on this node)
    and want to avoid the extra attributes added by module [D].

    If you use a [F]-node [n] in an injection ([%n]),
    it is considered as any OCaml value, NOT precisely the copy you (possibly)
    inserted in the page. For example, [To_dom.of_element %n] will not refer
    to the element in the page, but create a new DOM node.


    {5 DOM semantics}

    The [D] module provides functions to create elements with {e D}OM semantics:
    Firstly, they behave like DOM nodes, e.g. they can only be added once to the
    DOM tree even when appended several times.
    Secondly, those values have an identifier,
    which means they can be referred to
    on client side (by [%variable]) or used with the functions in
    {% <<a_api subproject="client"|module Eliom_content.Html.To_dom>> %} and
    {% <<a_api subproject="client"|module Eliom_content.Html.Manip>> %}.

    In case of doubt, always use [D]-nodes when you are writing a
    client-server Eliom app. You can also mix F-nodes and D-nodes.

   {5 Client-side value injection}

   The [C] modules provides functions to inject client-side elements and attributes
   into server-side content.

   {b Please read
   {% <<a_manual chapter="clientserver-html"|Eliom's manual>>%}
   to learn how to generate HTML. }

  *)

(** Low-level XML manipulation. *)
module Xml : sig

  (** {2 Base functions}
      See {% <<a_api project="tyxml" | module Xml_sigs.Iterable >> %}. *)

  include Xml_sigs.Iterable
    with type 'a wrap = 'a
     and type 'a list_wrap = 'a list
     and type event_handler =
           (Dom_html.event Js.t -> unit) Eliom_client_value.t
     and type mouse_event_handler =
           (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
     and type keyboard_event_handler =
           (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t

  (** {2 Unique nodes } *)

  (** Unique nodes are XML nodes that are manipulated 'by reference'
      when sent to the client part of an Eliom-application: the
      created element is allocated only one time in each instance of
      an application. See {% <<a_manual chapter="clientserver-html"
      fragment="unique" |the eliom manual>>%} for more
      details. *)

  (** {2 Event handlers } *)

  (** Values of type [caml_event_handler] represents event handler
      build with the [{{ ... }}] syntax (see the Eliom manual for more
      information on {% <<a_manual chapter="clientserver-html"
      fragment="syntax"|syntax extension>>%}). Such values are
      expected by functions like {!Eliom_content.Html.a_onclick}. *)
  type caml_event_handler

  (**/**)

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : ?reset:bool -> elt -> elt

  val uri_of_fun: (unit -> string) -> uri

  (* Concrete on client-side only. *)
  type node_id
  val get_node_id : elt -> node_id
  val make_event_handler_table : elt -> Eliom_runtime.RawXML.event_handler_table
  val make_client_attrib_table : elt -> Eliom_runtime.RawXML.client_attrib_table

  val caml_event_handler :
    (Dom_html.event Js.t -> unit) Eliom_client_value.t ->
    caml_event_handler

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
    | RAClient of string * attrib option * Eliom_lib.poly
                                           (* attrib client_value *)
  val racontent : attrib -> racontent

  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  (**/**)
  (** [Eliom_content.Xml.wrap page v] is like [Eliom_wrap.wrap v] but
      it makes sure that all [elt]s in [v] which are included in
      [page] are sent with empty content. This is safe because such
      elements will be taken from the DOM on the client either
      ways. *)
  val wrap : elt -> 'a -> 'a Eliom_wrap.wrapped_value

end

module Xml_shared : Xml_sigs.T
  with type 'a W.t = 'a Eliom_shared.React.S.t
   and type 'a W.tlist = 'a Eliom_shared.ReactiveData.RList.t
   and type event_handler =
         (Dom_html.event Js.t -> unit) Eliom_client_value.t
   and type mouse_event_handler =
         (Dom_html.mouseEvent Js.t -> unit) Eliom_client_value.t
   and type keyboard_event_handler =
         (Dom_html.keyboardEvent Js.t -> unit) Eliom_client_value.t

(** Building and pretty-printing valid SVG tree.
Information about Svg api can be found at {% <<a_api project="tyxml" | module Svg_sigs.T >> %}*)
module Svg : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type +'a attrib
  type 'a wrap = 'a
  type 'a list_wrap = 'a list
  type uri = Xml.uri

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module
      Svg_sigs.T >> %}. *)
  module F : sig

    (** See {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
    module Raw : Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

  end

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module Svg_sigs.T >> %}. *)
  module D : sig

    (** See {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
    module Raw : Svg_sigs.Make(Xml).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

  end

  (** Creation of SVG content from shared reactive signals and data
      ({% <<a_api project="eliom" subproject="server"|module Eliom_shared>> %}).
      For the operations provided, see
      {% <<a_api project="tyxml" | module Svg_sigs.T >> %}. *)
  module R : sig

    module Raw : Svg_sigs.Make(Xml_shared).T
      with type 'a elt = 'a elt
       and type 'a attrib = 'a attrib

    include module type of Raw

    (** [pcdata] is not implemented reactively for SVG. *)
    val pcdata : string Xml.W.t -> [> `Unimplemented ]

    (** [node s] produces an ['a elt] out of the shared reactive
        signal [s]. *)
    val node : 'a elt Eliom_shared.React.S.t -> 'a elt

  end

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

    val attr : ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib

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

  (** SVG printer.
      See {% <<a_api project="tyxml" | module Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type +'a elt := 'a elt
                                                  and type doc := F.doc


end





(** Building and printing valid HTML5 tree.
    Information about Html api can be found at
    {% <<a_api project="tyxml" | module Html_sigs.T >> %} .*)
module Html : sig

  (** See {% <<a_manual
      chapter="clientserver-html" fragment="unique"|
      more information on dom semantics vs. functional
      semantics>> %} in Eliom's manual
      for HTML5 tree manipulated by client/server application. *)

  type +'a elt
  type +'a attrib
  type uri = Xml.uri
  type 'a form_param

  (** Creation of {b F}unctional HTML5 content (copy-able but not
      referable, see also {% <<a_api|module Eliom_content>> %}). *)
  module F : sig

    (** {2 Content creation}

        See {% <<a_api project="tyxml" | module Html_sigs.T >> %}.
        If you want to create an untyped form, you will have to use {%
        <<a_api|module Eliom_content.Html.F.Raw>> %} otherwise, use
        Eliom form widgets.  For more information, see
        {{:http://ocsigen.org/howto/forms/}"how to make forms"} *)

    (** See {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
    module Raw : Html_sigs.Make(Xml)(Svg.F.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

    include Eliom_content_sigs.LINKS_AND_FORMS
      with type +'a elt := 'a elt
       and type +'a attrib := 'a attrib
       and type uri := uri
       and type ('a, 'b, 'c) star := ('a, 'b, 'c) star
       and type 'a form_param := 'a form_param

  end

  (** Creation of HTML content with {b D}OM semantics (referable, see
      also {% <<a_api|module Eliom_content>> %}). *)
  module D : sig

    (** {2 Content creation}

        See {% <<a_api project="tyxml" | module Html_sigs.T >> %}.
        If you want to create an untyped form, you will have to use {%
        <<a_api|module Eliom_content.Html.F.Raw>> %} otherwise, use
        Eliom form widgets.  For more information, see
        {{:http://ocsigen.org/howto/forms/}"how to make forms"} *)

    (** See {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
    module Raw : Html_sigs.Make(Xml)(Svg.D.Raw).T
      with type +'a elt = 'a elt
       and type +'a attrib = 'a attrib

    include module type of Raw

    include Eliom_content_sigs.LINKS_AND_FORMS
      with type +'a elt := 'a elt
       and type +'a attrib := 'a attrib
       and type uri := uri
       and type ('a, 'b, 'c) star := ('a, 'b, 'c) star
       and type 'a form_param := 'a form_param

  end

  (** Creation of HTML content from client-side values.  This makes
      possible to insert in server side generated pages some nodes
      that will be computed on client side (for example reactive
      nodes).  *)
  module C : sig

    (** {2 Content injection} *)

    (** See Eliom manual for more detail on {% <<a_manual
        chapter="clientserver-html" fragment="inject" | DOM &
        Client-values >>%}. *)

    (** [node e] is a server-side node corresponding to the
        client-side node [e] . [node e] can be used like any other
        server-side node.

        The implementation uses an initial placeholder node that is
        later replaced by the client node. By default, the placeholder
        node is [span]. The [~init] argument can be used to provide a
        custom placeholder node (e.g., one with the same tag as the
        client node). This can be useful in contexts where [span] is
        not allowed. *)
    val node :
      ?init:'a elt -> 'a elt Eliom_client_value.t -> 'a elt

    val attr :
      ?init:'a attrib -> 'a attrib Eliom_client_value.t -> 'a attrib

  end

  (** Node identifiers *)
  module Id : sig

    (** The type of global HTML element identifier. *)
    type +'a id

    (** The function [new_elt_id ()] creates a new global HTML element
        identifier (see the Eliom manual for more information on {%
        <<a_manual project="eliom" chapter="clientserver-html"
        fragment="global"|global element>>%}).*)
    val new_elt_id: ?global:bool -> unit -> 'a id

    (** The function [create_named_elt ~id elt] create a copy of the
        element [elt] that will be sent to client with the reference
        [id]. *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt

    (** The function [create_named_elt elt] is equivalent to
        [create_named_elt ~id:(new_elt_id ()) elt]. *)
    val create_global_elt: 'a elt -> 'a elt

    (** [create_request_elt ?reset elt] creates a referable copy of
        [elt]. If [~reset = true] is provided (default: false), a new
        ID is created even if [elt] has an ID already. *)
    val create_request_elt: ?reset:bool -> 'a elt -> 'a elt

    (* XXX: This function must be hidden in documentation but hidden rest of
     * file *)
    val have_id: 'a id -> 'b elt -> bool

  end

  (** Creation of HTML content from shared reactive signals and data
      ({% <<a_api project="eliom" subproject="server"|module Eliom_shared>> %}).
      For the operations provided, see
      {% <<a_api project="tyxml" | module Html_sigs.T >> %}. *)
  module R : sig

    include Html_sigs.Make(Xml_shared)(Svg.R.Raw).T
      with type 'a elt = 'a elt
       and type 'a attrib = 'a attrib

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

  end

  (** Type-safe custom data for HTML.
      See the {% <<a_manual chapter="clientserver-html"
      fragment="custom_data"|examples in the manual>> %}. *)
  module Custom_data : sig

    (** Custom data with values of type ['a]. *)
    type 'a t

    (** Create a custom data field by providing string conversion functions.
        If the [default] is provided, calls to {% <<a_api project="eliom" subproject="client" |
        val Eliom_content.Html.Custom_data.get_dom>> %} return that instead of throwing an
        exception [Not_found].  *)
    val create : name:string -> ?default:'a -> to_string:('a -> string) -> of_string:(string -> 'a) -> unit -> 'a t

    (** Create a custom data from a Json-deriving type.  *)
    val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t

    (** [attrib my_data value ] creates a HTML attribute for the custom-data
        type [my_data] with value [value] for injecting it into an a HTML tree
        ({% <<a_api | type Eliom_content.Html.elt >> %}). *)
    val attrib : 'a t -> 'a -> [> | `User_data ] attrib

  end

  (** {{:http://dev.w3.org/html5/html-xhtml-author-guide/}"Polyglot"} HTML printer.
     See {% <<a_api project="tyxml" | module Xml_sigs.Typed_simple_printer >> %}. *)
  module Printer : Xml_sigs.Typed_simple_printer with type +'a elt := 'a elt
                                                  and type doc := F.doc

end

(**/**)

val set_client_fun :
  ?app:string ->
  service:('a, 'b, _, _, _, _, _, _, _, _, _) Eliom_service.t ->
  ('a -> 'b -> unit Lwt.t) Eliom_client_value.t ->
  unit
