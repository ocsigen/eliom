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

(** XML building and deconstructing. Cf. {% <<a_api subproject="server" |
    module Eliom_content_core.Xml >> %}. *)

type content_ns = [ `HTML5 | `SVG ]

module Xml : sig

  type uri = string
  val uri_of_string : uri -> string
  val string_of_uri : string -> uri
  val uri_of_fun : (unit -> string) -> uri

  type aname = string
  type attrib

  type -'a caml_event_handler =
    | CE_registered_closure of string * ((#Dom_html.event as 'a) Js.t -> unit) Eliom_lib.Client_value_server_repr.t
    | CE_client_closure of ('a Js.t -> bool)
    | CE_call_service of
        ([ `A | `Form_get | `Form_post] * (bool * string list) option * string option) option Eliom_lazy.request

  type event_handler =
    | Raw of string
    | Caml of Dom_html.event caml_event_handler

  type ename = string
  type elt
  type 'a wrap = 'a
  type econtent = private
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list

  (**/**)

  val event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option) option Eliom_lazy.request -> event_handler
  val event_handler_of_function : (Dom_html.event Js.t -> unit) -> event_handler

  type separator = Space | Comma
  type acontent = private
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list

  val acontent : attrib -> acontent

  type racontent =
    | RA of acontent
    | RAReact of acontent option React.signal
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  val racontent : attrib -> racontent

  val aname : attrib -> aname

  val float_attrib : aname -> float -> attrib
  val int_attrib : aname -> int -> attrib
  val string_attrib : aname -> string -> attrib
  val space_sep_attrib : aname -> string list -> attrib
  val comma_sep_attrib : aname -> string list -> attrib
  val event_handler_attrib : aname -> event_handler -> attrib
  val uri_attrib : aname -> uri -> attrib
  val uris_attrib : aname -> uri list -> attrib

  val content : elt -> econtent

  val pcdata : string -> elt
  val encodedpcdata : string -> elt
  val entity : string -> elt

  val empty : unit -> elt
  val comment : string -> elt

  val leaf : ?a:(attrib list) -> ename -> elt
  val node : ?a:(attrib list) -> ename -> elt list -> elt
  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt

  val cdata : string -> elt
  val cdata_script : string -> elt
  val cdata_style : string -> elt

  type node_id =
    | NoId
    | ProcessId of string
    | RequestId of string
  val string_of_node_id : node_id -> string
  val make : ?id:node_id -> econtent -> elt
  val make_dom : ?id:node_id -> Dom.node Js.t -> elt
  val make_lazy : ?id:node_id -> elt lazy_t -> elt
  val force_lazy : elt -> unit
  val make_react : ?id:node_id -> elt React.signal -> elt

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt
  val get_node_id : elt -> node_id

  type node =
    | DomNode of Dom.node Js.t
    | TyXMLNode of econtent
    | ReactNode of elt React.signal

  val get_node : elt -> node
  val set_dom_node : elt -> Dom.node Js.t -> unit

  module ClosureMap : Map.S with type key = string
  type event_handler_table =
    ((Dom_html.event Js.t -> unit) Eliom_lib.Client_value_server_repr.t) ClosureMap.t

end

(** Building SVG tree. *)
module Svg : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type 'a wrap = 'a
  type +'a attrib
  type uri = Xml.uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      Svg_sigs.T >> %}. *)
  module F : sig
    module Raw : Svg_sigs.T with type Xml.uri = Xml.uri
                            and type Xml.event_handler = Xml.event_handler
                            and type Xml.attrib = Xml.attrib
                            and type Xml.elt = Xml.elt
                              with type 'a elt = 'a elt
                              and type 'a Xml.wrap = 'a
                              and type 'a wrap = 'a
                              and type 'a attrib = 'a attrib
                              and type uri = uri

    (** See {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
    include module type of Raw

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_svg_event_handler.mli"

  end

  (** {2 DOM semantics} *)

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
  module D : sig

    module Raw : Svg_sigs.T with type Xml.uri = Xml.uri
                            and type Xml.event_handler = Xml.event_handler
                            and type Xml.attrib = Xml.attrib
                            and type Xml.elt = Xml.elt
                              with type 'a elt = 'a elt
                              and type 'a Xml.wrap = 'a
                              and type 'a wrap = 'a
                              and type 'a attrib = 'a attrib
                              and type uri = uri

    (** See {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
    include module type of Raw

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_svg_event_handler.mli"

  end

  (** {2 Reactive DOM} *)

  (** Typed interface for building valid reactive SVG tree. *)
  module R : sig

    module Raw : Svg_sigs.T with type Xml.uri = Xml.uri
                            and type Xml.event_handler = Xml.event_handler
                            and type Xml.attrib = Xml.attrib
                            and type Xml.elt = Xml.elt
                              with type 'a elt = 'a elt
                              and type 'a Xml.wrap = 'a React.signal
                              and type 'a wrap = 'a React.signal
                              and type 'a attrib = 'a attrib
                              and type uri = uri

    (** See {% <<a_api project="tyxml" | module type Svg_sigs.T >> %}. *)
    include module type of Raw

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_svg_event_handler.mli"

  end

  (** {2 Global node} *)

  module Id : sig
    (** The type of global SVG element identifier. *)
    type +'a id

    (** See {!Eliom_content.Html5.Id.new_elt_id} *)
    val new_elt_id: ?global:bool -> unit -> 'a id
    (** See {!Eliom_content.Html5.Id.create_named_elt} *)
    val create_named_elt: id:'a id -> 'a elt -> 'a elt
    (** See {!Eliom_content.Html5.Id.create_global_elt} *)
    val create_global_elt: 'a elt -> 'a elt

    (**/**)
    val string_of_id : 'a id -> string
  end

  module Of_dom : sig
    val of_element: Dom_html.element Js.t -> 'a elt
  end

end

(** Building Html5 tree. *)
module Html5 : sig

  (** See the Eliom manual for more information on {% <<a_manual
      chapter="clientserver-html" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type 'a wrap = 'a
  type +'a attrib
  type uri = Xml.uri

  (** Typed interface for building valid HTML5 tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      Html5_sigs.T >> %}. *)
  module F : sig

    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with module Svg := Svg.F.Raw
                   with type 'a elt = 'a elt
                   and type 'a Xml.wrap = 'a
                   and type 'a wrap = 'a
                   and type 'a attrib = 'a attrib
                   and type uri = uri

    (** See {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus

  end


  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)
  module D: sig

    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   and module Svg := Svg.D.Raw
                   and type 'a elt = 'a elt
                   and type 'a Xml.wrap = 'a
                   and type 'a wrap = 'a
                   and type 'a attrib = 'a attrib
                   and type uri = uri
    include module type of Raw (*BB TODO Hide untyped [input]. *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< Html5_types.form_attrib ], [< Html5_types.form_content_fun ], [> Html5_types.form ]) lazy_plus

  end

  (** Typed interface for building valid HTML5 tree from
      {{: http://erratique.ch/software/react} React } signals.
      HTML5's trees are automatically updated whenever
      corresponding signals change.

      {% <<a_api project="tyxml" | module type Html5_sigs.T >> %}. *)

  module R: sig

    (** the function [node s] create an HTML5 [elt] from a signal [s].
    The resulting HTML5 [elt] can then be used like anyother HTML5 [elt] *)
    val node : 'a elt React.signal -> 'a elt

    module Raw : Html5_sigs.T
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   and module Svg := Svg.D.Raw
                   and type 'a elt = 'a elt
                   and type 'a Xml.wrap = 'a React.signal
                   and type 'a wrap = 'a React.signal
                   and type 'a attrib = 'a attrib
                   and type uri = uri
    include module type of Raw
  end


  (** Node identifiers *)
  module Id : sig

    (** The type of global HTML5 element identifier. *)
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

    (**/**)
    val string_of_id : 'a id -> string
  end

  (** Type-safe custom data for HTML5. See the {% <<a_manual chapter="clientserver-html"
      fragment="custom_data"|examples in the manual>> %}. *)
  module Custom_data : sig

    (** Custom data with values of type ['a]. *)
    type 'a t

    (** Create a custom data field by providing string conversion functions.
        If the [default] is provided, calls to {% <<a_api project="eliom" subproject="client" |
        val Eliom_content.Html5.Custom_data.get_dom>> %} return that instead of throwing an
        exception [Not_found].  *)
    val create : name:string -> ?default:'a -> to_string:('a -> string) -> of_string:(string -> 'a) -> unit -> 'a t

    (** Create a custom data from a Json-deriving type.  *)
    val create_json : name:string -> ?default:'a -> 'a Deriving_Json.t -> 'a t

    (** [attrib my_data value ] creates a HTML5 attribute for the custom-data
        type [my_data] with value [value] for injecting it into an a HTML5 tree
        ({% <<a_api | type Eliom_content.Html5.elt >> %}). *)
    val attrib : 'a t -> 'a -> [> | `User_data ] attrib

    (** [get_dom element custom_data] gets the [custom_data] from a JavaScript [element]
        ({% <<a_api project="js_of_ocaml"|class type Dom_html.element>> %}).
        @return The value encoded in the respective custom data attribute of [element], or the default value, if any.
        @raise Not_found if the element does not contain the respective custom
          data attribute and the [custom_data] was created without [default].
      *)
    val get_dom : Dom_html.element Js.t -> 'a t -> 'a

    (** [set_dom element custom_data value] sets the custom data attribute for
        [custom_data] of an JavaScript [element]
        ({% <<a_api project="js_of_ocaml"|class type Dom_html.element>> %})
        to [value]. *)
    val set_dom : Dom_html.element Js.t -> 'a t -> 'a -> unit
  end

  (** Conversion of Javascript DOM elements to HTML5 elts (with DOM semantics of course).
      One conversion function per source type (stressed by the [of_] prefix). *)
  module Of_dom : sig
    val of_element : Dom_html.element Js.t -> 'a elt
    val of_html : Dom_html.htmlElement Js.t -> Html5_types.html elt
    val of_head : Dom_html.headElement Js.t -> Html5_types.head elt
    val of_link : Dom_html.linkElement Js.t -> Html5_types.link elt
    val of_title : Dom_html.titleElement Js.t -> Html5_types.title elt
    val of_meta : Dom_html.metaElement Js.t -> Html5_types.meta elt
    val of_base : Dom_html.baseElement Js.t -> Html5_types.base elt
    val of_style : Dom_html.styleElement Js.t -> Html5_types.style elt
    val of_body : Dom_html.bodyElement Js.t -> Html5_types.body elt
    val of_form : Dom_html.formElement Js.t -> Html5_types.form elt
    val of_optGroup : Dom_html.optGroupElement Js.t -> Html5_types.optgroup elt
    val of_option : Dom_html.optionElement Js.t -> Html5_types.selectoption elt
    val of_select : Dom_html.selectElement Js.t -> Html5_types.select elt
    val of_input : Dom_html.inputElement Js.t -> Html5_types.input elt
    val of_textArea : Dom_html.textAreaElement Js.t -> Html5_types.textarea elt
    val of_button : Dom_html.buttonElement Js.t -> Html5_types.button elt
    val of_label : Dom_html.labelElement Js.t -> Html5_types.label elt
    val of_fieldSet : Dom_html.fieldSetElement Js.t -> Html5_types.fieldset elt
    val of_legend : Dom_html.legendElement Js.t -> Html5_types.legend elt
    val of_uList : Dom_html.uListElement Js.t -> Html5_types.ul elt
    val of_oList : Dom_html.oListElement Js.t -> Html5_types.ol elt
    val of_dList : Dom_html.dListElement Js.t -> [`Dl] elt
    val of_li : Dom_html.liElement Js.t -> Html5_types.li elt
    val of_div : Dom_html.divElement Js.t -> Html5_types.div elt
    val of_paragraph : Dom_html.paragraphElement Js.t -> Html5_types.p elt
    val of_heading : Dom_html.headingElement Js.t -> Html5_types.heading elt
    val of_quote : Dom_html.quoteElement Js.t -> Html5_types.blockquote elt
    val of_pre : Dom_html.preElement Js.t -> Html5_types.pre elt
    val of_br : Dom_html.brElement Js.t -> Html5_types.br elt
    val of_hr : Dom_html.hrElement Js.t -> Html5_types.hr elt
    val of_anchor : Dom_html.anchorElement Js.t -> 'a Html5_types.a elt
    val of_image : Dom_html.imageElement Js.t -> [`Img] elt
    val of_object : Dom_html.objectElement Js.t -> 'a Html5_types.object_ elt
    val of_param : Dom_html.paramElement Js.t -> Html5_types.param elt
    val of_area : Dom_html.areaElement Js.t -> Html5_types.area elt
    val of_map : Dom_html.mapElement Js.t -> 'a Html5_types.map elt
    val of_script : Dom_html.scriptElement Js.t -> Html5_types.script elt
    val of_tableCell : Dom_html.tableCellElement Js.t -> [ Html5_types.td | Html5_types.td ] elt
    val of_tableRow : Dom_html.tableRowElement Js.t -> Html5_types.tr elt
    val of_tableCol : Dom_html.tableColElement Js.t -> Html5_types.col elt
    val of_tableSection : Dom_html.tableSectionElement Js.t -> [ Html5_types.tfoot | Html5_types.thead | Html5_types.tbody ] elt
    val of_tableCaption : Dom_html.tableCaptionElement Js.t -> Html5_types.caption elt
    val of_table : Dom_html.tableElement Js.t -> Html5_types.table elt
    val of_canvas : Dom_html.canvasElement Js.t -> 'a Html5_types.canvas elt
    val of_iFrame : Dom_html.iFrameElement Js.t -> Html5_types.iframe elt
  end

  (**/**)

  val set_classes_of_elt : 'a elt -> 'a elt

end
