(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2011 Grégoire Henry
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

(** Pervasives module for Eliom-client: it extends the OCaml stdlib and should always be opened. *)

include module type of Ocsigen_lib_base
  with type poly = Ocsigen_lib.poly
  and type yesnomaybe = Ocsigen_lib_base.yesnomaybe
  and type ('a, 'b) leftright = ('a, 'b) Ocsigen_lib_base.leftright
  and type 'a Clist.t = 'a Ocsigen_lib_base.Clist.t
  and type 'a Clist.node = 'a Ocsigen_lib_base.Clist.node

include module type of Eliom_lib_base
  with type 'a client_expr = 'a Eliom_lib_base.client_expr

(** Pervasives module for Eliom extending stdlib, should always be opened. *)

exception Eliom_Internal_Error of string

type file_info

val to_json : ?typ:'a -> 'b -> string
val of_json : ?typ:'a -> string -> 'b

exception False

module Url : sig
  include module type of Url_base (* From ocsigenserver *)
  include module type of Url (* From js_of_ocaml *)
  val decode : string -> string
  val encode : ?plus:bool -> string -> string
  val make_encoded_parameters : (string * string) list -> string
  val split_path : string -> string list
  val get_ssl : string -> bool option
end

module String : sig
  include module type of String_base
  val remove_eols : string -> string
end

val debug : ('a, unit, string, unit) format4 -> 'a
val error : ('a, unit, string, 'b) format4 -> 'a
val debug_exn : ('a, unit, string, unit) format4 -> exn -> 'a
val jsdebug : 'a -> unit
val alert : ('a, unit, string, unit) format4 -> 'a
val jsalert : Js.js_string Js.t -> unit
val debug_var : string -> 'a -> unit

val lwt_ignore : ?message:string -> unit Lwt.t -> unit

val encode_form_value : 'a -> string
val unmarshal_js_var : string -> 'a

val encode_header_value : 'a -> string

(** XML building and deconstructing. *)
module XML : sig

  type uri = string
  val uri_of_string : uri -> string
  val string_of_uri : string -> uri
  val uri_of_fun : (unit -> string) -> uri

  type aname = string
  type attrib

  type -'a caml_event_handler =
    | CE_registered_closure of string * ((#Dom_html.event as 'a) Js.t -> unit) client_expr
    | CE_client_closure of ('a Js.t -> unit)
    | CE_call_service of
	([ `A | `Form_get | `Form_post] * (bool * string list) option * string option) option Eliom_lazy.request

  type event_handler =
    | Raw of string
    | Caml of Dom_html.event caml_event_handler

  type ename = string
  type elt
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
  val event_handler_of_function : (#Dom_html.event Js.t -> unit) -> event_handler

  (* Deprecated alias. *)
  val event_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option
      * string option ) option Eliom_lazy.request -> event_handler
  val event_of_function : ((#Dom_html.event Js.t as 'a) -> unit) -> ( 'a -> unit)

  type separator = Space | Comma
  type acontent = private
    | AFloat of float
    | AInt of int
    | AStr of string
    | AStrL of separator * string list
  val acontent : attrib -> acontent

  type racontent =
    | RA of acontent
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

  (* Deprecated alias. *)
  val event_attrib : aname -> event_handler -> attrib

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
  val make : ?id:node_id -> econtent -> elt
  val make_dom : ?id:node_id -> Dom.node Js.t -> elt

  val make_process_node : ?id:string -> elt -> elt
  val make_request_node : elt -> elt
  val get_node_id : elt -> node_id

  type node =
    | DomNode of Dom.node Js.t
    | TyXMLNode of econtent
  val get_node : elt -> node
  val set_dom_node : elt -> Dom.node Js.t -> unit

  module ClosureMap : Map.S with type key = string
  type event_handler_table = ((poly -> unit) client_expr) ClosureMap.t

end

(** Building SVG tree. *)
module SVG : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type 'a attrib
  type uri = XML.uri

  (** {2 Dom semantics} *)

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type SVG_sigs.T >> %}. *)
  module D: SVG_sigs.T with module XML := XML
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      SVG_sigs.T >> %}. *)
  module F : SVG_sigs.T with module XML := XML
		        and type 'a elt = 'a elt
		        and type 'a attrib = 'a attrib
		        and type uri = uri

  (** {2 Global node} *)

  (** The type of global SVG element identifier. *)
  type +'a id

  (** See {!Eliom_lib.HTML5.new_elt_id} *)
  val new_elt_id: ?global:bool -> unit -> 'a id
  (** See {!Eliom_lib.HTML5.create_named_elt} *)
  val create_named_elt: id:'a id -> 'a elt -> 'a elt
  (** See {!Eliom_lib.HTML5.create_global_elt} *)
  val create_global_elt: 'a elt -> 'a elt

end

(** Building HTML5 tree. *)
module HTML5 : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for SVG tree manipulated by client/server
      application. *)

  type +'a elt
  type +'a attrib
  type uri = XML.uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid HTML5 tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      HTML5_sigs.T >> %}. *)
  module F : sig

    (** See {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.F
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

    (** {2 Event handlers} *)

    (**/**)
    include "sigs/eliom_html5_event_handler_raw.mli"
    (**/**)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus
    (**/**)

  end

  (** {2 DOM semantics} *)

  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
  module D: sig

    include HTML5_sigs.T with module XML := XML
		         and module SVG := SVG.D
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

    (**/**)
    include "sigs/eliom_html5_event_handler_raw.mli"
    (**/**)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus
    (**/**)

  end

  (** {2 Global node} *)

  (** The type of global HTML5 element identifier. *)
  type +'a id

  (** The function [new_elt_id ()] creates a new HTML5 element
      identifier. (see the Eliom manual for more information on {%
      <<a_manual project="eliom" chapter="client"
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

  (** Conversion of Javascript DOM elements to HTML5 elts (with DOM semantics). *)
  module Of_dom : sig
    val element : Dom_html.element Js.t -> 'a elt
    val html : Dom_html.htmlElement Js.t -> HTML5_types.html elt
    val head : Dom_html.headElement Js.t -> HTML5_types.head elt
    val link : Dom_html.linkElement Js.t -> HTML5_types.link elt
    val title : Dom_html.titleElement Js.t -> HTML5_types.title elt
    val meta : Dom_html.metaElement Js.t -> HTML5_types.meta elt
    val base : Dom_html.baseElement Js.t -> HTML5_types.base elt
    val style : Dom_html.styleElement Js.t -> HTML5_types.style elt
    val body : Dom_html.bodyElement Js.t -> HTML5_types.body elt
    val form : Dom_html.formElement Js.t -> HTML5_types.form elt
    val optGroup : Dom_html.optGroupElement Js.t -> HTML5_types.optgroup elt
    val option : Dom_html.optionElement Js.t -> HTML5_types.selectoption elt
    val select : Dom_html.selectElement Js.t -> HTML5_types.select elt
    val input : Dom_html.inputElement Js.t -> HTML5_types.input elt
    val textArea : Dom_html.textAreaElement Js.t -> HTML5_types.textarea elt
    val button : Dom_html.buttonElement Js.t -> HTML5_types.button elt
    val label : Dom_html.labelElement Js.t -> HTML5_types.label elt
    val fieldSet : Dom_html.fieldSetElement Js.t -> HTML5_types.fieldset elt
    val legend : Dom_html.legendElement Js.t -> HTML5_types.legend elt
    val uList : Dom_html.uListElement Js.t -> HTML5_types.ul elt
    val oList : Dom_html.oListElement Js.t -> HTML5_types.ol elt
    val dList : Dom_html.dListElement Js.t -> [`Dl] elt
    val li : Dom_html.liElement Js.t -> HTML5_types.li elt
    val div : Dom_html.divElement Js.t -> HTML5_types.div elt
    val paragraph : Dom_html.paragraphElement Js.t -> HTML5_types.p elt
    val heading : Dom_html.headingElement Js.t -> HTML5_types.heading elt
    val quote : Dom_html.quoteElement Js.t -> HTML5_types.blockquote elt
    val pre : Dom_html.preElement Js.t -> HTML5_types.pre elt
    val br : Dom_html.brElement Js.t -> HTML5_types.br elt
    val hr : Dom_html.hrElement Js.t -> HTML5_types.hr elt
    val anchor : Dom_html.anchorElement Js.t -> 'a HTML5_types.a elt
    val image : Dom_html.imageElement Js.t -> [`Img] elt
    val object_ : Dom_html.objectElement Js.t -> 'a HTML5_types.object_ elt
    val param : Dom_html.paramElement Js.t -> HTML5_types.param elt
    val area : Dom_html.areaElement Js.t -> HTML5_types.area elt
    val map : Dom_html.mapElement Js.t -> 'a HTML5_types.map elt
    val script : Dom_html.scriptElement Js.t -> HTML5_types.script elt
    val tableCell : Dom_html.tableCellElement Js.t -> [ HTML5_types.td | HTML5_types.td ] elt
    val tableRow : Dom_html.tableRowElement Js.t -> HTML5_types.tr elt
    val tableCol : Dom_html.tableColElement Js.t -> HTML5_types.col elt
    val tableSection : Dom_html.tableSectionElement Js.t -> [ HTML5_types.tfoot | HTML5_types.thead | HTML5_types.tbody ] elt
    val tableCaption : Dom_html.tableCaptionElement Js.t -> HTML5_types.caption elt
    val table : Dom_html.tableElement Js.t -> HTML5_types.table elt
    val canvas : Dom_html.canvasElement Js.t -> 'a HTML5_types.canvas elt
    val iFrame : Dom_html.iFrameElement Js.t -> HTML5_types.iframe elt
  end

  module To_dom : sig
  end

  module Manip : sig
  end

end
