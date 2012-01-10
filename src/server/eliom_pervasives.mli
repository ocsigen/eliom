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

(** Pervasives module for Eliom : it extends the OCaml stdlib and should always be opened. *)

(** {2 Pervasives} *)

exception Eliom_Internal_Error of string

type ('a, 'b) leftright = Left of 'a | Right of 'b

val map_option : ('a -> 'b) -> 'a option -> 'b option

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

type file_info = Ocsigen_extensions.file_info

(**/**)

val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
val (!!) : 'a Lazy.t -> 'a

external id : 'a -> 'a = "%identity"

val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a
val debug: ('a, unit, string, unit) format4 -> 'a

type poly (* Warning: do not use [poly array]... *)
val to_poly: 'a -> poly
type 'a client_expr = int64 * poly

(**/**)

(** {2 Standard libraries extensions } *)

(** Extension of the [List] module from the OCaml standard library. *)
module List : sig
  include module type of List
  val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
  val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_first_if_any : 'a -> 'a list -> 'a list
  val remove_first_if_any_q : 'a -> 'a list -> 'a list
  val map_filter : ('a -> 'b option) -> 'a list -> 'b list
end

(** Extension of the [String] module from the OCaml standard library. *)
module String : sig

  include module type of String

  val basic_sep : char -> string -> string * string
  val sep : char -> string -> string * string
  val split : ?multisep:bool -> char -> string -> string list

  val first_diff : string -> string -> int -> int -> int
  val may_append : string -> sep:string -> string -> string (* WAS add_to_string *)
  val may_concat : string -> sep:string -> string -> string (* WAS concat_strings *)

  val make_cryptographic_safe : unit -> string

  module Table : Map.S with type key = string
                        and type 'a t = 'a Ocsigen_pervasives.String.Table.t
  module Set : Set.S with type elt = string
		     and type t = Ocsigen_pervasives.String.Set.t
end

(** Standard operations on integer. *)
module Int : sig
  module Table : Map.S with type key = int
end

(** {2 TyXML}

    XML tree manipulation within Eliom is based on the TyXML library
    but use a custom representation for XML values (see
    {!XML}). Then, [Eliom_pervasives] redefines the three high level
    interfaces ({!SVG}, {!HTML5} and {!XHTML}) that are provided by
    TyXML for valid XML tree creation and printing. *)

(** Low-level XML manipulation. *)
module XML : sig

  (** {2 Base functions } *)

  (** See {% <<a_api project="tyxml" | module type XML_sigs.Iterable >> %}. *)
  include XML_sigs.Iterable

  (** {2 Unique nodes } *)

  (** Unique nodes are XML nodes that are manipulated 'by reference'
      when sent to the client part of an Eliom-application: the
      created element is allocated only one time in each instance of
      an application. See {% <<a_manual chapter="client"
      fragment="unique" |the eliom manual>>%} for more
      details. *)

  (** The function [make_unique elt] create a copy of the node [elt]
      that will be manipulated 'by reference' when sent to the
      client. When the optional argument [~copy:ref_elt] is provided,
      the returned node share the same "reference id" as [ref_elt].
      This allows the definition of multiple "initial contents" for a
      unique node, depending on which mode is first sent. *)
  val make_unique : ?copy:elt -> elt -> elt

  (** The predicate [is_unique elt] is valid when [elt] is node
      created with the function [make_unique]. *)
  val is_unique : elt -> bool

  (** Event handlers *)

  (** Values of type ['a caml_event_handler] represents event handler
      build with the [{{ ... }}] syntax (see the Eliom manual for more
      information on {% <<a_manual chapter="client"
      fragment="syntax"|syntax extension>>%}). Such values are expected
      by functions like {!Eliom_services.on_load}} or
      {!Eliom_pervasives.HTML5.M.a_onclick}. The type parameter is the
      type of the javascript event expected by the handler, for
      example {% <<a_api project="js_of_ocaml" | type
      Dom_html.mouseEvent>>%} or {% <<a_api project="js_of_ocaml" | type
      Dom_html.keyboardEvent >>%}. *)
  type -'a caml_event_handler constraint 'a = #Dom_html.event


  (**/**)

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type event_handler_table (* Concrete on client-side only. *)
  val get_unique_id : elt -> string option
  val make_event_handler_table : elt -> event_handler_table

  val event_handler_of_string : string -> event_handler
  val string_of_event_handler : event_handler -> string
  val event_handler_of_js : int64 -> poly -> #Dom_html.event caml_event_handler
  val event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option ) option Eliom_lazy.request -> event_handler

  (**/**)
  (* Deprecated alias. *)
  val event_of_string : string -> event_handler
  val string_of_handler : event_handler -> string
  val event_of_js : int64 -> poly -> #Dom_html.event caml_event_handler
  val event_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option ) option Eliom_lazy.request -> event_handler
  (**/**)

  type racontent =
    | RA of acontent
    | RACamlEventHandler of Dom_html.event caml_event_handler
    | RALazyStr of string Eliom_lazy.request
    | RALazyStrL of separator * string Eliom_lazy.request list
  val racontent : attrib -> racontent

  val lazy_node : ?a:(attrib list) -> ename -> elt list Eliom_lazy.request -> elt
  (**/**)

end

(** Building and pretty-printing valid SVG tree. *)
module SVG : sig

  (** Typed interface for building valid SVG tree. *)
  module M : sig

    (** See {% <<a_api project="tyxml" | module type SVG_sigs.T >> %}. *)
    include SVG_sigs.T with module XML := XML

    (** The function [unique elt] create a copy of the SVG node
	[elt] that will be manipulated 'by reference'. See {% <<a_api
	| val Eliom_pervasives.XML.make_unique >> %}. *)
    val unique: ?copy:'a elt -> 'a elt -> 'a elt

  end

  (** SVG printer.
      See {% <<a_api project="tyxml" | module type XML_sigs.TypedSimplePrinter >> %}. *)
  module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a M.elt
					  and type doc := M.doc

end

(** Building and printing valid (X)HTML5 tree. *)
module HTML5 : sig

  (** Typed interface for building valid (X)HTML5 tree. *)
  module M : sig

    (** See {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.M

    (** The function [unique elt] create a copy of the HTML5 node
	[elt] that will be manipulated 'by reference'. See {% <<a_api
	| val Eliom_pervasives.XML.make_unique >> %}. *)
    val unique: ?copy:'a elt -> 'a elt -> 'a elt

    val raw_a_onabort : XML.event_handler -> [> | `OnAbort] attrib
    val a_onabort : Dom_html.event XML.caml_event_handler -> [> | `OnAbort] attrib

    val raw_a_onafterprint : XML.event_handler -> [> | `OnAfterPrint] attrib
    val a_onafterprint : Dom_html.event XML.caml_event_handler -> [> | `OnAfterPrint] attrib

    val raw_a_onbeforeprint : XML.event_handler -> [> | `OnBeforePrint] attrib
    val a_onbeforeprint : Dom_html.event XML.caml_event_handler -> [> | `OnBeforePrint] attrib

    val raw_a_onbeforeunload : XML.event_handler -> [> | `OnBeforeUnload] attrib
    val a_onbeforeunload : Dom_html.event XML.caml_event_handler -> [> | `OnBeforeUnload] attrib

    val raw_a_onblur : XML.event_handler -> [> | `OnBlur] attrib
    val a_onblur : Dom_html.event XML.caml_event_handler -> [> | `OnBlur] attrib

    val raw_a_oncanplay : XML.event_handler -> [> | `OnCanPlay] attrib
    val a_oncanplay : Dom_html.event XML.caml_event_handler -> [> | `OnCanPlay] attrib

    val raw_a_oncanplaythrough : XML.event_handler -> [> | `OnCanPlayThrough] attrib
    val a_oncanplaythrough : Dom_html.event XML.caml_event_handler -> [> | `OnCanPlayThrough] attrib

    val raw_a_onchange : XML.event_handler -> [> | `OnChange] attrib
    val a_onchange : Dom_html.event XML.caml_event_handler -> [> | `OnChange] attrib

    val raw_a_onclick : XML.event_handler -> [> | `OnClick] attrib
    val a_onclick : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnClick] attrib

    val raw_a_oncontextmenu : XML.event_handler -> [> | `OnContextMenu] attrib
    val a_oncontextmenu : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnContextMenu] attrib

    val raw_a_ondblclick : XML.event_handler -> [> | `OnDblClick] attrib
    val a_ondblclick : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDblClick] attrib

    val raw_a_ondrag : XML.event_handler -> [> | `OnDrag] attrib
    val a_ondrag : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDrag] attrib

    val raw_a_ondragend : XML.event_handler -> [> | `OnDragEnd] attrib
    val a_ondragend : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDragEnd] attrib

    val raw_a_ondragenter : XML.event_handler -> [> | `OnDragEnter] attrib
    val a_ondragenter : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDragEnter] attrib

    val raw_a_ondragleave : XML.event_handler -> [> | `OnDragLeave] attrib
    val a_ondragleave : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDragLeave] attrib

    val raw_a_ondragover : XML.event_handler -> [> | `OnDragOver] attrib
    val a_ondragover : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDragOver] attrib

    val raw_a_ondragstart : XML.event_handler -> [> | `OnDragStart] attrib
    val a_ondragstart : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDragStart] attrib

    val raw_a_ondrop : XML.event_handler -> [> | `OnDrop] attrib
    val a_ondrop : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnDrop] attrib

    val raw_a_ondurationchange : XML.event_handler -> [> | `OnDurationChange] attrib
    val a_ondurationchange : Dom_html.event XML.caml_event_handler -> [> | `OnDurationChange] attrib

    val raw_a_onemptied : XML.event_handler -> [> | `OnEmptied] attrib
    val a_onemptied : Dom_html.event XML.caml_event_handler -> [> | `OnEmptied] attrib

    val raw_a_onended : XML.event_handler -> [> | `OnEnded] attrib
    val a_onended : Dom_html.event XML.caml_event_handler -> [> | `OnEnded] attrib

    val raw_a_onerror : XML.event_handler -> [> | `OnError] attrib
    val a_onerror : Dom_html.event XML.caml_event_handler -> [> | `OnError] attrib

    val raw_a_onfocus : XML.event_handler -> [> | `OnFocus] attrib
    val a_onfocus : Dom_html.event XML.caml_event_handler -> [> | `OnFocus] attrib

    val raw_a_onformchange : XML.event_handler -> [> | `OnFormChange] attrib
    val a_onformchange : Dom_html.event XML.caml_event_handler -> [> | `OnFormChange] attrib

    val raw_a_onforminput : XML.event_handler -> [> | `OnFormInput] attrib
    val a_onforminput : Dom_html.event XML.caml_event_handler -> [> | `OnFormInput] attrib

    val raw_a_onhashchange : XML.event_handler -> [> | `OnHashChange] attrib
    val a_onhashchange : Dom_html.event XML.caml_event_handler -> [> | `OnHashChange] attrib

    val raw_a_oninput : XML.event_handler -> [> | `OnInput] attrib
    val a_oninput : Dom_html.event XML.caml_event_handler -> [> | `OnInput] attrib

    val raw_a_oninvalid : XML.event_handler -> [> | `OnInvalid] attrib
    val a_oninvalid : Dom_html.event XML.caml_event_handler -> [> | `OnInvalid] attrib

    val raw_a_onmousedown : XML.event_handler -> [> | `OnMouseDown] attrib
    val a_onmousedown : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnMouseDown] attrib

    val raw_a_onmouseup : XML.event_handler -> [> | `OnMouseUp] attrib
    val a_onmouseup : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnMouseUp] attrib

    val raw_a_onmouseover : XML.event_handler -> [> | `OnMouseOver] attrib
    val a_onmouseover : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnMouseOver] attrib

    val raw_a_onmousemove : XML.event_handler -> [> | `OnMouseMove] attrib
    val a_onmousemove : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnMouseMove] attrib

    val raw_a_onmouseout : XML.event_handler -> [> | `OnMouseOut] attrib
    val a_onmouseout : Dom_html.mouseEvent XML.caml_event_handler -> [> | `OnMouseOut] attrib

    val raw_a_onmousewheel : XML.event_handler -> [> | `OnMouseWheel] attrib
    val a_onmousewheel : Dom_html.event XML.caml_event_handler -> [> | `OnMouseWheel] attrib

    val raw_a_onoffline : XML.event_handler -> [> | `OnOffLine] attrib
    val a_onoffline : Dom_html.event XML.caml_event_handler -> [> | `OnOffLine] attrib

    val raw_a_ononline : XML.event_handler -> [> | `OnOnLine] attrib
    val a_ononline : Dom_html.event XML.caml_event_handler -> [> | `OnOnLine] attrib

    val raw_a_onpause : XML.event_handler -> [> | `OnPause] attrib
    val a_onpause : Dom_html.event XML.caml_event_handler -> [> | `OnPause] attrib

    val raw_a_onplay : XML.event_handler -> [> | `OnPlay] attrib
    val a_onplay : Dom_html.event XML.caml_event_handler -> [> | `OnPlay] attrib

    val raw_a_onplaying : XML.event_handler -> [> | `OnPlaying] attrib
    val a_onplaying : Dom_html.event XML.caml_event_handler -> [> | `OnPlaying] attrib

    val raw_a_onpagehide : XML.event_handler -> [> | `OnPageHide] attrib
    val a_onpagehide : Dom_html.event XML.caml_event_handler -> [> | `OnPageHide] attrib

    val raw_a_onpageshow : XML.event_handler -> [> | `OnPageShow] attrib
    val a_onpageshow : Dom_html.event XML.caml_event_handler -> [> | `OnPageShow] attrib

    val raw_a_onpopstate : XML.event_handler -> [> | `OnPopState] attrib
    val a_onpopstate : Dom_html.event XML.caml_event_handler -> [> | `OnPopState] attrib

    val raw_a_onprogress : XML.event_handler -> [> | `OnProgress] attrib
    val a_onprogress : Dom_html.event XML.caml_event_handler -> [> | `OnProgress] attrib

    val raw_a_onratechange : XML.event_handler -> [> | `OnRateChange] attrib
    val a_onratechange : Dom_html.event XML.caml_event_handler -> [> | `OnRateChange] attrib

    val raw_a_onreadystatechange : XML.event_handler -> [> | `OnReadyStateChange] attrib
    val a_onreadystatechange : Dom_html.event XML.caml_event_handler -> [> | `OnReadyStateChange] attrib

    val raw_a_onredo : XML.event_handler -> [> | `OnRedo] attrib
    val a_onredo : Dom_html.event XML.caml_event_handler -> [> | `OnRedo] attrib

    val raw_a_onresize : XML.event_handler -> [> | `OnResize] attrib
    val a_onresize : Dom_html.event XML.caml_event_handler -> [> | `OnResize] attrib

    val raw_a_onscroll : XML.event_handler -> [> | `OnScroll] attrib
    val a_onscroll : Dom_html.event XML.caml_event_handler -> [> | `OnScroll] attrib

    val raw_a_onseeked : XML.event_handler -> [> | `OnSeeked] attrib
    val a_onseeked : Dom_html.event XML.caml_event_handler -> [> | `OnSeeked] attrib

    val raw_a_onseeking : XML.event_handler -> [> | `OnSeeking] attrib
    val a_onseeking : Dom_html.event XML.caml_event_handler -> [> | `OnSeeking] attrib

    val raw_a_onselect : XML.event_handler -> [> | `OnSelect] attrib
    val a_onselect : Dom_html.event XML.caml_event_handler -> [> | `OnSelect] attrib

    val raw_a_onshow : XML.event_handler -> [> | `OnShow] attrib
    val a_onshow : Dom_html.event XML.caml_event_handler -> [> | `OnShow] attrib

    val raw_a_onstalled : XML.event_handler -> [> | `OnStalled] attrib
    val a_onstalled : Dom_html.event XML.caml_event_handler -> [> | `OnStalled] attrib

    val raw_a_onstorage : XML.event_handler -> [> | `OnStorage] attrib
    val a_onstorage : Dom_html.event XML.caml_event_handler -> [> | `OnStorage] attrib

    val raw_a_onsubmit : XML.event_handler -> [> | `OnSubmit] attrib
    val a_onsubmit : Dom_html.event XML.caml_event_handler -> [> | `OnSubmit] attrib

    val raw_a_onsuspend : XML.event_handler -> [> | `OnSuspend] attrib
    val a_onsuspend : Dom_html.event XML.caml_event_handler -> [> | `OnSuspend] attrib

    val raw_a_ontimeupdate : XML.event_handler -> [> | `OnTimeUpdate] attrib
    val a_ontimeupdate : Dom_html.event XML.caml_event_handler -> [> | `OnTimeUpdate] attrib

    val raw_a_onundo : XML.event_handler -> [> | `OnUndo] attrib
    val a_onundo : Dom_html.event XML.caml_event_handler -> [> | `OnUndo] attrib

    val raw_a_onunload : XML.event_handler -> [> | `OnUnload] attrib
    val a_onunload : Dom_html.event XML.caml_event_handler -> [> | `OnUnload] attrib

    val raw_a_onvolumechange : XML.event_handler -> [> | `OnVolumeChange] attrib
    val a_onvolumechange : Dom_html.event XML.caml_event_handler -> [> | `OnVolumeChange] attrib

    val raw_a_onwaiting : XML.event_handler -> [> | `OnWaiting] attrib
    val a_onwaiting : Dom_html.event XML.caml_event_handler -> [> | `OnWaiting] attrib

    val raw_a_onkeypress : XML.event_handler -> [> | `OnKeyPress] attrib
    val a_onkeypress : Dom_html.keyboardEvent XML.caml_event_handler -> [> | `OnKeyPress] attrib

    val raw_a_onkeydown : XML.event_handler -> [> | `OnKeyDown] attrib
    val a_onkeydown : Dom_html.keyboardEvent XML.caml_event_handler -> [> | `OnKeyDown] attrib

    val raw_a_onkeyup : XML.event_handler -> [> | `OnKeyUp] attrib
    val a_onkeyup : Dom_html.keyboardEvent XML.caml_event_handler -> [> | `OnKeyUp] attrib

    val raw_a_onload : XML.event_handler -> [> | `OnLoad] attrib
    val a_onload : Dom_html.event XML.caml_event_handler -> [> | `OnLoad] attrib

    val raw_a_onloadeddata : XML.event_handler -> [> | `OnLoadedData] attrib
    val a_onloadeddata : Dom_html.event XML.caml_event_handler -> [> | `OnLoadedData] attrib

    val raw_a_onloadedmetadata : XML.event_handler -> [> | `OnLoadedMetaData] attrib
    val a_onloadedmetadata : Dom_html.event XML.caml_event_handler -> [> | `OnLoadedMetaData] attrib

    val raw_a_onloadstart : XML.event_handler -> [> | `OnLoadStart] attrib
    val a_onloadstart : Dom_html.event XML.caml_event_handler -> [> | `OnLoadStart] attrib

    val raw_a_onmessage : XML.event_handler -> [> | `OnMessage] attrib
    val a_onmessage : Dom_html.event XML.caml_event_handler -> [> | `OnMessage] attrib

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus

    (* Aliases used in Eliom_output_base (redefined on client side) *)
    val raw_a_onclick: XML.event_handler -> [> `OnClick ] attrib
    val raw_a_onsubmit: XML.event_handler -> [> `OnSubmit ] attrib
    (**/**)

  end

  (** {{:http://dev.w3.org/html5/html-xhtml-author-guide/}"Polyglot"} HTML5 printer.
     See {% <<a_api project="tyxml" | module type XML_sigs.TypedSimplePrinter >> %}. *)
  module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a M.elt
					  and type doc := M.doc

end

(** Building and printing valid XHTML tree. *)
module XHTML : sig

  (** Typed interface for building valid XHTML (Strict) tree. *)
  module M : sig

    (** See {% <<a_api project="tyxml" | module type XHTML_sigs.T >> %}. *)
    include XHTML_sigs.T with module XML := XML

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end

  (** XHTML (latest Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module P : XML_sigs.TypedSimplePrinter
    with type 'a elt := 'a M.elt
    and type doc := M.doc


  (** Typed interface for building valid XHTML (1.0 Strict) tree. *)
  module M_01_00 : sig

    (** See {% <<a_api project="tyxml" | module type XHTML_sigs.T >> %}. *)
    include XHTML_sigs.T with module XML := XML

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end

  (** XHTML (1.0 Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module P_01_00 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_00.elt
		    and type doc := M_01_00.doc

  (** Typed interface for building valid XHTML (1.1 Strict) tree. *)
  module M_01_01 : sig

    (** See {% <<a_api project="tyxml" | module type XHTML_sigs.T >> %}. *)
    include XHTML_sigs.T with module XML := XML

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      action:XML.uri ->
      ([< XHTML_types.form_attrib ], [< XHTML_types.form_content ], [> XHTML_types.form ]) lazy_plus
    (**/**)

  end

  (** XHTML (1.1 Strict) printer. See {% <<a_api project="tyxml" |
      module type XML_sigs.TypedSimplePrinter >> %}. This printer try
      to follow the {{:http://www.w3.org/TR/xhtml1/#guidelines}W3C
      guidelines} for HTML compatibility. Hence the resulting string
      could be serve as well as [application/xhtml+xml] or
      [text/html]. This however has some
      {{:http://hixie.ch/advocacy/xhtml}limitations}. *)
  module P_01_01 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_01.elt
		    and type doc := M_01_01.doc

end


(** {2 Other libraries} *)

(** Helpers for Url manipulations *)
module Url : sig

  type t = Ocsigen_pervasives.Url.t
  type uri = Ocsigen_pervasives.Url.uri
  type path = Ocsigen_pervasives.Url.path

  val make_absolute_url :
      https:bool -> host:string -> port:int -> uri -> t

  val remove_slash_at_beginning : path -> path
  val remove_internal_slash : path -> path
  val is_prefix_skip_end_slash : string list -> string list -> bool
  val change_empty_list : path -> path

  val string_of_url_path : encode:bool -> path -> uri

  val make_encoded_parameters : (string * string) list -> uri

  val encode : ?plus:bool -> string -> string
  val decode : ?plus:bool -> string -> string

end

(** Helpers for IP addresses manipulations *)
module Ip_address : sig

  type t = Ocsigen_pervasives.Ip_address.t =
    | IPv4 of int32
    | IPv6 of int64 * int64

  val parse : string -> t * (t option)

  val network_of_ip : t -> int32 -> int64 * int64 -> t
  val inet6_addr_loopback : t

end
