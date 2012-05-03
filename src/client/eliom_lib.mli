
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

val to_json : ?typ:'a -> 'b -> string
val of_json : ?typ:'a -> string -> 'b

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

  (** {2 Dom semantics} *)

  include SVG_sigs.T with module XML := XML

  (** Typed interface for building valid SVG tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type SVG_sigs.T >> %}. *)
  module DOM: SVG_sigs.T with module XML := XML
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

  (** {2 Functional semantics} *)

  (** Typed interface for building valid SVG tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      SVG_sigs.T >> %}. *)
  module M : SVG_sigs.T with module XML := XML
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

  (** {2 Dom semantics} *)

  (** Cf. <<a_api project="tyxml"|module HTML5_sigs.T>>. *)

  include HTML5_sigs.T with module XML := XML
		       and module SVG := SVG

  (** Typed interface for building valid HTML5 tree (DOM semantics). See
      {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
  module DOM: sig

    include HTML5_sigs.T with module XML := XML
		         and module SVG := SVG.DOM
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus

  end

  (** {2 Functional semantics} *)

  (** Typed interface for building valid HTML5 tree (functional
      semantics). See {% <<a_api project="tyxml" | module type
      HTML5_sigs.T >> %}. *)
  module M : sig

    (** See {% <<a_api project="tyxml" | module type HTML5_sigs.T >> %}. *)
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.M
		         and type 'a elt = 'a elt
		         and type 'a attrib = 'a attrib
		         and type uri = uri

    (** {2 Event handlers} *)

    (** Redefine event handler attributes to simplify their usage. *)
    include "sigs/eliom_html5_event_handler.mli"

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus

  end

  (** {2 Event handlers} *)

  (** Redefine event handler attributes to simplify their usage. *)
  include "sigs/eliom_html5_event_handler.mli"

  (**/**)
  val raw_a_onabort : XML.event_handler -> [> | `OnAbort] attrib
  val raw_a_onafterprint : XML.event_handler -> [> | `OnAfterPrint] attrib
  val raw_a_onbeforeprint : XML.event_handler -> [> | `OnBeforePrint] attrib
  val raw_a_onbeforeunload : XML.event_handler -> [> | `OnBeforeUnload] attrib
  val raw_a_onblur : XML.event_handler -> [> | `OnBlur] attrib
  val raw_a_oncanplay : XML.event_handler -> [> | `OnCanPlay] attrib
  val raw_a_oncanplaythrough : XML.event_handler -> [> | `OnCanPlayThrough] attrib
  val raw_a_onchange : XML.event_handler -> [> | `OnChange] attrib
  val raw_a_onclick : XML.event_handler -> [> | `OnClick] attrib
  val raw_a_oncontextmenu : XML.event_handler -> [> | `OnContextMenu] attrib
  val raw_a_ondblclick : XML.event_handler -> [> | `OnDblClick] attrib
  val raw_a_ondrag : XML.event_handler -> [> | `OnDrag] attrib
  val raw_a_ondragend : XML.event_handler -> [> | `OnDragEnd] attrib
  val raw_a_ondragenter : XML.event_handler -> [> | `OnDragEnter] attrib
  val raw_a_ondragleave : XML.event_handler -> [> | `OnDragLeave] attrib
  val raw_a_ondragover : XML.event_handler -> [> | `OnDragOver] attrib
  val raw_a_ondragstart : XML.event_handler -> [> | `OnDragStart] attrib
  val raw_a_ondrop : XML.event_handler -> [> | `OnDrop] attrib
  val raw_a_ondurationchange : XML.event_handler -> [> | `OnDurationChange] attrib
  val raw_a_onemptied : XML.event_handler -> [> | `OnEmptied] attrib
  val raw_a_onended : XML.event_handler -> [> | `OnEnded] attrib
  val raw_a_onerror : XML.event_handler -> [> | `OnError] attrib
  val raw_a_onfocus : XML.event_handler -> [> | `OnFocus] attrib
  val raw_a_onformchange : XML.event_handler -> [> | `OnFormChange] attrib
  val raw_a_onforminput : XML.event_handler -> [> | `OnFormInput] attrib
  val raw_a_onhashchange : XML.event_handler -> [> | `OnHashChange] attrib
  val raw_a_oninput : XML.event_handler -> [> | `OnInput] attrib
  val raw_a_oninvalid : XML.event_handler -> [> | `OnInvalid] attrib
  val raw_a_onmousedown : XML.event_handler -> [> | `OnMouseDown] attrib
  val raw_a_onmouseup : XML.event_handler -> [> | `OnMouseUp] attrib
  val raw_a_onmouseover : XML.event_handler -> [> | `OnMouseOver] attrib
  val raw_a_onmousemove : XML.event_handler -> [> | `OnMouseMove] attrib
  val raw_a_onmouseout : XML.event_handler -> [> | `OnMouseOut] attrib
  val raw_a_onmousewheel : XML.event_handler -> [> | `OnMouseWheel] attrib
  val raw_a_onoffline : XML.event_handler -> [> | `OnOffLine] attrib
  val raw_a_ononline : XML.event_handler -> [> | `OnOnLine] attrib
  val raw_a_onpause : XML.event_handler -> [> | `OnPause] attrib
  val raw_a_onplay : XML.event_handler -> [> | `OnPlay] attrib
  val raw_a_onplaying : XML.event_handler -> [> | `OnPlaying] attrib
  val raw_a_onpagehide : XML.event_handler -> [> | `OnPageHide] attrib
  val raw_a_onpageshow : XML.event_handler -> [> | `OnPageShow] attrib
  val raw_a_onpopstate : XML.event_handler -> [> | `OnPopState] attrib
  val raw_a_onprogress : XML.event_handler -> [> | `OnProgress] attrib
  val raw_a_onratechange : XML.event_handler -> [> | `OnRateChange] attrib
  val raw_a_onreadystatechange : XML.event_handler -> [> | `OnReadyStateChange] attrib
  val raw_a_onredo : XML.event_handler -> [> | `OnRedo] attrib
  val raw_a_onresize : XML.event_handler -> [> | `OnResize] attrib
  val raw_a_onscroll : XML.event_handler -> [> | `OnScroll] attrib
  val raw_a_onseeked : XML.event_handler -> [> | `OnSeeked] attrib
  val raw_a_onseeking : XML.event_handler -> [> | `OnSeeking] attrib
  val raw_a_onselect : XML.event_handler -> [> | `OnSelect] attrib
  val raw_a_onshow : XML.event_handler -> [> | `OnShow] attrib
  val raw_a_onstalled : XML.event_handler -> [> | `OnStalled] attrib
  val raw_a_onstorage : XML.event_handler -> [> | `OnStorage] attrib
  val raw_a_onsubmit : XML.event_handler -> [> | `OnSubmit] attrib
  val raw_a_onsuspend : XML.event_handler -> [> | `OnSuspend] attrib
  val raw_a_ontimeupdate : XML.event_handler -> [> | `OnTimeUpdate] attrib
  val raw_a_onundo : XML.event_handler -> [> | `OnUndo] attrib
  val raw_a_onunload : XML.event_handler -> [> | `OnUnload] attrib
  val raw_a_onvolumechange : XML.event_handler -> [> | `OnVolumeChange] attrib
  val raw_a_onwaiting : XML.event_handler -> [> | `OnWaiting] attrib
  val raw_a_onkeypress : XML.event_handler -> [> | `OnKeyPress] attrib
  val raw_a_onkeydown : XML.event_handler -> [> | `OnKeyDown] attrib
  val raw_a_onkeyup : XML.event_handler -> [> | `OnKeyUp] attrib
  val raw_a_onload : XML.event_handler -> [> | `OnLoad] attrib
  val raw_a_onloadeddata : XML.event_handler -> [> | `OnLoadedData] attrib
  val raw_a_onloadedmetadata : XML.event_handler -> [> | `OnLoadedMetaData] attrib
  val raw_a_onloadstart : XML.event_handler -> [> | `OnLoadStart] attrib
  val raw_a_onmessage : XML.event_handler -> [> | `OnMessage] attrib
  (**/**)

  (** {2 Wrap [Dom_node] into [HTML5.elt]} *)

  val of_element : Dom_html.element Js.t -> 'a elt
  val of_html : Dom_html.htmlElement Js.t -> HTML5_types.html elt
  val of_head : Dom_html.headElement Js.t -> HTML5_types.head elt
  val of_link : Dom_html.linkElement Js.t -> HTML5_types.link elt
  val of_title : Dom_html.titleElement Js.t -> HTML5_types.title elt
  val of_meta : Dom_html.metaElement Js.t -> HTML5_types.meta elt
  val of_base : Dom_html.baseElement Js.t -> HTML5_types.base elt
  val of_style : Dom_html.styleElement Js.t -> HTML5_types.style elt
  val of_body : Dom_html.bodyElement Js.t -> HTML5_types.body elt
  val of_form : Dom_html.formElement Js.t -> HTML5_types.form elt
  val of_optGroup : Dom_html.optGroupElement Js.t -> HTML5_types.optgroup elt
  val of_option : Dom_html.optionElement Js.t -> HTML5_types.selectoption elt
  val of_select : Dom_html.selectElement Js.t -> HTML5_types.select elt
  val of_input : Dom_html.inputElement Js.t -> HTML5_types.input elt
  val of_textArea : Dom_html.textAreaElement Js.t -> HTML5_types.textarea elt
  val of_button : Dom_html.buttonElement Js.t -> HTML5_types.button elt
  val of_label : Dom_html.labelElement Js.t -> HTML5_types.label elt
  val of_fieldSet : Dom_html.fieldSetElement Js.t -> HTML5_types.fieldset elt
  val of_legend : Dom_html.legendElement Js.t -> HTML5_types.legend elt
  val of_uList : Dom_html.uListElement Js.t -> HTML5_types.ul elt
  val of_oList : Dom_html.oListElement Js.t -> HTML5_types.ol elt
  val of_dList : Dom_html.dListElement Js.t -> [`Dl] elt
  val of_li : Dom_html.liElement Js.t -> HTML5_types.li elt
  val of_div : Dom_html.divElement Js.t -> HTML5_types.div elt
  val of_paragraph : Dom_html.paragraphElement Js.t -> HTML5_types.p elt
  val of_heading : Dom_html.headingElement Js.t -> HTML5_types.heading elt
  val of_quote : Dom_html.quoteElement Js.t -> HTML5_types.blockquote elt
  val of_pre : Dom_html.preElement Js.t -> HTML5_types.pre elt
  val of_br : Dom_html.brElement Js.t -> HTML5_types.br elt
  val of_hr : Dom_html.hrElement Js.t -> HTML5_types.hr elt
  val of_anchor : Dom_html.anchorElement Js.t -> 'a HTML5_types.a elt
  val of_image : Dom_html.imageElement Js.t -> [`Img] elt
  val of_object : Dom_html.objectElement Js.t -> 'a HTML5_types.object_ elt
  val of_param : Dom_html.paramElement Js.t -> HTML5_types.param elt
  val of_area : Dom_html.areaElement Js.t -> HTML5_types.area elt
  val of_map : Dom_html.mapElement Js.t -> 'a HTML5_types.map elt
  val of_script : Dom_html.scriptElement Js.t -> HTML5_types.script elt
  val of_tableCell : Dom_html.tableCellElement Js.t -> [ HTML5_types.td | HTML5_types.td ] elt
  val of_tableRow : Dom_html.tableRowElement Js.t -> HTML5_types.tr elt
  val of_tableCol : Dom_html.tableColElement Js.t -> HTML5_types.col elt
  val of_tableSection : Dom_html.tableSectionElement Js.t -> [ HTML5_types.tfoot | HTML5_types.thead | HTML5_types.tbody ] elt
  val of_tableCaption : Dom_html.tableCaptionElement Js.t -> HTML5_types.caption elt
  val of_table : Dom_html.tableElement Js.t -> HTML5_types.table elt
  val of_canvas : Dom_html.canvasElement Js.t -> 'a HTML5_types.canvas elt
  val of_iFrame : Dom_html.iFrameElement Js.t -> HTML5_types.iframe elt

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

end

type file_info
