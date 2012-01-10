
(** Pervasives module for Eliom extending stdlib, should always be opened. *)

exception Eliom_Internal_Error of string

external id : 'a -> 'a = "%identity"

val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
val ( !! ) : 'a Lazy.t -> 'a

(* val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
(* val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)
val map_option : ('a -> 'b) -> 'a option -> 'b option
val iter_option : ('a -> unit) -> 'a option -> unit

(* val fst3 : 'a * 'b * 'c -> 'a *)
(* val snd3 : 'a * 'b * 'c -> 'b *)
(* val thd3 : 'a * 'b * 'c -> 'c *)

(* type yesnomaybe = Yes | No | Maybe *)
type ('a, 'b) leftright = Left of 'a | Right of 'b

type poly
val to_poly: 'a -> poly
val from_poly: poly -> 'a
type 'a client_expr = int64 * poly

exception False

module List : sig
  include module type of List
  (* val remove_first_if_any : 'a -> 'a list -> 'a list *)
  (* val remove_first_if_any_q : 'a -> 'a list -> 'a list *)
  (* val remove_first : 'a -> 'a list -> 'a list *)
  (* val remove_first_q : 'a -> 'a list -> 'a list *)
  (* val remove_all : 'a -> 'a list -> 'a list *)
  (* val remove_all_q : 'a -> 'a list -> 'a list *)
  (* val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list *)
  (* val remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list *)
  (* val last : 'a list -> 'a *)
  (* val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list *)
  (* val is_prefix : 'a list -> 'a list -> bool *)
  val is_prefix_skip_end_slash : string list -> string list -> bool
  val chop : int -> 'a list -> 'a list
  val map_filter : ('a -> 'b option) -> 'a list -> 'b list
end

module String : sig
  include module type of String
  (* val remove_spaces : string -> int -> int -> string *)
  (* val basic_sep : char -> string -> string * string *)
  (* val sep : char -> string -> string * string *)
  val split : ?multisep:bool -> char -> string -> string list
  (* val may_append : string -> sep:string -> string -> string *)
  val may_concat : string -> sep:string -> string -> string
  (* val first_diff : string -> string -> int -> int -> int *)
  module Table : Map.S with type key = string
  module Set : Set.S with type elt = string
end

module Url : sig
  include module type of Url
  type t = string
  (* type uri = string *)
  type path = string list
  val make_absolute_url :
      https:bool -> host:string -> port:int -> string -> t
  (* val remove_dotdot : string list -> string list *)
  (* val remove_end_slash : string -> string *)
  val remove_internal_slash : string list -> string list
  (* val change_empty_list : string list -> string list *)
  (* val add_end_slash_if_missing : string list -> string list *)
  (* val remove_slash_at_end : string list -> string list *)
  val remove_slash_at_beginning : string list -> string list
  (* val recursively_remove_slash_at_beginning : string list -> string list *)
  val decode : string -> string
  val encode : ?plus:bool -> string -> string
  val make_encoded_parameters : (string * string) list -> string
  val split_path : string -> string list
  val split_fragment : string -> string * string option
  val get_ssl : string -> bool option
end

(*
module Ip_address : sig
  (* type t = IPv4 of int32 | IPv6 of int64 * int64 *)
  (* exception Invalid_ip_address of string *)
  (* val parse : string -> t * t option *)
  (* val match_ip : t * t option -> t -> bool *)
  (* val network_of_ip : ip:t -> mask:t -> t *)
end
*)

module Printexc : sig
  include module type of Printexc
  val register_exn_printer : ((exn -> string) -> exn -> string) -> unit
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
    | CE_registered_closure of int * ((#Dom_html.event as 'a) Js.t -> unit) client_expr
    | CE_client_closure of ('a Js.t -> unit)
    | CE_call_service of
	([ `A | `Form_get | `Form_post] * (bool * string list) option) option Eliom_lazy.request

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
      * (bool * string list) option ) option Eliom_lazy.request -> event_handler
  val event_handler_of_function : (#Dom_html.event Js.t -> unit) -> event_handler

  (* Deprecated alias. *)
  val event_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option ) option Eliom_lazy.request -> event_handler
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

  val make_unique : ?copy:elt -> elt -> elt
  val is_unique : elt -> bool
  val get_unique_id : elt -> string option

  type node_id = string

  module ClosureMap : Map.S with type key = int
  type event_handler_table = ((poly -> unit) client_expr) ClosureMap.t

end

module SVG : sig

  (** type safe SVG creation. *)
  module M : sig
    include SVG_sigs.T with module XML := XML
    val unique: ?copy:'a elt -> 'a elt -> 'a elt
  end

end

module HTML5 : sig

  (** type safe HTML5 creation. *)
  module M : sig

    include HTML5_sigs.T with module XML := XML and module SVG := SVG.M

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

    val unique: ?copy:'a elt -> 'a elt -> 'a elt

    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus

    val a_onabort : (#Dom_html.event Js.t -> unit) -> [> | `OnAbort] attrib
    val a_onafterprint : (#Dom_html.event Js.t -> unit) -> [> | `OnAfterPrint] attrib
    val a_onbeforeprint : (#Dom_html.event Js.t -> unit) -> [> | `OnBeforePrint] attrib
    val a_onbeforeunload : (#Dom_html.event Js.t -> unit) -> [> | `OnBeforeUnload] attrib
    val a_onblur : (#Dom_html.event Js.t -> unit) -> [> | `OnBlur] attrib
    val a_oncanplay : (#Dom_html.event Js.t -> unit) -> [> | `OnCanPlay] attrib
    val a_oncanplaythrough : (#Dom_html.event Js.t -> unit) -> [> | `OnCanPlayThrough] attrib
    val a_onchange : (#Dom_html.event Js.t -> unit) -> [> | `OnChange] attrib
    val a_onclick : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnClick] attrib
    val a_oncontextmenu : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnContextMenu] attrib
    val a_ondblclick : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDblClick] attrib
    val a_ondrag : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDrag] attrib
    val a_ondragend : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDragEnd] attrib
    val a_ondragenter : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDragEnter] attrib
    val a_ondragleave : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDragLeave] attrib
    val a_ondragover : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDragOver] attrib
    val a_ondragstart : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDragStart] attrib
    val a_ondrop : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnDrop] attrib
    val a_ondurationchange : (#Dom_html.event Js.t -> unit) -> [> | `OnDurationChange] attrib
    val a_onemptied : (#Dom_html.event Js.t -> unit) -> [> | `OnEmptied] attrib
    val a_onended : (#Dom_html.event Js.t -> unit) -> [> | `OnEnded] attrib
    val a_onerror : (#Dom_html.event Js.t -> unit) -> [> | `OnError] attrib
    val a_onfocus : (#Dom_html.event Js.t -> unit) -> [> | `OnFocus] attrib
    val a_onformchange : (#Dom_html.event Js.t -> unit) -> [> | `OnFormChange] attrib
    val a_onforminput : (#Dom_html.event Js.t -> unit) -> [> | `OnFormInput] attrib
    val a_onhashchange : (#Dom_html.event Js.t -> unit) -> [> | `OnHashChange] attrib
    val a_oninput : (#Dom_html.event Js.t -> unit) -> [> | `OnInput] attrib
    val a_oninvalid : (#Dom_html.event Js.t -> unit) -> [> | `OnInvalid] attrib
    val a_onmousedown : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnMouseDown] attrib
    val a_onmouseup : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnMouseUp] attrib
    val a_onmouseover : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnMouseOver] attrib
    val a_onmousemove : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnMouseMove] attrib
    val a_onmouseout : (#Dom_html.mouseEvent Js.t -> unit) -> [> | `OnMouseOut] attrib
    val a_onmousewheel : (#Dom_html.event Js.t -> unit) -> [> | `OnMouseWheel] attrib
    val a_onoffline : (#Dom_html.event Js.t -> unit) -> [> | `OnOffLine] attrib
    val a_ononline : (#Dom_html.event Js.t -> unit) -> [> | `OnOnLine] attrib
    val a_onpause : (#Dom_html.event Js.t -> unit) -> [> | `OnPause] attrib
    val a_onplay : (#Dom_html.event Js.t -> unit) -> [> | `OnPlay] attrib
    val a_onplaying : (#Dom_html.event Js.t -> unit) -> [> | `OnPlaying] attrib
    val a_onpagehide : (#Dom_html.event Js.t -> unit) -> [> | `OnPageHide] attrib
    val a_onpageshow : (#Dom_html.event Js.t -> unit) -> [> | `OnPageShow] attrib
    val a_onpopstate : (#Dom_html.event Js.t -> unit) -> [> | `OnPopState] attrib
    val a_onprogress : (#Dom_html.event Js.t -> unit) -> [> | `OnProgress] attrib
    val a_onratechange : (#Dom_html.event Js.t -> unit) -> [> | `OnRateChange] attrib
    val a_onreadystatechange : (#Dom_html.event Js.t -> unit) -> [> | `OnReadyStateChange] attrib
    val a_onredo : (#Dom_html.event Js.t -> unit) -> [> | `OnRedo] attrib
    val a_onresize : (#Dom_html.event Js.t -> unit) -> [> | `OnResize] attrib
    val a_onscroll : (#Dom_html.event Js.t -> unit) -> [> | `OnScroll] attrib
    val a_onseeked : (#Dom_html.event Js.t -> unit) -> [> | `OnSeeked] attrib
    val a_onseeking : (#Dom_html.event Js.t -> unit) -> [> | `OnSeeking] attrib
    val a_onselect : (#Dom_html.event Js.t -> unit) -> [> | `OnSelect] attrib
    val a_onshow : (#Dom_html.event Js.t -> unit) -> [> | `OnShow] attrib
    val a_onstalled : (#Dom_html.event Js.t -> unit) -> [> | `OnStalled] attrib
    val a_onstorage : (#Dom_html.event Js.t -> unit) -> [> | `OnStorage] attrib
    val a_onsubmit : (#Dom_html.event Js.t -> unit) -> [> | `OnSubmit] attrib
    val a_onsuspend : (#Dom_html.event Js.t -> unit) -> [> | `OnSuspend] attrib
    val a_ontimeupdate : (#Dom_html.event Js.t -> unit) -> [> | `OnTimeUpdate] attrib
    val a_onundo : (#Dom_html.event Js.t -> unit) -> [> | `OnUndo] attrib
    val a_onunload : (#Dom_html.event Js.t -> unit) -> [> | `OnUnload] attrib
    val a_onvolumechange : (#Dom_html.event Js.t -> unit) -> [> | `OnVolumeChange] attrib
    val a_onwaiting : (#Dom_html.event Js.t -> unit) -> [> | `OnWaiting] attrib
    val a_onkeypress : (#Dom_html.keyboardEvent Js.t -> unit) -> [> | `OnKeyPress] attrib
    val a_onkeydown : (#Dom_html.keyboardEvent Js.t -> unit) -> [> | `OnKeyDown] attrib
    val a_onkeyup : (#Dom_html.keyboardEvent Js.t -> unit) -> [> | `OnKeyUp] attrib
    val a_onload : (#Dom_html.event Js.t -> unit) -> [> | `OnLoad] attrib
    val a_onloadeddata : (#Dom_html.event Js.t -> unit) -> [> | `OnLoadedData] attrib
    val a_onloadedmetadata : (#Dom_html.event Js.t -> unit) -> [> | `OnLoadedMetaData] attrib
    val a_onloadstart : (#Dom_html.event Js.t -> unit) -> [> | `OnLoadStart] attrib
    val a_onmessage : (#Dom_html.event Js.t -> unit) -> [> | `OnMessage] attrib

    (**/**)
    val raw_a_onclick: XML.event_handler -> [> `OnClick ] attrib
    val raw_a_onsubmit: XML.event_handler -> [> `OnSubmit ] attrib
    (**/**)

  end

end

type file_info
