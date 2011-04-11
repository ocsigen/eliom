
exception Eliom_Internal_Error of string

external id : 'a -> 'a = "%identity"

val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val ( >|= ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
val ( !! ) : 'a Lazy.t -> 'a

(* val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
(* val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c *)
val map_option : ('a -> 'b) -> 'a option -> 'b option

(* val fst3 : 'a * 'b * 'c -> 'a *)
(* val snd3 : 'a * 'b * 'c -> 'b *)
(* val thd3 : 'a * 'b * 'c -> 'c *)

(* type yesnomaybe = Yes | No | Maybe *)
type ('a, 'b) leftright = Left of 'a | Right of 'b

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
  (* module Set : Set.S with type elt = string *)
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
  (* val split_path : string -> string list *)
end

module Ip_address : sig
  (* type t = IPv4 of int32 | IPv6 of int64 * int64 *)
  (* exception Invalid_ip_address of string *)
  (* val parse : string -> t * t option *)
  (* val match_ip : t * t option -> t -> bool *)
  (* val network_of_ip : ip:t -> mask:t -> t *)
end

module Printexc : sig
  include module type of Printexc
  val register_exn_printer : ((exn -> string) -> exn -> string) -> unit
end

val debug : string -> unit
val jsdebug : 'a -> unit
val alert : string -> unit
val jsalert : Js.js_string Js.t -> unit

val to_json : ?typ:'a -> 'b -> string
val of_json : ?typ:'a -> string -> 'b

val encode_form_value : 'a -> string
val unmarshal_js_var : string -> 'a

val encode_header_value : 'a -> string

module Html : module type of Dom_html

module XML : sig

  type aname = string
  type attrib = string * Js.Unsafe.any
  type attribs = attrib list
  type event = unit -> unit

  val attrib_name : attrib -> aname

  val float_attrib : aname -> float -> attrib
  val int_attrib : aname -> int -> attrib
  val string_attrib : aname -> string -> attrib
  val space_sep_attrib : aname -> string list -> attrib
  val comma_sep_attrib : aname -> string list -> attrib
  val event_attrib : aname -> event -> attrib

  type ename = string
  type elt = Dom.node Js.t

  val pcdata : string -> elt
  val encodedpcdata : string -> elt
  val entity : string -> elt

  val empty : unit -> elt
  val comment : string -> elt

  val leaf : ?a:(attrib list) -> ename -> elt
  val node : ?a:(attrib list) -> ename -> elt list -> elt

  val cdata : string -> elt
  val cdata_script : string -> elt
  val cdata_style : string -> elt

  (* those functions are unsafe: for internal use only *)
  val lwt_register_event : ?keep_default:bool -> elt -> ename -> ('a -> 'b Lwt.t) -> 'a -> unit
  val register_event : ?keep_default:bool -> elt -> ename -> ('a -> 'b) -> 'a -> unit

  val ref_node : elt -> int

  type ref_tree = Ref_tree of int option * (int * ref_tree) list

end

module SVG : sig

  module M : SVG_sigs.SVG(XML).T

end

module HTML5 : sig
  module M :
  sig
    include HTML5_sigs.HTML5(XML)(SVG.M).T
    val to_html : HTML5_types.html elt -> Dom_html.htmlElement Js.t
    val to_head : HTML5_types.head elt -> Dom_html.headElement Js.t
    val to_link : HTML5_types.link elt -> Dom_html.linkElement Js.t
    val to_title : HTML5_types.title elt -> Dom_html.titleElement Js.t
    val to_meta : HTML5_types.meta elt -> Dom_html.metaElement Js.t
    val to_base : HTML5_types.base elt -> Dom_html.baseElement Js.t
    val to_style : HTML5_types.style elt -> Dom_html.styleElement Js.t
    val to_body : HTML5_types.body elt -> Dom_html.bodyElement Js.t
    val to_form : HTML5_types.form elt -> Dom_html.formElement Js.t
    val to_optGroup : HTML5_types.optgroup elt -> Dom_html.optGroupElement Js.t
    val to_option : HTML5_types.selectoption elt -> Dom_html.optionElement Js.t
    val to_select : HTML5_types.select elt -> Dom_html.selectElement Js.t
    val to_input : HTML5_types.input elt -> Dom_html.inputElement Js.t
    val to_textArea : HTML5_types.textarea elt -> Dom_html.textAreaElement Js.t
    val to_button : HTML5_types.button elt -> Dom_html.buttonElement Js.t
    val to_label : HTML5_types.label elt -> Dom_html.labelElement Js.t
    val to_fieldSet : HTML5_types.fieldset elt -> Dom_html.fieldSetElement Js.t
    val to_legend : HTML5_types.legend elt -> Dom_html.legendElement Js.t
    val to_uList : HTML5_types.ul elt -> Dom_html.uListElement Js.t
    val to_oList : HTML5_types.ol elt -> Dom_html.oListElement Js.t
    val to_dList : [`Dl] elt -> Dom_html.dListElement Js.t
    val to_li : HTML5_types.li elt -> Dom_html.liElement Js.t
    val to_div : HTML5_types.div elt -> Dom_html.divElement Js.t
    val to_paragraph : HTML5_types.p elt -> Dom_html.paragraphElement Js.t
    val to_heading : HTML5_types.heading elt -> Dom_html.headingElement Js.t
    val to_quote : HTML5_types.blockquote elt -> Dom_html.quoteElement Js.t
    val to_pre : HTML5_types.pre elt -> Dom_html.preElement Js.t
    val to_br : HTML5_types.br elt -> Dom_html.brElement Js.t
    val to_hr : HTML5_types.hr elt -> Dom_html.hrElement Js.t
    val to_anchor : 'a HTML5_types.a elt -> Dom_html.anchorElement Js.t
    val to_image : [`Img] elt -> Dom_html.imageElement Js.t
    val to_object : 'a HTML5_types.object_ elt -> Dom_html.objectElement Js.t
    val to_param : HTML5_types.param elt -> Dom_html.paramElement Js.t
    val to_area : HTML5_types.area elt -> Dom_html.areaElement Js.t
    val to_map : 'a HTML5_types.map elt -> Dom_html.mapElement Js.t
    val to_script : HTML5_types.script elt -> Dom_html.scriptElement Js.t
    val to_tableCell : [ HTML5_types.td | HTML5_types.td ] elt -> Dom_html.tableCellElement Js.t
    val to_tableRow : HTML5_types.tr elt -> Dom_html.tableRowElement Js.t
    val to_tableCol : HTML5_types.col elt -> Dom_html.tableColElement Js.t
    val to_tableSection : [ HTML5_types.tfoot | HTML5_types.thead | HTML5_types.tbody ] elt ->
      Dom_html.tableSectionElement Js.t
    val to_tableCaption : HTML5_types.caption elt -> Dom_html.tableCaptionElement Js.t
    val to_table : HTML5_types.table elt -> Dom_html.tableElement Js.t
    val to_canvas : 'a HTML5_types.canvas elt -> Dom_html.canvasElement Js.t
    val to_iFrame : HTML5_types.iframe elt -> Dom_html.iFrameElement Js.t
  end
end

module XHTML : sig

  module M : XHTML_sigs.XHTML(XML).T
  module M_01_00 : XHTML_sigs.XHTML(XML).T
  module M_01_01 : XHTML_sigs.XHTML(XML).T
  module M_01_00_compat : XHTML_sigs.XHTML(XML).T
  module M_01_01_compat : XHTML_sigs.XHTML(XML).T

end

module Reactive_dom : sig

  val signalify : (unit -> 'a) -> 'a React.S.t

  val eventify_mouse :
      (#Dom_html.eventTarget as 'a) Js.t ->
	(#Dom_html.event as 'b) Js.t Dom_events.Typ.typ ->
	  ('a Js.t -> 'b Js.t -> 'c) -> 'c React.event

  val eventify_keyboard :
      (#Dom_html.eventTarget as 'a) Js.t ->
	(#Dom_html.event as 'b) Js.t Dom_events.Typ.typ ->
	  ('a Js.t -> 'b Js.t -> 'c) -> 'c React.event

end

module Regexp : sig
  type t
  type flag = Global_search | Case_insensitive | Multi_line
  val last_index : t -> int
  val make :
      ?global:bool ->
	?case_insensitive:bool -> ?multi_line:bool -> string -> t
  val test : t -> string -> bool
  val exec : t -> string -> string array
  val index : t -> string -> int
  val replace : t -> string -> string -> string
  val replace_fun : t -> (int -> string array -> string) -> string -> string
  val split : t -> string -> string array
end
