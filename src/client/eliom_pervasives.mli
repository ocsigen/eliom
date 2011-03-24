
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
  (* val parse_ip : string -> t * t option *)
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

  val leaf : ?a:(attrib list) -> ename -> elt
  val node : ?a:(attrib list) -> ename -> elt list -> elt

  val cdata : string -> elt
  val cdata_script : string -> elt
  val cdata_style : string -> elt

  val lwt_register_event : ?keep_default:bool -> elt -> ename -> ('a -> 'b Lwt.t) -> 'a -> unit
  val register_event : ?keep_default:bool -> elt -> ename -> ('a -> 'b) -> 'a -> unit

  type ref_tree = Ref_tree of int option * (int * ref_tree) list
  val ref_node : 'a -> int

  val class_name : string

end

module SVG : sig

  module M : SVG_defs.T with type raw_xml_elt = XML.elt
                         and type raw_xml_attrib = XML.attrib

end

module XHTML5 : sig

  module M : XHTML5_defs.T with type raw_xml_elt       = XML.elt
                            and type raw_xml_attrib    = XML.attrib
                            and type raw_xml_event     = XML.event
                            and type 'a raw_svg_elt    = 'a SVG.M.elt
                            and type 'a raw_svg_attrib = 'a SVG.M.attrib

  module M_05_00 : XHTML5_defs.T_05_00 with type raw_xml_elt       = XML.elt
                                        and type raw_xml_attrib    = XML.attrib
                                        and type raw_xml_event     = XML.event
                                        and type 'a raw_svg_elt    = 'a SVG.M.elt
                                        and type 'a raw_svg_attrib = 'a SVG.M.attrib

end

module XHTML : sig

  module M : XHTML_defs.T with type raw_xml_elt       = XML.elt
                           and type raw_xml_attrib    = XML.attrib
                           and type raw_xml_event     = XML.event

  module M_01_01 : XHTML_defs.T with type raw_xml_elt       = XML.elt
                                 and type raw_xml_attrib    = XML.attrib
                                 and type raw_xml_event     = XML.event

  module M_01_00 : XHTML_defs.T with type raw_xml_elt       = XML.elt
                                 and type raw_xml_attrib    = XML.attrib
                                 and type raw_xml_event     = XML.event

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
