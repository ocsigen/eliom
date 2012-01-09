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

  (**/**)

  val uri_of_fun: (unit -> string) -> uri

  (* Building ref tree. *)
  type event_handler_table (* Concrete on client-side only. *)
  val get_unique_id : elt -> string option
  val make_event_handler_table : elt -> event_handler_table

  (* Custom event handlers and lazy nodes. *)
  type caml_event_handler

  val event_handler_of_string : string -> event_handler
  val string_of_event_handler : event_handler -> string
  val event_handler_of_js : int64 -> poly -> event_handler
  val event_handler_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option ) option Eliom_lazy.request -> event_handler

  (**/**)
  (* Deprecated alias. *)
  val event_of_string : string -> event_handler
  val string_of_handler : event_handler -> string
  val event_of_js : int64 -> poly -> event_handler
  val event_of_service :
    ( [ `A | `Form_get | `Form_post ]
      * (bool * string list) option ) option Eliom_lazy.request -> event_handler
  (**/**)

  type racontent =
    | RA of acontent
    | RACamlEventHandler of caml_event_handler
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

    (**/**)
    type ('a, 'b, 'c) lazy_plus =
      ?a: (('a attrib) list) -> 'b elt Eliom_lazy.request -> ('b elt) list Eliom_lazy.request -> 'c elt

    val lazy_form:
      ([< HTML5_types.form_attrib ], [< HTML5_types.form_content_fun ], [> HTML5_types.form ]) lazy_plus
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
