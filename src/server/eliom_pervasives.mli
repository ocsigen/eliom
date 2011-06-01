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

(** Pervasives module for Eliom extending stdlib, should always be opened. *)

exception Eliom_Internal_Error of string

external id : 'a -> 'a = "%identity"

val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
val (!!) : 'a Lazy.t -> 'a

type yesnomaybe = Yes | No | Maybe
type ('a, 'b) leftright = Left of 'a | Right of 'b

val map_option : ('a -> 'b) -> 'a option -> 'b option

val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val thd3 : 'a * 'b * 'c -> 'c

type poly (* Warning: do not use [poly array]... *)
val to_poly: 'a -> poly
type 'a client_expr = int64 * poly
type 'a wrapped_value = poly * 'a

module List : sig
  include module type of List
  val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
  val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_first_if_any : 'a -> 'a list -> 'a list
  val remove_first_if_any_q : 'a -> 'a list -> 'a list
end

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

module Ip_address : sig

  type t = Ocsigen_pervasives.Ip_address.t =
    | IPv4 of int32
    | IPv6 of int64 * int64

  val parse : string -> t * (t option)

  val network_of_ip : t -> int32 -> int64 * int64 -> t
  val inet6_addr_loopback : t

end

module Filename : sig
  include module type of Filename
end

module Printexc :  sig
  include module type of Printexc
end

module Int : sig
  module Table : Map.S with type key = int
end

val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a

(** XML building and deconstructing. *)
module XML : sig

  type aname = string
  type separator = Space | Comma

  type attrib
  type acontent = private
    | AFloat of aname * float
    | AInt of aname * int
    | AStr of aname * string
    | AStrL of separator * aname * string list
  val acontent : attrib -> acontent
  val aname : attrib -> aname

  type caml_event
  type event

  val event_of_string : string -> event
  val string_of_event : event -> string
  val event_of_js : int64 -> poly -> event
  val event_of_service_a : (bool * Url.path) option -> event

  type racontent =
    | RA of acontent
    | RACamlEvent of (aname * caml_event)
  val racontent : attrib -> racontent

  val float_attrib : aname -> float -> attrib
  val int_attrib : aname -> int -> attrib
  val string_attrib : aname -> string -> attrib
  val space_sep_attrib : aname -> string list -> attrib
  val comma_sep_attrib : aname -> string list -> attrib
  val event_attrib : aname -> event -> attrib

  type elt
  type ename = string
  type econtent = private
    | Empty
    | Comment of string
    | EncodedPCDATA of string
    | PCDATA of string
    | Entity of string
    | Leaf of ename * attrib list
    | Node of ename * attrib list * elt list
  val content : elt -> econtent

  val make : econtent -> elt

  val empty : unit -> elt

  val comment : string -> elt
  val pcdata : string -> elt
  val encodedpcdata : string -> elt
  val entity : string -> elt
    (** Neither [comment], [pcdata] nor [entity] check their argument
	for invalid characters.  Unsafe characters will be escaped later
	by the output routines. *)

  val leaf : ?a:(attrib list) -> ename -> elt
  val node : ?a:(attrib list) -> ename -> elt list -> elt
    (** NB: [Leaf ("foo", []) -> "<foo />"],
	but [Node ("foo", [], []) -> "<foo></foo>"] *)

  val cdata : string -> elt
  val cdata_script : string -> elt
  val cdata_style : string -> elt

  val make_unique : ?copy:elt -> elt -> elt
  val is_unique : elt -> bool
  val get_unique_id : elt -> string option

  module Hashtbl : Hashtbl.S with type key = elt

  val hash: elt -> int
  val compare: elt -> elt -> int

  type ref_tree
  val make_ref_tree : elt -> ref_tree
  val make_ref_tree_list : elt list -> ref_tree list

end

module SVG : sig

  (** Type safe SVG creation. *)
  module M : sig
    include SVG_sigs.T with module XML := XML
    val unique: 'a elt -> 'a elt
  end

  module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a M.elt
					  and type doc := M.doc

end

module HTML5 : sig

  (** Type safe HTML5 creation. *)
  module M : sig
    include HTML5_sigs.T with module XML := XML and module SVG := SVG.M
    val unique: 'a elt -> 'a elt
  end
  module P : XML_sigs.TypedSimplePrinter with type 'a elt := 'a M.elt
					  and type doc := M.doc

end

module XHTML : sig

  (** Type safe XHTML creation. *)
  module M : XHTML_sigs.T with module XML := XML
  module P : XML_sigs.TypedSimplePrinter
             with type 'a elt := 'a M.elt
	      and type doc := M.doc

  module M_01_00 : XHTML_sigs.T with module XML := XML
  module P_01_00 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_00.elt
		    and type doc := M_01_00.doc
  module P_01_00_compat : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_00.elt
		    and type doc := M_01_00.doc

  module M_01_01 : XHTML_sigs.T with module XML := XML
  module P_01_01 : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_01.elt
		    and type doc := M_01_01.doc
  module P_01_01_compat : XML_sigs.TypedSimplePrinter
                   with type 'a elt := 'a M_01_01.elt
		    and type doc := M_01_01.doc

end

type file_info = Ocsigen_extensions.file_info
