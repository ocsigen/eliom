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
  with type 'a Int64_map.t = 'a Eliom_lib_base.Int64_map.t
  with type 'a String_map.t = 'a Eliom_lib_base.String_map.t
  with type 'a Int_map.t = 'a Eliom_lib_base.Int_map.t

(** An ['a] client value on the client is just an ['a].
    See also {% <<a_api subproject="server" text="on the server" |
    Eliom_lib.client_value >> %}.
  *)
type 'a client_value = 'a

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

val error : ('a, unit, string, 'b) format4 -> 'a
val alert : ('a, unit, string, unit) format4 -> 'a
val jsalert : Js.js_string Js.t -> unit
val debug : ('a, unit, string, unit) format4 -> 'a
val debug_var : string -> 'a -> unit
val debug_exn : ('a, unit, string, unit) format4 -> exn -> 'a
val jsdebug : 'a -> unit
val trace : ('a, unit, string, unit) format4 -> 'a
val set_tracing : bool -> unit

val lwt_ignore : ?message:string -> unit Lwt.t -> unit

val encode_form_value : 'a -> string
val unmarshal_js_var : string -> 'a

val encode_header_value : 'a -> string

val js_array_to_list : 'a Js.js_array Js.t -> 'a list

(**/**)

type 'a escaped_value = 'a lazy_t

(** This is the counter part of {% <<a_api subproject="server"|
    val Eliom_lib.wrap_and_marshall_poly >> %} *)
val unescape_and_unwrap : string -> poly

module Client_value_data : sig
  type t = string Int_map.t Int64_map.t
  val closure_ids : t -> int64 list
  val instance_ids : int64 -> t -> int list
  val find : int64 -> int -> t -> poly
end

module Injection_data : sig
  type t = string String_map.t
  val names : t -> string list
  val find : string -> t -> poly
end
