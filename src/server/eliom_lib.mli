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

(** Pervasives module for Eliom-server: it extends the OCaml stdlib and should always be opened. *)

(** See {% <<a_api project="ocsigenserver" | module Ocsigen_lib>> %} *)
include module type of Ocsigen_lib
  with type poly = Ocsigen_lib.poly
  and type yesnomaybe = Ocsigen_lib.yesnomaybe
  and type ('a, 'b) leftright = ('a, 'b) Ocsigen_lib.leftright
  and type 'a Clist.t = 'a Ocsigen_lib.Clist.t
  and type 'a Clist.node = 'a Ocsigen_lib.Clist.node
  and type Ip_address.t = Ocsigen_lib.Ip_address.t

include module type of Eliom_lib_base
  with type 'a Int64_map.t = 'a Eliom_lib_base.Int64_map.t
  with type 'a String_map.t = 'a Eliom_lib_base.String_map.t
  with type 'a Int_map.t = 'a Eliom_lib_base.Int_map.t
  with module Client_value_data := Eliom_lib_base.Client_value_data
  with module Injection_data := Eliom_lib_base.Injection_data

(** Client values on the server are created by the syntax [{{ ... }}] in the
    server section. Those values are abstract on the server and unwrapped
    (cf. {% <<a_manual chapter="wrapping"|(Un-)wrapping>> %}) to the actual
    value when used on the client side by [%var].
  *)
type 'a client_value

(**/**)
val create_client_value : 'a Eliom_server.Client_value.t -> 'a client_value
val client_value_client_value : 'a client_value -> 'a Eliom_server.Client_value.t
val escaped_value : 'a -> Eliom_server.escaped_value (* * Eliom_wrap.unwrapper *)
(**/**)

(** {2 Pervasives} *)

exception Eliom_Internal_Error of string

type file_info = Ocsigen_extensions.file_info

val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a
val debug: ('a, unit, string, unit) format4 -> 'a

(** Marshal an OCaml value into a string. All characters are escaped *)
val jsmarshal : 'a -> string

(**/**)

val string_escape : string -> string

module Client_value_data : sig

  include module type of Eliom_lib_base.Client_value_data
    with type elt = Eliom_lib_base.Client_value_data.elt
    with type global = Eliom_lib_base.Client_value_data.global
    with type request = Eliom_lib_base.Client_value_data.request
    with type base = Eliom_lib_base.Client_value_data.base
  type t = base * Eliom_wrap.unwrapper
  val with_unwrapper : base -> t

end

module Injection_data : sig

  include module type of Eliom_lib_base.Injection_data
  type t = ((string * poly) * Eliom_wrap.unwrapper) list
  val with_unwrapper : base -> t

end

(**/**)
