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
(*   and type 'a Int64_map.t = 'a Ocsigen_lib.Int64_map.t *)

include module type of Eliom_lib_base

(** The type of holes [{{ ... }}].
    A client value on the server is abstract, as defined in {% <<a_api subproject="server" | type
    Eliom_server.Client_value.t >> %}. See also {% <<a_api subproject="client" text="on the client" |
    Eliom_lib.Client_value.t >> %}.
  *)
type 'a client_value = 'a Eliom_server.Client_value.t

(** {2 Pervasives} *)

exception Eliom_Internal_Error of string

type file_info = Ocsigen_extensions.file_info

val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a
val debug: ('a, unit, string, unit) format4 -> 'a

