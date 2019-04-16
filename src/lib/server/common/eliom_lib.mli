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

(** A few common functions used by Eliom. Extension of OCaml stdlib.
    See also {% <<a_api project="ocsigenserver" | module Ocsigen_lib>> %} *)

(** See {% <<a_api project="ocsigenserver"| module Ocsigen_lib>> %}. *)
include module type of Ocsigen_lib
  with type poly = Ocsigen_lib.poly
  and type yesnomaybe = Ocsigen_lib.yesnomaybe
  and type ('a, 'b) leftright = ('a, 'b) Ocsigen_lib.leftright
  and type 'a Clist.t = 'a Ocsigen_lib.Clist.t
  and type 'a Clist.node = 'a Ocsigen_lib.Clist.node

include module type of Eliom_lib_base
  with type 'a Int64_map.t = 'a Eliom_lib_base.Int64_map.t
  with type 'a String_map.t = 'a Eliom_lib_base.String_map.t
  with type 'a Int_map.t = 'a Eliom_lib_base.Int_map.t

type file_info = Ocsigen_extensions.file_info

val string_escape : string -> string

val to_json : ?typ:'a Deriving_Json.t -> 'a -> string
val of_json : ?typ:'a Deriving_Json.t -> string -> 'a

(** Deprecated. Use Lwt_log.ign_info_f instead *)
val debug: ('a, unit, string, unit) format4 -> 'a

(** Marshal an OCaml value into a string. All characters are escaped *)
val jsmarshal : 'a -> string

(** Extension of {% <<a_api project="lwt"| module Lwt_log>> %}. *)
module Lwt_log : sig
  include module type of Lwt_log
  with type level = Lwt_log.level
   and type logger = Lwt_log.logger
   and type section = Lwt_log.section
   and type template = Lwt_log.template
   and module Section = Lwt_log.Section
  val eliom : section
end


(** Return a base-64 encoded cryptographic safe string of the given length.
    Not implemented client-side. *)
val make_cryptographic_safe_string : ?len:int -> unit -> string
