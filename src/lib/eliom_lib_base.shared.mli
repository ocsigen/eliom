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

exception Eliom_Internal_Error of string

module type Map_S = sig
  include Map.S

  val from_list : (key * 'a) list -> 'a t
  val to_string : ?sep:string -> ('a -> string) -> 'a t -> string
end

module Int64_map : Map_S with type key = int64
module Int_map : Map_S with type key = int
module String_map : Map_S with type key = string

(**/**)

type pos = Lexing.position * Lexing.position

val pos_to_string : pos -> string
