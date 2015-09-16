(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015 Vasilis Papavasileiou
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

(** Shared Cache for Eliom Applications *)

{shared{
type ('a, 'b) t

exception Not_ready
}}

{server{
val create :
  string ->
  'a Deriving_Json.t Eliom_lib.shared_value ->
  'b Deriving_Json.t Eliom_lib.shared_value ->
  ('a, 'b) t

val set : ('a, 'b) t -> 'a -> 'b -> unit Lwt.t
}}

{shared{

type ('a, 'b) et = Set of ('a * 'b) | Remove of 'a

val find : ('a, 'b) t -> 'a -> 'b Lwt.t

val event : ('a, 'b) t -> ('a, 'b) et Eliom_react.Down.t

}}
