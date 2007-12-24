(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessiongroups.ml
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

module type MEMTAB =
  sig
    val find : string option -> string list
    val add : string -> string option -> unit
    val remove : string -> string option -> unit
    val remove_group : string option -> unit
    val move :
      string -> string option -> string option -> unit
    val length : unit -> int
  end

module Serv : MEMTAB
module Data : MEMTAB

module Pers :
  sig
    val find : string option -> string list Lwt.t
    val add : string -> string option -> unit Lwt.t
    val remove : string -> string option -> unit Lwt.t
    val remove_group : string option -> unit Lwt.t
    val move : string -> string option -> string option -> unit Lwt.t
    val length : unit -> int Lwt.t
  end
