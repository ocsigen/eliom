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

type sessgrp
type perssessgrp

val make_full_group_name : string -> string option -> sessgrp option
val make_persistent_full_group_name : string -> string option -> perssessgrp option

val getsessgrp : sessgrp -> (string * string)
val getperssessgrp : perssessgrp -> (string * string)

module type MEMTAB =
  sig
    val find : sessgrp option -> string list
    val add : ?set_max: int option ->
      int option -> string -> sessgrp option -> string list
    val remove : string -> sessgrp option -> unit
    val remove_group : sessgrp option -> unit
    val move :
      ?set_max: int option ->
      int option ->
      string -> 
      sessgrp option -> 
      sessgrp option -> 
      string list
    val up : string -> sessgrp option -> unit
    val length : unit -> int
  end

module Serv : MEMTAB
module Data : MEMTAB

module Pers :
  sig
    val find : perssessgrp option -> string list Lwt.t
    val add : ?set_max: int option ->
      int option -> string -> perssessgrp option -> string list Lwt.t
    val remove : string -> perssessgrp option -> unit Lwt.t
    val remove_group : perssessgrp option -> unit Lwt.t
    val move : 
      ?set_max: int option ->
      int option -> 
      string -> 
      perssessgrp option -> 
      perssessgrp option -> 
      string list Lwt.t
    val up : string -> perssessgrp option -> unit Lwt.t
    val length : unit -> int Lwt.t
  end
