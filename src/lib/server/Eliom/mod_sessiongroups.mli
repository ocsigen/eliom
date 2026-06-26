(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessiongroups.ml
 * Copyright (C) 2007 Vincent Balat
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

val make_full_named_group_name_ :
   cookie_level:Common.cookie_level
  -> Common.sitedata
  -> string
  -> Common.scope Common.sessgrp

val make_full_group_name :
   cookie_level:Common.cookie_level
  -> Ocsigen.Request.t
  -> string
  -> int
  -> int
  -> string option
  -> Common.scope Common.sessgrp

val make_persistent_full_group_name :
   cookie_level:Common.cookie_level
  -> string
  -> string option
  -> Common.perssessgrp option

val getsessgrp :
   Common.scope Common.sessgrp
  -> string * Common.cookie_level * (string, Ipaddr.t) Either.t

val getperssessgrp :
   Common.perssessgrp
  -> string * Common.cookie_level * (string, Ipaddr.t) Either.t

module type MEMTAB = sig
  type group_of_group_data

  val add :
     ?set_max:int
    -> Common.sitedata
    -> string
    -> [< Common.cookie_level] Common.sessgrp
    -> string Ocsigen_base.Cache.Dlist.node

  val remove : 'a Ocsigen_base.Cache.Dlist.node -> unit
  val remove_group : [< Common.cookie_level] Common.sessgrp -> unit

  val find :
     [< Common.cookie_level] Common.sessgrp
    -> string Ocsigen_base.Cache.Dlist.t
  (** returns the dlist containing all session group elements *)

  val find_node_in_group_of_groups :
     [< `Session] Common.sessgrp
    -> group_of_group_data option
  (** Groups of browser sessions belong to a group of groups.
        As these groups are not associated to a cookie,
        we put this information here. *)

  val move :
     ?set_max:int
    -> Common.sitedata
    -> string Ocsigen_base.Cache.Dlist.node
    -> [< Common.cookie_level] Common.sessgrp
    -> string Ocsigen_base.Cache.Dlist.node

  val up : string Ocsigen_base.Cache.Dlist.node -> unit
  val nb_of_groups : unit -> int
  val group_size : [< Common.cookie_level] Common.sessgrp -> int
  val set_max : 'a Ocsigen_base.Cache.Dlist.node -> int -> unit
end

module Serv :
  MEMTAB
  with type group_of_group_data =
    Common.tables ref * [`Session] Common.sessgrp Ocsigen_base.Cache.Dlist.node

module Data :
  MEMTAB
  with type group_of_group_data =
    [`Session] Common.sessgrp Ocsigen_base.Cache.Dlist.node

module Pers : sig
  val find : Common.perssessgrp option -> string list Lwt.t

  val add :
     ?set_max:int option
    -> int option
    -> string
    -> Common.perssessgrp option
    -> string list Lwt.t

  val remove :
     Common.sitedata
    -> string
    -> Common.perssessgrp option
    -> unit Lwt.t

  val remove_group :
     cookie_level:[`Session | `Client_process of Common.perssessgrp option]
    -> Common.sitedata
    -> Common.perssessgrp option
    -> unit Lwt.t

  val move :
     Common.sitedata
    -> ?set_max:int option
    -> int option
    -> string
    -> Common.perssessgrp option
    -> Common.perssessgrp option
    -> string list Lwt.t

  val up : string -> Common.perssessgrp option -> unit Lwt.t
  val nb_of_groups : unit -> int Lwt.t

  val close_persistent_session2 :
     cookie_level:Common.cookie_level
    -> Common.sitedata
    -> Common.perssessgrp option
    -> string
    -> unit Lwt.t
end
