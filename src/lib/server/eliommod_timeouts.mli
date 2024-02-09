(* Ocsigen
 * http://www.ocsigen.org
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

type kind = [`Service | `Data | `Persistent]

val set_default :
   ?scope_hierarchy:Eliom_common.scope_hierarchy
  -> [< kind]
  -> [< Eliom_common.cookie_level]
  -> float option
  -> unit

val find_global :
   [< kind]
  -> Eliom_common.full_state_name
  -> Eliom_common.sitedata
  -> float option

val get_global :
   kind:[< kind]
  -> cookie_scope:[< Eliom_common.cookie_scope]
  -> secure:bool
  -> Eliom_common.sitedata
  -> float option

val set_global :
   kind:[< kind]
  -> cookie_scope:[< Eliom_common.cookie_scope]
  -> secure:bool
  -> recompute_expdates:bool
  -> bool
  -> Eliom_common.sitedata
  -> float option
  -> unit

val set_global_ :
   ?full_st_name:Eliom_common.full_state_name
  -> ?cookie_level:[< Eliom_common.cookie_level]
  -> kind:[< kind]
  -> recompute_expdates:bool
  -> bool
  -> bool
  -> Eliom_common.sitedata
  -> float option
  -> unit

val set_default_global :
   [< kind]
  -> [< Eliom_common.cookie_level]
  -> bool
  -> bool
  -> Eliom_common.sitedata
  -> float option
  -> unit
