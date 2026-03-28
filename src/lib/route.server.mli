(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_services.ml
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

open Lib

val get_page :
   float
  -> Common.info
  -> Common.sitedata
  -> Ocsigen_response.t Lwt.t

val add_service :
   int
  -> Common.tables
  -> String.Table.key list
  -> Common.Serv_Table.key
  -> (Common.server_params, Ocsigen_response.t) Common.service
  -> unit

val remove_service :
   Common.tables
  -> String.Table.key list
  -> Common.Serv_Table.key
  -> Common.anon_params_type * Common.anon_params_type
  -> unit

val add_naservice :
   Common.tables
  -> Common.NAserv_Table.key
  -> int ref option
     * (float * float ref) option
     * (Common.server_params -> Ocsigen_response.t Lwt.t)
  -> unit

val remove_naservice :
   Common.tables
  -> Common.NAserv_Table.key
  -> unit

val make_naservice :
   float
  -> Common.info
  -> Common.sitedata
  -> Ocsigen_response.t Lwt.t
