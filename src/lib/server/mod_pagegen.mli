(* Ocsigen
 * http://www.ocsigen.org
 * Module eliommod_pagegen.ml
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
val def_handler : exn -> 'b Lwt.t

val execute :
   float
  -> (float -> Common.info -> Common.sitedata -> Ocsigen.Response.t Lwt.t)
  -> Common.info
  -> Common.sitedata
  -> Ocsigen.Response.t Lwt.t

val gen :
   Extension.eliom_extension_sig option
  -> Common.sitedata
  -> Ocsigen.Extensions.request_state
  -> Ocsigen.Extensions.answer Lwt.t

val update_cookie_table :
   ?now:float
  -> Common.sitedata
  -> Common.tables Common.cookie_info
  -> unit Lwt.t
