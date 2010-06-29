(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2010
 * Raphaël Proust
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

module Engine :
sig
  val start : unit -> unit
  val stop : unit -> unit
  val running : bool React.S.t
end

module Channels :
sig

  val unwrap :
     'a Eliom_common_comet.chan_id Eliom_client_types.data_key
  -> 'a Eliom_common_comet.chan_id

  val register : 'a Eliom_common_comet.chan_id -> ('a -> unit Lwt.t) -> unit
  val unregister : 'a Eliom_common_comet.chan_id -> unit

end

module Buffered_channels :
sig

  val unwrap :
     'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key
  -> 'a Eliom_common_comet.buffered_chan_id

  val register : 'a Eliom_common_comet.buffered_chan_id -> ('a -> unit Lwt.t) -> unit
  val unregister : 'a Eliom_common_comet.buffered_chan_id -> unit

end
