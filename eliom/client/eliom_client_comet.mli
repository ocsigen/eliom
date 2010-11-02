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
(** The [Engine] is responsible for making asynchronous calls to the server,
    associating results with registered channels and triggering associated
    events. The interface is trimmed down to prevent low-level tampering and
    because of some implementation choices. When the engine is running a comet
    connection is opened to the server. *)
sig

  val start : unit -> unit
  (** [start ()] makes the engine start. If it was already running, nothing
      happens. If it wasn't, it first checks for channels of interest and then
      opens a connection to the server if such channels exists. *)

  val stop : unit -> unit
  (** [stop ()] makes the engine stop. If it wasn't running, nothing happens.
    * Registered channels are kept so that the server can be started again. *)

  val running : unit -> bool
  (** [running ()] reflects the current state the engine is in. *)

  val restart : unit -> unit
  (** [restart ()] makes the engine restart. If it was started, the current XHR
      is canceled and a new one is created. *)

end

module Channels :
(** [Channels] is a module for basic channel manipulation. On basic channels,
    messages may be lost. *)
sig

  val unwrap :
     'a Eliom_common_comet.chan_id Eliom_client_types.data_key
  -> 'a Eliom_common_comet.chan_id
  (** [unwrap c] returns a channel identifier that can be used to register upon.
    *)

  val register : 'a Eliom_common_comet.chan_id -> ('a -> unit Lwt.t) -> unit
  (** [register c f] registers the channel [c], associating it to [f] on the
      engine.  If the engine wasn't running it is automatically started.
      Whenever a message [m] from the server reaches the client over the channel
      [c], the function [f] is called with [m] as argument. Calls to [f] are not
      sequentialized.
      *)

  val unregister : 'a Eliom_common_comet.chan_id -> unit
  (** [unregister c] cancel registration on [c]. The function associated to [c]
      won't be called anymore and further comet related XHR won't mention [c].
    *)

end

module Buffered_channels :
(** [Dlisted_channels] is a module for buffered channels manipulation. Such a
    channel tends not to lose as many messages. All the functions have the same
    semantic, the only difference is in implementation, mainly on the server
    side (where some values are stored and retransmitted when needed). *)
sig

  val unwrap :
     'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key
  -> 'a Eliom_common_comet.buffered_chan_id

  val register : 'a Eliom_common_comet.buffered_chan_id -> ('a -> unit Lwt.t) -> unit
  val unregister : 'a Eliom_common_comet.buffered_chan_id -> unit

end
