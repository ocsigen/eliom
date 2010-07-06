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


module Channels :
(** A module with all the base primitive needed for server push.*)
sig

  type 'a chan
  (** The type of channels transporting values of type 'a. Values are marshalled
      before transmition, it is for the client to properly unmarshall it. The
      Eliom_client_comet module provides primitives that allow just that. *)

  val create : ?name:string -> 'a React.E.t -> 'a chan
  (** [create e] makes a fresh new channel immediatly usable. The id can
      be transmitted to a client in order to let him collect information passed
      on it. The identifier for the channel can be manually specified so that it
      will still be valid after server restart.

      [Comet.Too_many_virtual_channels] may be raised if [max_virtual_channels]
      is exceeded. *)

  val get_id : 'a chan -> 'a Eliom_common_comet.chan_id
  (** [get_id c] returns a unique identifier associated to [c]. The client can
      register to [c] using the returned identifier. *)

  val wrap :
    sp:Eliom_sessions.server_params ->
    'a chan -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key
  (** [wrap sp c] wraps the channel [c] into the global data transmitted
      to the client with the application client-side code. The result is a value
      of type ['a chan_id] and it is the client responsability to register to
      the channel. Functions in Eliom_client_comet and Eliom_client_event can be
      of use in this matter. *)


end

module Buffered_channels :
(** A module with primitives needed for buffered channels manipulation. A
    buffered channel will not loose data (except for user specified case). *)
sig

  type 'a chan
  (** The type of buffered channels. Such channels transport values of type [('a
      * int) list] (where the [int] is an increasing identifier for values of
      type ['a]. [Eliom_client_comet] provides a module to use these. *)

  val create :
     max_size:int
  -> ?sizer:('a -> int)
  -> ?timer:('a -> float option)
  -> 'a React.E.t
  -> 'a chan
  (** [create ~max_size ?sizer ?timer e] creates a channel with [e] as a
      triggering event. The [sizer] argument defaults to [fun _ -> 1] and the
      [timer] argument defaults to [fun _ -> None]. When an occurrence of [e]
      happens, the size of the carried value [x] is computed with [sizer x]. The
      time given to the value is computed with [timer x]. When the time for [x]
      is up, it is erased from the buffer. And whenever more room is needed
      (because of occurrences of [e]) some previous elements may be erased too.
      No option is given for specifying the channel name. Named channels are for
      public use only and buffered channels don't behave as they should for
      multiple listeners. *)

  val get_id : 'a chan -> 'a Eliom_common_comet.buffered_chan_id
  (** Returns the unique identifier associated to the channel. *)

  val wrap :
    sp:Eliom_sessions.server_params ->
    'a chan -> 'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key

end

