(* Ocsigen
 * http://www.ocsigen.org
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
(** A module with all the base primitive needed for server push. This is a high
    level wrap of the Comet Ocsigen extension, adding support for values of
    non string type. *)
sig

  type 'a t
  (** The type of channels carrying values of type 'a. Values are marshalled
      before transmission, it is up to the client to properly unmarshall it. The
      [Eliom_client_comet] module provides primitives that allow just that. *)

  val write : 'a t -> 'a -> unit
  (** [write c x] sends the value [x] over the channel [c]. Every client
      registered on the said channel will receive the value [x], except in case
      of connexion interruption. *)

  val create : ?name:string -> unit -> 'a t
  (** [create ?name ()] returns a fresh channel. If the [name] argument is
      provided then the channel identifier will be the given value. Note that in
      this case if the name is already attributed to a channel, the exception
      [Comet.Channels.Non_unique_channel_name] is raised. *)

  val get_id : 'a t -> 'a Eliom_common_comet.chan_id
  (** [get_id c] returns a unique identifier associated to [c]. The client can
      register to [c] using the returned identifier. *)

  val wrap :
    sp:Eliom_request_info.server_params ->
    'a t -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key
  (** [wrap sp c] wraps the channel [c] into the global data transmitted
      to the client with the application client-side code. The result is a value
      of type ['a chan_id] and it is the client responsibility to register to
      the channel. Functions in [Eliom_client_comet] and [Eliom_client_event]
      can be of use in this matter. *)

end

module Dlisted_channels :
(** A module with primitives needed for buffered channels manipulation. A
    buffered channel will not loose data (except for user specified case). *)
sig

  type 'a t
  (** The type of buffered channels. Such channels transport values of type
      ['a list]. [Eliom_client_comet] provides a module to use these. *)

  val write : 'a t -> 'a -> unit
  (** [write c x] either sends [x] on [c] or adds it in the buffer associated to
      [c] if no clients have a connection opened on [c]. When a client reopens a
      connection to such a channel, the buffer content is flushed. *)

  val create :
    max_size:int -> ?timer:float ->
    ?name:string ->
    unit -> 'a t
  (** [create ~max_size ?timer ?name ()] returns a fresh channel. The [name]
    * argument is the same as in [Channels.create] and the same exception is
    * raised in the same case. The [max_size] value indicates the number of
    * values that should be kept in the buffer, older values will be deleted
    * whenever a [write] is performed on a maxed out buffered channel. The
    * [timer] argument allow for value lifespan control. Values pushed in the
    * buffer won't be available for more than [timer] seconds. *)

  val get_id : 'a t -> 'a Eliom_common_comet.buffered_chan_id
  (** Returns the unique identifier associated to the channel. *)

  val wrap :
    sp:Eliom_request_info.server_params ->
    'a t -> 'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key
  (** [wrap] is to be executed automatically when transmitting global values to
      the client. *)

end

