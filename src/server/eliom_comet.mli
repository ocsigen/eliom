(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust
 * Pierre Chambart
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

(** Primitives to push data to the client, without explicit request. *)
module Channels :
(** Basic primitives needed for server push. *)
sig

  type 'a t
  (** [v t] is the type of server-to-client communication channels
      transporting data of type [v] *)

  type comet_scope =
    [ Eliom_common.global_scope
    | Eliom_common.client_process_scope ]

  val create : ?scope:[< comet_scope ] ->
    ?name:string -> ?size:int -> 'a Lwt_stream.t -> 'a t
  (** [create s] returns a channel sending values from [s].

      There are two kind of channels created depending on the given
      scope ( defaults to [Eliom_common.comet_client_process] ).

      With scope [`Global] all user knowing the name of the channel
      can access it. Only one message queue is created: it is what we
      call a stateless channel in the sense that the memory used by
      the channel doesn't depend on the number of users.
      The channel can be reclaimed when there is no more reference to it.
      The buffer channel has a limited buffer of size [size] (default: 1000).
      If the client request too old messages, it raise ( on client side )
      Channel_full.

      With scope [`Client_process _] the channel can only be accessed
      by the user which created it. It can only be created when client
      application datas are available. The eliom service created to
      communicate with the client is only available in the scope of
      the client process. To avoid memory leak when the client do not
      read the sent datas, the channel has a limited [size]. When a
      channel is full, no data can be read from it anymore.

      A channel can be used only one time on client side. To be able
      to receive the same data multiples times on client side, use
      [create (Lwt_stream.clone s)] each time.

      To enforce the limit on the buffer size, the data are read into
      [stream] as soon as possible: If you want a channel that read
      data to the stream only when the client request it, use
      [create_unlimited] instead, but be carefull to memory leaks. *)

  val create_unlimited : ?scope:Eliom_common.client_process_scope ->
    ?name:string -> 'a Lwt_stream.t -> 'a t
  (** [create_unlimited s] creates a channel wich does not read
      immediately on the stream it is read only when the client
      request it: use it if the data you send depends on the time of
      the request ( for instance the number of unread mails ). Be
      carefull the size of this stream is not limited: if the size of
      the stream increase and your clients don't read it, you may have
      memory leaks. *)

  val create_newest : ?name:string -> 'a Lwt_stream.t -> 'a t
  (** [create_newest s] is similar to [create ~scope:`Global s] but
      only the last message is returned to the client. *)

  (**/**)

  val get_wrapped : 'a t -> 'a Eliom_comet_base.wrapped_channel

end

