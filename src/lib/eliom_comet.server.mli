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

(** {b Please read the
    {% <<a_manual chapter="clientserver-communication" | chapter on
    communication >>%} of Eliom's manual
    before this page to learn how client and server parts communicate. }
 *)

(** Basic primitives needed for server push. *)
module Channel : sig
  type 'a t
  (** [v t] is the type of server-to-client communication channels
      transporting data of type [v] *)

  type comet_scope =
    [Eliom_common.site_scope | Eliom_common.client_process_scope]

  val create :
     ?scope:[< comet_scope]
    -> ?name:string
    -> ?size:int
    -> 'a Lwt_stream.t
    -> 'a t
  (** [create s] returns a channel sending the values returned by stream [s].

      There are two kinds of channels created depending on the given
      scope (defaults to [Eliom_common.comet_client_process]).

      With scope {!Eliom_common.site_scope} all users knowing the name of
      the channel can access it. Only one message queue is created: it
      is what we call a //stateless// channel in the sense that the memory
      used by the channel does not depend on the number of users.  The
      channel can be reclaimed by the GC when there is no more reference to it.
      The buffer channel has a limited buffer of size [size] (default:
      1000). If the client requests too old messages, exception
      [Eliom_coment.Channel_full] will be raised (on client side).

      With a scope of level {!Eliom_common.client_process_scope} the
      channel can only be accessed by the user who created it. It
      can only be created when client process data is
      available (that is: during a request).
      The eliom service created to communicate with the
      client is only available in the scope of the client process. To
      avoid memory leak when the client do not read sent data,
      the channel has a limited [size]. When a channel is full, no
      data can be read from it anymore.

      A channel can be used only once on client side. To be able
      to receive the same data multiple times on client side, use
      [create (Lwt_stream.clone s)] every time.

      To enforce the limit on the buffer size, the data is read into
      [stream] as soon as possible: If you want a channel that reads
      data on the stream only when the client requests it, use
      [create_unlimited] instead, but be careful of memory leaks. *)

  val create_from_events :
     ?scope:[< comet_scope]
    -> ?name:string
    -> ?size:int
    -> 'a React.event
    -> 'a t
  (** [create_from_events e] returns a channel sending the values returned
      by the event stream [e]. *)

  val create_unlimited :
     ?scope:Eliom_common.client_process_scope
    -> ?name:string
    -> 'a Lwt_stream.t
    -> 'a t
  (** [create_unlimited s] creates a channel which does not read
      immediately on the stream. It is read only when the client
      requests it: use it if the data you send depends on the time of
      the request (for instance the number of unread mails). Be
      careful, the size of this stream is not limited: if the size of
      the stream increases and your clients don't read it, you may have
      memory leaks. *)

  val create_newest : ?name:string -> 'a Lwt_stream.t -> 'a t
  (** [create_newest s] is similar to [create
      ~scope:Eliom_common.site_scope s] but only the last message is
      returned to the client. *)

  val external_channel :
     ?history:int
    -> ?newest:bool
    -> prefix:string
    -> name:string
    -> unit
    -> 'a t
  (** [external_channel ~prefix ~name ()] declares an external
      channel. The channel was created by an instance of Eliom serving
      the prefix [prefix] (the prefix configured in the <site> tag of
      the configuration file). The channel was named by [name]. Both
      servers must run the exact same version of Eliom.

      The optional [newest] parameters tells whether the channel is a
      new one. If the channel is not new, [history] is the maximum
      number of messages retrieved at the first request. The default
      is [1]. *)

  val wait_timeout :
     ?scope:Eliom_common.client_process_scope
    -> float
    -> unit Lwt.t
  (** [wait_timeout ~scope time] waits for a period of inactivity of
      length [time] in the [scope]. Only activity on stateful
      channels is taken into accounts.

      The default [scope] is [Eliom_common.comet_client_process]. *)

  (**/**)

  val get_wrapped : 'a t -> 'a Eliom_comet_base.wrapped_channel
end

val section : Lwt_log_core.section
