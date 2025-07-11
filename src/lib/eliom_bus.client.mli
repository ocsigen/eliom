(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
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

(** Broadcasting facilities between clients and server.

    See the Eliom manual for a detailed introduction to the concept of
    {% <<a_manual chapter="clientserver-communication"|client server communication>>%}. *)

type ('a, 'b) t

val register : ('a, 'b) t -> ('b option -> unit Lwt.t) -> unit
(** Register a callback that will get called on every messages from the server.
    Messages received before the call to [register] are lost. The callback is
    called with [Some data] when receiving a message from the server or with
    [None] when no more data will be received. *)

val stream : ('a, 'b) t -> 'b Lwt_stream.t
(** Create a new stream from the messages from the server. This has the same
    behavior as {!register}. *)

val original_stream : ('a, 'b) t -> 'b Lwt_stream.t
(** @deprecated Deprecated alias to [stream]. *)

val write : ('a, 'b) t -> 'a -> unit Lwt.t
(** [write b v] send [v] to the bus [b]. Every participant of the bus
    will receive [v], including the sender. *)

val close : ('a, 'b) t -> unit
(** after [close b], [stream b] stops receiving new messages from the
    bus, but it is still possible to write to the bus. It is also
    possible to close the bus by canceling a thread reading on the
    stream. *)

val set_queue_size : ('a, 'b) t -> int -> unit
(** To reduce traffic from the client busses try to send messages by
    group. [set_queue_size bus size] set the maximum number of
    messages that are accumulated before forcing a flush.
    default is 20 *)

val set_time_before_flush : ('a, 'b) t -> float -> unit
(** [set_time_before_flush bus time] set the maximum time to wait for
    a new message to enqueue before flushing. Set this to a small
    value to make your app more responsive, but remember that it will
    consume more bandwidth. default is 0.05 second. *)

(**/**)

val force_link : unit
val section : Logs.src
