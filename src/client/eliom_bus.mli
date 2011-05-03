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

(** Broadcasting facilities between clients and server *)

type 'a t

val stream : 'a t -> 'a Lwt_stream.t
(** [stream b] returns the stream of datas sent to bus [b]. Notice you
    sould not use that function multiple times on the same bus in the
    same client process, it will return the same stream. If you want
    to receive mutiple times the same datas, you sould copy the stream
    with [Lwt_stream.clone] *)

val write : 'a t -> 'a -> unit Lwt.t
(** [write b v] send [v] to the bus [b]. Every participant of the bus
    will receive [v], including the sender. *)

val close : 'a t -> unit
(** after [close b], [stream b] stops receiving new messages from the
    bus, but it is still possible to write to the bus. It is also
    possible to close the bus by canceling a thread reading on the
    stream. *)

val set_queue_size : 'a t -> int -> unit
(** To reduce traffic from the client busses try to send messages by
    group. [set_queue_size bus size] set the maximum number of
    messages that are accumulated before forcing a flush.
    default is 20 *)

val set_time_before_flush : 'a t -> float -> unit
(** [set_time_before_flush bus time] set the maximum time to wait for
    a new message to enqueue before flushing. Set this to a small
    value to make your app more responsive, but remember that it will
    consume more bandwidth. default is 0.05 second. *)
