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

(** Comet server extension : provides low-level server to client communication
    scheme. *)

module Channels :
(** A module with all the base primitive needed for server push. *)
sig

  type chan
  (** The type of server-to-client communication channels *)  

  val create : unit -> chan
  (** [create ()] makes a fresh new channel immediately usable. *)

  val write : chan -> string -> unit
  (** [write c s] transmit the string [s] onto the channel [c]. Any client
      collecting values from [c] will receive [s]. *)

  val notification : chan -> string React.E.t
  (** [notification c] is an event that occurs with [s] whenever a client sends
      a notification [s] for the channel [c]. This is useful to implement an ACK
      system on private channels. *)

  val get_id : chan -> string
  (** [get_id c] returns a unique identifier associated to [c]. The client can
      register to [c] using the returned identifier. *)

end

(** Usage :
   
    On the server side :
      1) create needed channels
      2) transmit their identifiers to clients
      3) write on a channel when appropriate
      3') optionnaly lift the notification event for feedback
   
    On the client :
      1) make a XmlHttpRequest (XHR) with the encoded list of channel ids and
         some notification notice if appropriate.
           encoding for request :
             content-type is : "application/x-ocsigen-comet"
             POST parameter "registration" : list of channel ids (in clear text)
               separated by semi-colon character
             POST parameter "notification" : list (separated by semi-colon) of
               pairs (separated by colon) of channel id and string.
      2) wait for the encoded reply
           encoding for reply : list (separated by semi-colon) of pairs
           (separated by colon) of channel identifier and string message. Eg :
           "a:foo;b:bar" for message "foo" on the channel "a" and "bar" on "b"
      3) GOTO 1

    WARNING : in between the server answering the request and the client
    reopening it, messages written upon a channel won't reach the said client.
   
    *)

(** Note to Eliom users :
    Although it is possible to use Comet as an extension to the Ocsigen Server,
    it is recommended to use the higher level Eliom modules, namely Eliom_comet
    (for server side) and Eliom_client_comet (for client side). The former
    provides typed channels (with automatic marshaling) and channel wrapping,
    the later automates decoding and unmarshaling and manages channel
    registration and unregistration.
    *)
