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

  exception Too_many_virtual_channels
  (** An exception that may be raised when the channel count exceed
      [max_virtual_channels]. Note that by default [max_virtual_channels] is set
      to [None] so that the exception may never be raised. *)

  type chan
  (** The type of server-to-client communication channels. *)

  type chan_id = string
  (** The type of channel identifier. *)

  val create : (string * int option) React.E.t -> chan
  (** [create e] makes a fresh new channel upon which a message is sent whenever
      [e] has an occurrence. When [e] as an occurence of value [s, None] then
      [s] is sent and no effort is made to detect transmition errors. If [s,
      Some n] is sent upon [e] AND a client is registered to the channel, then
      [outcomes] will be triggered with [x] as a witness.

      The [Too_many_virtual_channels] exception may be raised if [create] is
      called when the count of channels exceed [max_virtual_channels]. *)

  val outcomes : chan -> (Ocsigen_stream.outcome * int) React.E.t
  (** [outcomes c] is an event triggered with [o, x] when an occurrence of
      the event associated to [c] is triggered with [Some x] as a second
      composant. The value of [o] is either [`Failure] or [`Success] depending
      on the way the communication went.

      There are several limitations to this :
      * The information is only measured by server. The client takes no part in
        occurrences of this event !
      * When no client is registered to the channel, no feedback is provided.
        Because of this, the [listeners] function is provided.
      * When several clients are registered to the channel it is not possible to
        distinguish successes and failures on a per client basis.
      These limitations are being worked upon. *)

  val listeners : chan -> int React.S.t
  (** [listeners c] is the ever updated number of client actively registered on
      [c]. A client is "actively registered" on a channel if an actual
      connection is open for the server to push a message to. Note that this
      information is server-based only, and that because it is so, some clients
      may still be registered as active while they have in fact closed the
      connection. In such a case, [outcomes c] will trigger an event (providing
      the event used to create the channel have an actual value in the second
      composant of it's occurence). *)

  val get_id : chan -> chan_id
  (** [get_id c] returns a unique identifier associated to [c]. The client can
      register to [c] using the returned identifier. *)

end

(** Usage :

  On the server side :
    1) create needed channels with appropriate events
    2) transmit their identifiers to clients
    3) optionally lift the outcome event for feedback

  On the client :
    1) make a XmlHttpRequest (XHR) with a list of channel identifiers.
    2) wait for the reply
    3) GOTO 1

  Encoding for client-to-server requests:
    * The content type header should be set to [application/x-ocsigen-comet]
      (without quotes)
    * A POST parameter is required. Its name should be [registration] and its
      content should be a list of channel identifiers separated by [\n]
      (newline) characters.
    * Name and content of the said POST parameter should be encoded according to
      [escape] JavaScript primitive


  Encoding for server-to-client answer:
    * The server answer is either empty (when no channel was written upon before
      timeout) or a list of pairs of channel identifiers and message content.
      The pairs are separated by [:] (colon) while the list elements are
      separated by [\n] (newline) characters.

  *)

(** Note to Eliom users :
    Although it is possible to use Comet as an extension to the Ocsigen Server,
    it is recommended to use the higher level Eliom modules, namely Eliom_comet
    (for server side) and Eliom_client_comet (for client side). The former
    provides typed channels (with automatic marshaling) and channel wrapping,
    the later automates decoding and unmarshaling and manages channel
    registration and unregistration.

    The low level Ocisgen server extension can however be used with classic
    Javascript clients (whereas the high level Eliom module requires Ocaml
    compatible unmarshalling which may be difficult to find in a non
    js_of_ocaml/O'browser based client). It may also be used to add your own
    high level wrapper with a custom communication protocol.
  *)
