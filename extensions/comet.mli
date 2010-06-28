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
  (** The type of server-to-client communication channels. *)

  type chan_id = string
  (** The type of channel identifier. *)

  val create : string React.E.t -> chan
  (** [create e] makes a fresh new channel upon which a message is sent whenever
      [e] has an occurrence. *)

  val outcomes : chan -> (Ocsigen_stream.outcome * string) React.E.t
  (** [outcomes c] is an event triggered with [`Failure,s] when a call to [write
      c s] fails (eg. when no one is listening) and [`Success,s] when a call to
      [write c s] is successful.
      Note that this information is only measured by server. The client takes no
      part in occurrences of this event ! *)

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
