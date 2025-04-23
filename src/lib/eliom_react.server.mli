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

(** Propagate events
    occurrences from the server to the client and the other way
    around. Occurrence propagation is done asynchronously.

    Warning: it is not possible to catch exceptions on each channel
    using Eliom_react (for example channel closed or full).
    If you need an error handling, use Eliom_comet.Channel instead.
*)

(** {b Please read the
    {% <<a_manual chapter="clientserver-communication" | chapter on
    communication >>%} of Eliom's manual
    before this page to learn how client and server parts communicate. }

    The use of this module is pretty much useless without it's client counter
    part.
*)

(* These two dual files are to be modified together
   with compatibility issues in mind. *)

(** Event from server to client. *)
module Down : sig
  (** A "Down event" (AKA down-going event) is an event which occurrences are
      transmitted asynchronously to the client. Even if they are named "events",
      it might be better to consider them as asynchronous server-to-client
      edges in the react events dependency graph.

      To use this, call function [of_react] on server side,
      and just use the returned value as a react event on client side.
      Example:
      [let e = of_react ... in ... {{ ... React.E.map f %e; ... }}]
 *)

  type 'a t
  (** The abstract type of down events. *)

  val of_react :
    ?scope:[< Eliom_comet.Channel.comet_scope ] ->
    ?throttling:float ->
    ?name:string ->
    ?size:int ->
    'a React.E.t ->
    'a t
  (** [of_react ?scope ?throttling ?name e] create an
      asynchronous edge originating from [e]. The parameters are:
      - [throttling]
      for the limit to event propagation rate (minimum time, in second,
      between two consecutive events - other events are lost),
      - [name] for named edges,
      - [size] for the size of the server side buffer.
      - [scope]
      tell which kind of channel this rely on (See [Eliom_comet.create]). *)
end

(** Event from client to server. *)
module Up : sig
  (** Up events are quite different from Down events. Because of the
      asymmetrical nature of web programming and because of the reactive model
      used, an Up event must be created on the server and wrapped into a
      callback (or something the client can build a callback with).

      Example of use:
      [let e_up = Eliom_react.Up.create
        (Eliom_parameter.ocaml "a" [%json: string])
      in
      ... {{ ignore ( %e_up "A") }} ...
      ]
    *)

  type 'a t
  (** The type of events that, while being "on the server", are triggered by
      clients. On the server such an event is /primitive/ (hence the [create]
      function) whereas it is /effect-full/ on the client. *)

  val to_react : 'a t -> 'a React.E.t
  (** [to_react e] injects the up events [e] into react events so that it can
      be manipulated as a standard event. *)

  val create :
    ?scope:Eliom_common.scope ->
    ?name:string ->
    ( 'a,
      [ `WithoutSuffix ],
      [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name )
    Eliom_parameter.params_type ->
    'a t
  (** [create param] creates an Up event.
      If [~name] is present, the coservice used to transmit the event will
      always have the same name, even if the server is restarted.
      [~scope] describes the visibility of the event. By default, it is
      [`Site] if it is called during initialisation,
      [`Client_process] otherwise.
  *)
end

module S : sig
  module Down : sig
    type 'a t
    (** The abstract type of down signals. *)

    val of_react :
      ?scope:[< Eliom_comet.Channel.comet_scope ] ->
      ?throttling:float ->
      ?name:string ->
      'a React.S.t ->
      'a t
  end
end
