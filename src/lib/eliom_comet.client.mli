(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
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

(** Handle unsolicited server to client communications.

    See the Eliom manual for a detailed introduction to the concept of
    {% <<a_manual chapter="clientserver-communication"|client server communication>>%}. *)

(** When the page is not active the client stops making comet requests
    to the server, implying that the client can't be notified by the
    server anymore. The activity status is changed when the page is
    focused or unfocused.

    To stop receiving inputs from a channel, use Lwt.cancel on a
    thread waiting for data. For instance, if you iterate with
    [ let t = Lwt_stream.iter f %channel ] calling [Lwt.cancel t]
    will close the channel. *)

exception Channel_full
(** [Channel_full] is raised when trying to read on a channel marked
    full by the server. It is not possible to read anything else from a
    full channel. *)

exception Channel_closed
(** [Channel_closed] is raised when reading on a channel and the
    server side of the application closed channel ( the server was restarted,
    a session was closed, or a stateless channel was garbage collected).
     *)

val is_active : unit -> [`Active | `Idle | `Inactive]
(** [is_active ()] returns the current activity state *)

val activate : unit -> unit
(** if the client is inactive [activate ()] launch a new xhr
    connection to start receiving server messages *)

val set_handle_exn_function : (?exn:exn -> unit -> unit Lwt.t) -> unit
(** Makes possible to customize the function called when comet fails
    for unknown reason.
    The usual practice is to warn the user and ask to reload the page.
    This function is not called when a channel is full or closed.
    It is called only once, for the first exception.
*)

(** Change the reactivity of channels. Multiples configurations ( of
    type [t] ) can be created. The resulting behaviour is the minimal
    ( in the meaning of maximal reactivity ) between all
    configurations *)
module Configuration : sig
  type t

  val new_configuration : unit -> t
  (** Creates a new configuration with default value. It modifies the
      current behaviour immediately *)

  val drop_configuration : t -> unit
  (** [drop_configuration t] restores the behaviour to the minimum of
      configuration without [t]. If there is no other configuration
      than [t], it is restored to the defaults. *)

  val set_always_active : t -> bool -> unit
  (** [set_always_active c b] if b is true, tells the client to always
      stay active.
      Default value is false. *)

  val set_timeout : t -> float -> unit
  (** [set_timeout c t] tells the client to stay active at least [t]
      seconds when the application lose the focus.
      Default value is 180. *)

  val set_active_until_timeout : t -> bool -> unit
  (** [set_active_until_timeout c v] sets the activity changing
      behaviour. if [v] is [true] the page is kept active even if not
      focused until the client receive a timeout message from the
      server. It implies that if the server keeps sending data to the
      client, the comet connection will never be closed.
      Default value is false. *)

  val set_time_between_requests : t -> float -> unit
  (** after [set_time_between_requests t v], the main loop will wait for
      [v] seconds between two requests. It is taken into account
      immediately.
      Default value is 0.*)

  val set_time_between_requests_when_idle : t -> float * float * float -> unit
  (** [set_time_between_requests_when_idle t (a, b, c)] sets the time
      between two requests when the the windows does not have the focus,
      after the timeout.
      This amount of time is computed using an affine function
      (a * T + b), where T is the amount of time elapsed since the beginning
      of the idle phase, with a maximal time of c.
      If you want no request at all, do [set_always_active false].
      Setting this to [(0., 0., 0.)] is equivalent
      to [set_always_active true].
      Default value is [(0.5, 60., 600.)].
  *)
end

module Channel : sig
  type 'a t = 'a Lwt_stream.t
end

(**/**)

val register
  :  ?wake:bool
  -> 'a Eliom_comet_base.wrapped_channel
  -> 'a Lwt_stream.t
(** if wake is false, the registration of the channel won't
    activate the handling loop ( no request will be sent ). Default is true *)

val restart : unit -> unit
(** [restart ()] Restarts the loop waiting for server messages. It is
    only useful after that a formulary is sent. Indeed browsers stops
    all xhr requests in that case. It is normally not needed, but some
    browsers (based on webkit) also destroy the xhr object in that
    case, preventing client code from receiving the failure
    notification. This shouldn't be used by average user. *)

val close : 'a Eliom_comet_base.wrapped_channel -> unit
(** [close c] closes the channel c. This function should be only use
    internally. The normal way to close a channel is to cancel a thread
    waiting on inputs. *)

val force_link : unit

val handle_exn : ?exn:exn -> unit -> unit Lwt.t
(** This function calls manually the function
    that is usually called automatically when an exception
    is received during communication. *)

val section : Lwt_log_core.section
