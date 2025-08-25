(* This file is released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. 
   
   It is a partial translation of Lwt_react for Eio
*)

(** React utilities *)

(** This module is an overlay for the [React] module. You can open it
    instead of the [React] module in order to get all of [React]'s functions
    plus Eio ones.
 *)

type 'a event = 'a React.event
(** Type of events. *)

type 'a signal = 'a React.signal
(** Type of signals. *)

module E : sig
  include module type of React.E

  (** {2 Eio-specific utilities} *)

  val with_finaliser : (unit -> unit) -> 'a event -> 'a event
  (** [with_finaliser f e] returns an event [e'] which behave as
        [e], except that [f] is called when [e'] is garbage
        collected. *)

  val limit : (unit -> unit) -> 'a event -> 'a event
  (** [limit f e] limits the rate of [e] with [f].

        For example, to limit the rate of an event to 1 per second you
        can use: [limit (fun () -> Eio_unix.sleep 1.0) event]. *)

  val to_stream : 'a event -> 'a Eliom_stream.t
  (** Creates a stream holding all values occurring on the given
        event *)

  val of_stream : 'a Eliom_stream.t -> 'a event
  (** [of_stream stream] creates an event which occurs each time a
        value is available on the stream.

        If updating the event causes an exception at any point during the update
        step, the exception is passed to [!]{!Lwt.async_exception_hook}, which
        terminates the process by default. *)
end

module S : sig
  include module type of React.S

  val limit : ?eq:('a -> 'a -> bool) -> (unit -> unit) -> 'a signal -> 'a signal
  (** [limit f s] limits the rate of [s] update with [f].

        For example, to limit it to 1 per second, you can use: [limit
        (fun () -> Eio_unix.sleep 1.0) s]. *)
end
