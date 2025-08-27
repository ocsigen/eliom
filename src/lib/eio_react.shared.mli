open Eio.Std

(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(** React utilities *)

(** This module is an overlay for the [React] module. You can open it
    instead of the [React] module in order to get all of [React]'s functions
    plus Lwt ones.

    This module is provided by OPAM package [lwt_react]. Link with ocamlfind
    package [lwt_react]. *)

type 'a event = 'a React.event
(** Type of events. *)

type 'a signal = 'a React.signal
(** Type of signals. *)

module E : sig
  include module type of React.E

  (** {2 Lwt-specific utilities} *)

  val with_finaliser : (unit -> unit) -> 'a event -> 'a event
  (** [with_finaliser f e] returns an event [e'] which behave as
        [e], except that [f] is called when [e'] is garbage
        collected. *)

  val next : 'a event -> 'a
  (** [next e] returns the next occurrence of [e].

      Avoid trying to create an “asynchronous loop” by calling [next e] again in
      a callback attached to the promise returned by [next e]:

      - The callback is called within the React update step, so calling [next e]
        within it will return a promise that is fulfilled with the same value as
        the current occurrence.
      - If you instead arrange for the React update step to end (for example, by
        calling [Lwt.pause ()] within the callback), multiple React update steps
        may occur before the callback calls [next e] again, so some occurrences
        can be effectively “lost.”

      To robustly asynchronously process occurrences of [e] in a loop, use
      [to_stream e], and repeatedly call {!Eliom_stream.next} on the resulting
      stream. *)

  val limit : (unit -> unit) -> 'a event -> 'a event
  (** [limit f e] limits the rate of [e] with [f].

        For example, to limit the rate of an event to 1 per second you
        can use: [limit (fun () -> Lwt_unix.sleep 1.0) event]. *)

  val from : (unit -> 'a) -> 'a event
  (** [from f] creates an event which occurs each time [f ()]
        returns a value. If [f] raises an exception, the event is just
        stopped. *)

  val to_stream : 'a event -> 'a Eliom_stream.t
  (** Creates a stream holding all values occurring on the given
        event *)

  val of_stream : 'a Eliom_stream.t -> 'a event
  (** [of_stream stream] creates an event which occurs each time a
        value is available on the stream.

        If updating the event causes an exception at any point during the update
        step, the exception is passed to [!]{!Lwt.async_exception_hook}, which
        terminates the process by default. *)

  val delay : 'a event Promise.t -> 'a event
  (** [delay promise] is an event which does not occur until
        [promise] resolves. Then it behaves as the event returned by
        [promise]. *)

  val keep : 'a event -> unit
  (** [keep e] keeps a reference to [e] so it will never be garbage
        collected. *)

  (** {2 Threaded versions of React transformation functions} *)

  (** The following functions behave as their [React] counterpart,
      except that they take functions that may yield.

      As usual the [_s] suffix is used when calls are serialized, and
      the [_p] suffix is used when they are not.

      Note that [*_p] functions may not preserve event order. *)

  val app_s : ('a -> 'b) event -> 'a event -> 'b event
  val app_p : ('a -> 'b) event -> 'a event -> 'b event
  val map_s : ('a -> 'b) -> 'a event -> 'b event
  val map_p : ('a -> 'b) -> 'a event -> 'b event
  val filter_s : ('a -> bool) -> 'a event -> 'a event
  val filter_p : ('a -> bool) -> 'a event -> 'a event
  val fmap_s : ('a -> 'b option) -> 'a event -> 'b event
  val fmap_p : ('a -> 'b option) -> 'a event -> 'b event
  val diff_s : ('a -> 'a -> 'b) -> 'a event -> 'b event
  val accum_s : ('a -> 'a) event -> 'a -> 'a event
  val fold_s : ('a -> 'b -> 'a) -> 'a -> 'b event -> 'a event
  val merge_s : ('a -> 'b -> 'a) -> 'a -> 'b event list -> 'a event
  val run_s : 'a Promise.t event -> 'a event
  val run_p : 'a Promise.t event -> 'a event
end

module S : sig
  include module type of React.S

  (** {2 Monadic interface} *)

  val return : 'a -> 'a signal
  (** Same as [const]. *)

  val bind :
     ?eq:('b -> 'b -> bool)
    -> 'a signal
    -> ('a -> 'b signal)
    -> 'b signal
  (** [bind ?eq s f] is initially [f x] where [x] is the current
        value of [s]. Each time [s] changes to a new value [y], [bind
        signal f] is set to [f y], until the next change of
        [signal]. *)

  val bind_s :
     ?eq:('b -> 'b -> bool)
    -> 'a signal
    -> ('a -> 'b signal)
    -> 'b signal
  (** Same as {!bind} except that [f] returns a promise. Calls to [f]
        are serialized. *)

  (** {2 Lwt-specific utilities} *)

  val with_finaliser : (unit -> unit) -> 'a signal -> 'a signal
  (** [with_finaliser f s] returns a signal [s'] which behaves as
        [s], except that [f] is called when [s'] is garbage
        collected. *)

  val limit : ?eq:('a -> 'a -> bool) -> (unit -> unit) -> 'a signal -> 'a signal
  (** [limit f s] limits the rate of [s] update with [f].

        For example, to limit it to 1 per second, you can use: [limit
        (fun () -> Lwt_unix.sleep 1.0) s]. *)

  val keep : 'a signal -> unit
  (** [keep s] keeps a reference to [s] so it will never be garbage
        collected. *)

  (** {2 Threaded versions of React transformation functions} *)

  (** The following functions behave as their [React] counterpart,
      except that they take functions that may yield.

      The [_s] suffix means that calls are serialized.
  *)

  val app_s :
     ?eq:('b -> 'b -> bool)
    -> ('a -> 'b) signal
    -> 'a signal
    -> 'b signal

  val map_s : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal

  val filter_s :
     ?eq:('a -> 'a -> bool)
    -> ('a -> bool)
    -> 'a
    -> 'a signal
    -> 'a signal

  val fmap_s :
     ?eq:('b -> 'b -> bool)
    -> ('a -> 'b option)
    -> 'b
    -> 'a signal
    -> 'b signal

  val diff_s : ('a -> 'a -> 'b) -> 'a signal -> 'b event
  val sample_s : ('b -> 'a -> 'c) -> 'b event -> 'a signal -> 'c event
  val accum_s : ?eq:('a -> 'a -> bool) -> ('a -> 'a) event -> 'a -> 'a signal

  val fold_s :
     ?eq:('a -> 'a -> bool)
    -> ('a -> 'b -> 'a)
    -> 'a
    -> 'b event
    -> 'a signal

  val merge_s :
     ?eq:('a -> 'a -> bool)
    -> ('a -> 'b -> 'a)
    -> 'a
    -> 'b signal list
    -> 'a signal

  val l1_s : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal

  val l2_s :
     ?eq:('c -> 'c -> bool)
    -> ('a -> 'b -> 'c)
    -> 'a signal
    -> 'b signal
    -> 'c signal

  val l3_s :
     ?eq:('d -> 'd -> bool)
    -> ('a -> 'b -> 'c -> 'd)
    -> 'a signal
    -> 'b signal
    -> 'c signal
    -> 'd signal

  val l4_s :
     ?eq:('e -> 'e -> bool)
    -> ('a -> 'b -> 'c -> 'd -> 'e)
    -> 'a signal
    -> 'b signal
    -> 'c signal
    -> 'd signal
    -> 'e signal

  val l5_s :
     ?eq:('f -> 'f -> bool)
    -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
    -> 'a signal
    -> 'b signal
    -> 'c signal
    -> 'd signal
    -> 'e signal
    -> 'f signal

  val l6_s :
     ?eq:('g -> 'g -> bool)
    -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
    -> 'a signal
    -> 'b signal
    -> 'c signal
    -> 'd signal
    -> 'e signal
    -> 'f signal
    -> 'g signal

  val run_s : ?eq:('a -> 'a -> bool) -> 'a Promise.t signal -> 'a signal
end
