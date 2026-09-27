(** A Comet client, which requests the data of channels as the client-side
    program of Eliom does. *)

exception State_closed
(** Raised when the state of the client process of a request is closed. *)

exception Comet_error of string
(** Raised when the server answers with an error. *)

type 'a message =
  | Data of 'a  (** A value sent on the channel *)
  | Full  (** Messages were lost: the buffer of the channel was full *)
  | Closed  (** The stream of the channel ended *)

val register : Tab.t -> Comet_info.t -> unit Lwt.t
(** [register t c] registers the channel [c], of the client process of [t],
    to be requested by [t]. *)

val close : Tab.t -> Comet_info.t -> unit Lwt.t
(** [close t c] tells that [t] no longer requests [c]. *)

val request :
   ?idle:bool
  -> Tab.t
  -> Comet_info.t
  -> int
  -> (string * 'a message) list Lwt.t
(** [request t c n] is the [n]th request of data of the channels registered by
    [t] (the Comet service of [c]), as channel identifiers and messages. It
    waits for data, unless [idle] is [true] (default [false]). The type of
    the values is the one of the channels: it is not checked. *)

val request_stateless :
   Eliom_test_server.Browser.t
  -> Comet_info.t
  -> Eliom.Comet_base.position
  -> (string * ('a * int) message) list Lwt.t
(** [request_stateless b c position] requests the data of the stateless
    channel [c] from [position], as values and their indices. *)
