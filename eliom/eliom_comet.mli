
(** The type of channels transporting values of type 'a. Values are marshalled
    before transmition, it is for the client to properly unmarshall it. The
    Eliom_client_comet module provides primitives that allow just that. *)
type 'a chan

module Channels :
(** A module with all the base primitive needed for server push.*)
sig

  val new_channel : unit -> 'a chan
  (** [new_channel ()] makes a fresh new channel immediatly usable. The id can
      be transmitted to a client in order to let him collect information passed
      on it. *)

  val write : 'a chan -> 'a -> unit
  (** [write c x] transmit the value [x] (in a marshelled form) onto the channel
      [c]. Any client collecting values from [c] can unmarshall [x]. *)

  val get_id : 'a chan -> 'a Eliom_common_comet.chan_id
  (** [get_id c] returns a unique identifier associated to [c]. The client can
      register to [c] using the returned identifier. *)

end

(** [wrap_channel sp c] wraps the channel [c] into the global data transmitted
    to the client with the application client-side code. The result is a value
    of type ['a chan_id] and it is the client responsability to register to the
    channel. Functions in Eliom_client_comet and Eliom_client_event can be of
    use in this matter. *)
val wrap_channel :
  sp:Eliom_sessions.server_params ->
  'a chan -> 'a Eliom_common_comet.chan_id Eliom_client_types.data_key
