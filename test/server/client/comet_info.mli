(** What a client needs to know about a Comet channel to request its data:
    computed by the test server, and sent to the test in JSON. *)

type t =
  { url : string  (** The URL of the Comet service of the channel *)
  ; post_params : (string * string) list
    (** The hidden POST parameters of the service *)
  ; idle_param : string  (** The name of the POST parameter [idle] *)
  ; request_param : string  (** The name of the POST parameter of the request *)
  ; channel : string  (** The identifier of the channel *) }

val of_channel : 'a Eliom.Comet.Channel.t -> t
(** [of_channel c] is the information about [c]. To be called by a service
    handler of the test server. *)

val of_wrapped : 'a Eliom.Comet_base.wrapped_channel -> t
(** [of_wrapped c] is the information about the channel [c] as sent to the
    client. *)

val to_string : t -> string
val of_string : string -> t
