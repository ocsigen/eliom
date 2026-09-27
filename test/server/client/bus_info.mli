(** What a client needs to know about a bus: the Comet channel on which it
    receives the values of the bus, and the service writing on the bus.
    Computed by the test server, sent to the test in JSON. *)

type t = {channel : Comet_info.t; write : Service_info.t}

val of_bus : ('a, 'b) Eliom.Bus.t -> t
(** [of_bus b] is the information about [b], as sent to the client of the
    request. To be called by a service handler of the test server. *)

val to_string : t -> string
val of_string : string -> t
