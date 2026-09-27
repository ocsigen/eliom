(** What a client needs to know about the events of {!Eliom.Eliom_react}, as
    sent to the client of the request. To be called by a service handler of
    the test server. *)

val of_down : 'a Eliom.Eliom_react.Down.t -> Comet_info.t
(** [of_down e] is the Comet channel carrying the occurrences of [e]. *)

val of_up : 'a Eliom.Eliom_react.Up.t -> Service_info.t
(** [of_up e] is the service triggering [e]. *)
