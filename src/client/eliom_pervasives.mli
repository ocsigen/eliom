
open Eliom_pervasives_base

type ('a, 'b) server_function = 'a -> 'b Lwt.t

(**/**)

val _force_link : unit
