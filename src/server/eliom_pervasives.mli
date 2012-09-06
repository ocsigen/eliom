
open Eliom_pervasives_base

type ('a, 'b) server_function

val server_function : ('a -> 'b Lwt.t) -> ('a, 'b) server_function
