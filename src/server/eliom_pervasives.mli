
open Eliom_pervasives_base

(** {2 RPC / Server functions}
    Cf. the {% <<a_manual chapter="client-communication" fragment="rpc"|manual>> %}.*)

(** A value of type [('a, 'b) server_function] is created on the server from a
    function ['a -> 'b Lwt.t] and provides a given function on the client side.
    Cf. {% <<a_api subproject="client" text="the concrete client side representation"|
              type Eliom_pervasives.server_function>> %} *)
type ('a, 'b) server_function

(** [server_function argument_type f] creates a value of type {% <<a_api | type
    Eliom_pervasives.server_function>> %}. This allows to call [f] from the
    client. The first argument [argument_type] is an instance of [Deriving_Json]
    for the type of the argument, to make sending values to the server safe. *)
val server_function : 'a Deriving_Json.t -> ('a -> 'b Lwt.t) -> ('a, 'b) server_function
