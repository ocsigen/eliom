
open Eliom_pervasives_base

(** This module is automatically open by {v eliomc v} and {v js_of_eliom v}. *)

(** {2 Client values}

    See the {% <<a_manual chapter="eliomc"|manual>> %}. *)

(** Client values on the server are created by the syntax [{typ{ expr }}]
    in the server section (cf. {% <<a_manual chapter="eliomc"
    fragment="clientvalues"|the manual>> %}).  They are abstract, but
    become concrete once sent to the client. See also {% <<a_api
    subproject="client" text="the concrete representation on the
    client"|type Eliom_pervasives.client_value>> %}. *)
type 'a client_value = 'a Eliom_lib.client_value

(** {2 RPC / Server functions}

    See the {% <<a_manual chapter="client-communication" fragment="rpc"|manual>> %}.*)

(** A value of type [('a, 'b) server_function] is created on the server from a
    function ['a -> 'b Lwt.t] and provides a given function on the client side.
    See also {% <<a_api subproject="client" text="the concrete client side representation"|
              type Eliom_pervasives.server_function>> %}. *)
type ('a, 'b) server_function

(** [server_function argument_type f] creates a value of type {% <<a_api | type
    Eliom_pervasives.server_function>> %}. This allows to call [f] from the
    client. The first argument [argument_type] is an instance of [Deriving_Json]
    for the type of the argument, to make sending values to the server safe. *)
val server_function : 'a Deriving_Json.t -> ('a -> 'b Lwt.t) -> ('a, 'b) server_function
