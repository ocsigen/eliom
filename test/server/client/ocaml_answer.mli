(** The answers of the services sending OCaml values
    ({!Eliom.Registration.Ocaml}), such as server functions. *)

val decode :
   Eliom_test_server.Browser.response
  -> [`Success of 'a | `Failure of string]
(** [decode r] is the value of the answer [r], or the code of the error of
    the handler, which the server logs. The type of the value is not
    checked.
    @raise Failure if [r] is not the answer of such a service. *)
