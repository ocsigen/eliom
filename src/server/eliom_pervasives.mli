(** This module is automatically open by {v eliomc v} and {v js_of_eliom v}. *)

open Eliom_pervasives_base


(** {2 Client values}

    See the {% <<a_manual chapter="clientserver-language"|manual>> %}. *)

(** Client values on the server are created by the syntax [{typ{ expr }}]
    in the server section (cf. {% <<a_manual chapter="clientserver-language"
    fragment="clientvalues"|the manual>> %}).  They are abstract, but
    become concrete once sent to the client. See also {% <<a_api
    subproject="client" text="the concrete representation on the client"
    |type Eliom_pervasives.client_value>> %}. *)
type 'a client_value = 'a Eliom_lib.client_value

(** {2 RPC / Server functions}

    See the {% <<a_manual chapter="clientserver-communication" fragment="rpc"|manual>> %}.*)

(** A value of type [('a, 'b) server_function] is created on the server from a
    function ['a -> 'b Lwt.t] and provides a given function on the client side.
    See also {% <<a_api subproject="client" text="the concrete client side representation"|
              type Eliom_pervasives.server_function>> %}. *)
type ('a, 'b) server_function

(** [server_function argument_type f] creates a value of type {%
    <<a_api | type Eliom_pervasives.server_function>> %}. This allows
    to call [f] from the client. The first argument [argument_type] is
    an instance of [Deriving_Json] for the type of the argument. It is
    used to safely encode and decode the argument sent to the server.

    The optional parameters correspond directly to the optional
    parameters of {% <<a_api|val Eliom_registration.Ocaml.register_coservice'>> %}.

    See also the {% <<a_manual chapter="clientserver-communication"
    fragment="rpc"|manual>> %}.
*)
(* BBB This is not in Eliom_service because it depends on Eliom_registration *)
val server_function :
  ?scope:[< Eliom_common.scope ] ->
  ?options:unit ->
  ?charset:string ->
  ?code:int ->
  ?content_type:string ->
  ?headers:Http_headers.t ->
  ?secure_session:bool ->
  ?name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Eliom_common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?error_handler:((string * exn) list -> 'b Lwt.t) ->
  'a Deriving_Json.t -> ('a -> 'b Lwt.t) -> ('a, 'b) server_function


(**/**)
type hook =
  {mutable a : 'a 'b 'c 'd.
     ?scope:([< Eliom_common.scope ] as 'd) ->
     ?options:unit ->
     ?charset:string ->
     ?code:int ->
     ?content_type:string ->
     ?headers:Http_headers.t ->
     ?secure_session:bool ->
     ?name:string ->
     ?csrf_safe:bool ->
     ?csrf_scope:([< Eliom_common.user_scope ] as 'c) ->
     ?csrf_secure:bool ->
     ?max_use:int ->
     ?timeout:float ->
     ?https:bool ->
     ?error_handler:((string * exn) list -> 'b Lwt.t) ->
     'a Deriving_Json.t -> ('a -> 'b Lwt.t) -> ('a, 'b) server_function}

val server_function_hook : hook

val mk_serv_fun :
  ('a, 'b) server_function_service -> Eliom_wrap.unwrapper ->
  ('a, 'b) server_function
