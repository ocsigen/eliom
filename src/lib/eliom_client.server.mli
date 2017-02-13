(** Returns whether the application is sent by a server or started on
    client side. If called on server side, always returns [false].
    Otherwise, it tests the presence of JS variables added automatically by
    Eliom when the page is sent by a server.
    Example:
    {[ if not (Eliom_client.is_client_app ())
 then Eliom_client.init_client_app ... ]}
*)
val is_client_app : unit -> bool


(** {2 RPC / Server functions}

    See the {% <<a_manual chapter="clientserver-communication" fragment="rpc"|manual>> %}.*)

(** A value of type [('a, 'b) server_function] is created on the server from a
    function ['a -> 'b Lwt.t] and provides a given function on the client side.
    See also {% <<a_api subproject="client" text="the concrete client side representation"|
              type Eliom_client.server_function>> %}. *)
type ('a, 'b) server_function

(** [server_function argument_type f] creates a value of type {%
    <<a_api | type Eliom_client.server_function>> %}. This allows
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
  ?headers:Ocsigen_header.t ->
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
