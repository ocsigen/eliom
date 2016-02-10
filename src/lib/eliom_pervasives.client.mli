(* ocamldoc/camlp4 hack ? : need an open first ? *)
open Eliom_pervasives_base

(** This module is automatically open by {v eliomc v} and {v js_of_eliom v}. *)

(** {2 Client values}

    See the {% <<a_manual chapter="clientserver-language"|manual>> %}. *)

(** An ['a] client value on the client is just an ['a].
    See also {% <<a_api subproject="server" text="the abstract representation on the server" |
    type Eliom_pervasives.client_value >> %}. *)
type 'a client_value = 'a Eliom_lib.client_value

(** An alias for {!client_value}. *)
type 'a fragment = 'a client_value

(* Re-export Eliom_lib.False here, when
   cf. http://caml.inria.fr/mantis/view.php?id=5778 is fixed *)
(* exception False *)

(** {2 RPC / Server functions}

    See the {% <<a_manual chapter="clientserver-communication" fragment="rpc"|manual>> %}.*)

(** A [('a, 'b) server_function] provides transparently access to a
    server side function which has been created by {% <<a_api
    subproject="server"|Eliom_pervasives.server_function>> %}.

    See also {% <<a_api subproject="server" text="the opaque server
    side representation"| type Eliom_pervasives.server_function>> %}.

    The handling of exception on the server corresponds to that of
    <<a_api subproject="client"|val Eliom_client.call_ocaml_service>>.
*)
type ('a, +'b) server_function = 'a -> 'b Lwt.t

(** [server_function argument_type f] creates a value of type {%
    <<a_api | type Eliom_pervasives.server_function>> %}. This allows
    to call [f] from the client. The first argument [argument_type] is
    an instance of [Deriving_Json] for the type of the argument. It is
    used to safely encode and decode the argument sent to the server.

    The optional parameters correspond directly to the optional
    parameters of {% <<a_api|val Eliom_registration.Ocaml.register_coservice'>> %}.

    See also the {% <<a_manual chapter="clientserver-communication"
    fragment="rpc"|manual>> %}.

    Defining server functions in shared or client sections is possible only
    if you give them a name.
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
  name:string ->
  ?csrf_safe:bool ->
  ?csrf_scope:[< Eliom_common.user_scope ] ->
  ?csrf_secure:bool ->
  ?max_use:int ->
  ?timeout:float ->
  ?https:bool ->
  ?error_handler:((string * exn) list -> 'b Lwt.t) ->
  'a Deriving_Json.t -> unit -> ('a, 'b) server_function


(**/**)
val _force_link : unit
