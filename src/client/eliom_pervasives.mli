
open Eliom_pervasives_base

(** This module is automatically open by {v eliomc v} and {v js_of_eliom v}. *)

(** {2 Client values}

    See the {% <<a_manual chapter="clientserver-language"|manual>> %}. *)

(** An ['a] client value on the client is just an ['a].
    See also {% <<a_api subproject="server" text="the abstract representation on the server" |
    type Eliom_pervasives.client_value >> %}. *)
type 'a client_value = 'a Eliom_lib.client_value

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
type ('a, 'b) server_function = 'a -> 'b Lwt.t

(**/**)

val _force_link : unit
