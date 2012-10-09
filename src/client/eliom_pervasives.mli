
open Eliom_pervasives_base

(** This module is automatically open by {v eliomc v} and {v js_of_eliom v}. *)

(** An ['a] client value on the client is just an ['a].
    See also {% <<a_api subproject="server" text="on the server" |
    Eliom_lib.client_value >> %}. *)
type 'a client_value = 'a Eliom_lib.client_value

(* Re-export Eliom_lib.False here, when
   cf. http://caml.inria.fr/mantis/view.php?id=5778 is fixed *)
(* exception False *)

(** {2 RPC / Server functions}
    Cf. the {% <<a_manual chapter="client-communication" fragment="rpc"|manual>> %}.*)

(** A [('a, 'b) server_function] provides transparently access to a
    server side function which has been created by {% <<a_api
    subproject="server"|Eliom_pervasives.server_function>> %}.  Cf. {%
    <<a_api subproject="server" text="the opaque server side
    representation"| type Eliom_pervasives.server_function>> %} *)
type ('a, 'b) server_function = 'a -> 'b Lwt.t

(**/**)

val _force_link : unit
