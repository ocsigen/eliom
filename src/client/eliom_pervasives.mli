
open Eliom_pervasives_base

(** {2 RPC / Server functions}
    Cf. the {% <<a_manual chapter="client-communication" fragment="rpc"|manual>> %}.*)

(** A [('a, 'b) server_function] provides transparently access to a server side function which
    has been created by {% <<a_api subproject="server"|Eliom_pervasives.server_function>> %}.
    Cf. {% <<a_api subproject="server" text="the opaque server side representation"|
              type Eliom_pervasives.server_function>> %} *)
type ('a, 'b) server_function = 'a -> 'b Lwt.t

(**/**)

val _force_link : unit
