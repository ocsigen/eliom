(** Simulated browsers: HTTP clients of a test server, each with its own
    cookies. *)

type t
(** A browser. *)

type response = {status : int; headers : Cohttp.Header.t; body : string}
(** The responses of the server. *)

val create : Server_harness.t -> t
(** [create server] is a new browser for [server], with no cookie. *)

val get : ?headers:(string * string) list -> t -> string -> response Lwt.t
(** [get b url] sends a GET request for [url], a path with an optional query
    string, e.g. ["/a/b?x=1"]. The cookies of [b] matching the path are sent
    with [headers], and the cookies set by the response are kept, as described
    in {!Cookie_jar}. Redirections are not followed. *)

val post :
   ?headers:(string * string) list
  -> t
  -> string
  -> (string * string) list
  -> response Lwt.t
(** [post b url params] sends a POST request for [url], with [params] in the
    body, form-encoded. Cookies are handled as by {!get}. *)

val cookies : t -> (string * string) list
(** [cookies b] are the names and values of the cookies of [b] not expired,
    sorted by name. *)

val header : response -> string -> string option
(** [header r name] is the value of the header [name] of [r], if any. *)
