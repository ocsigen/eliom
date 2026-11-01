(** Services of the test servers of test.ml, which answer in plain text:
    - [/set?v=] and [/get] set and read a session reference,
    - [/count] is the number of volatile data sessions of the server. *)

val session_value : string Eliom.Reference.Volatile.eref
(** The session reference of [/set] and [/get]. *)

val start : unit -> unit
(** [start ()] registers the common services and starts the server. *)
