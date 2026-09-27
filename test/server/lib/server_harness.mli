(** Test servers run in a child process, in a directory of their own. *)

type t
(** A running test server. *)

val with_server : string -> (t -> 'a) -> 'a
(** [with_server exe f] runs the server program [exe] and returns [f s], where
    [s] is the running server, then stops the server.

    [exe] is started in a new temporary directory, given as its only argument,
    and must start Ocsigen Server with {!start}. The standard and error
    outputs of the server go to a log, printed if [f] raises an exception,
    which is raised again. The directory is removed if [f] returns.

    @raise Failure if the server does not start. *)

val socket : t -> string
(** [socket s] is the Unix-domain socket on which [s] listens. *)

val start : Ocsigen.Server.instruction list -> unit
(** [start instructions] starts Ocsigen Server in the directory given as
    argument of the program, listening on the socket and the command pipe
    that {!with_server} expects, with a single host running [instructions].
    It does not return. To be called by the server program. *)
