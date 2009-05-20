(** Reload the configuration of the server.
    The optional parameter [?file] may be use to read the configuration
    from another file.
*)
val reload: ?file:string -> unit -> unit

(** Start the server (does not return) *)
val start_server: unit -> unit
