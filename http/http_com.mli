
(**
Who can raise the exceptions:
R: receiver
r: receiver stream
S: sender
*)

(** The other side has cleanly closed the connection after a message *)
exception Connection_closed (** R *)

(** The connection has been unexpectedly broken *)
exception Lost_connection of exn (** RrS *)

(** No activity on the other side *)
exception Timeout (** RrS *)
exception Keepalive_timeout (** R *)

(** Connection killed *)
exception Aborted (** RrS *)

type mode = Answer | Query | Nofirstline
type connection
val create_receiver : mode -> Lwt_ssl.socket -> connection
val lock_receiver : connection -> unit Lwt.t
val unlock_receiver : connection -> unit
val get_http_frame : ?head:bool -> connection -> Http_frame.t Lwt.t

(****)

type slot

val start_processing : connection -> (slot -> unit Lwt.t) -> unit
val wait_all_senders : connection -> unit Lwt.t

(****)

(**
This function may return any I/O error from the channel, or a
interrupted stream exception.
*)
val write_stream :
  ?chunked:bool -> Lwt_chan.out_channel -> string Ocsistream.t -> unit Lwt.t

(****)

type sender_type

val create_sender :
  ?server_name:string -> ?headers:Http_headers.t ->
  ?proto:Http_frame.Http_header.proto -> unit -> sender_type

(** Sender with only the server name, and HTTP/1.1 *)
val default_sender : sender_type



(** send an HTTP message. 
    [send] may also fail with [Interrupted_stream] exception if the input
    stream is interrupted.
 *)
val send :
    slot ->
    clientproto:Http_frame.Http_header.proto ->
    ?mode:Http_frame.Http_header.http_mode ->
    ?proto:Http_frame.Http_header.proto ->
    keep_alive:bool ->
    head:bool ->
    sender:sender_type ->
    Http_frame.result -> 
    unit Lwt.t

val abort : connection -> unit


