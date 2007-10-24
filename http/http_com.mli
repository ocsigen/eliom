
(**
Who can raise the exceptions:
R: receiver
r: receiver stream
S: sender
*)

(** The other side has cleanly closed the connection after a message *)
exception Connection_closed (* R *)

(** The connection has been unexpectedly broken *)
exception Lost_connection of exn (* RrS *)

(** No activity on the other side *)
exception Timeout (* RrS *)
exception Keepalive_timeout (* R *)

(** Connection killed *)
exception Aborted (* RrS *)

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

(*
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

module type SENDER =
  sig
    type t
(* [send] may also fail with Interrupted_stream exception *)
    val send :
      ?filter:('a option ->
               Http_frame.full_stream -> Http_frame.full_stream Lwt.t) ->
      slot ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
      mode:Http_frame.Http_header.http_mode ->
      ?proto:Http_frame.Http_header.proto ->
      ?headers:Http_headers.t ->
      ?contenttype:'a ->
      content:t -> head:bool -> sender_type -> unit Lwt.t
  end
module FHttp_sender :
  functor (C : Http_frame.HTTP_CONTENT) -> SENDER with type t = C.t

val abort : connection -> unit
