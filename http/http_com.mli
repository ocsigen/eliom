exception Ocsigen_HTTP_parsing_error of string * string
exception Ocsigen_header_too_long
exception Ocsigen_Timeout
exception Ocsigen_KeepaliveTimeout
exception Connection_reset_by_peer
exception MustClose
exception Ocsigen_sending_error of exn

module Timeout :
  sig
    val start_timeout_killer : unit -> unit Lwt.t
  end
type s_http_mode = Answer | Query | Nofirstline
type receiver_type
val create_receiver : mode:s_http_mode -> Lwt_ssl.socket -> receiver_type
module type RECEIVER =
  sig
    type t
    val get_http_frame :
      unit Lwt.t ->
      receiver_type ->
      ?head:bool -> doing_keep_alive:bool -> unit ->
      t Http_frame.FHttp_frame.http_frame Lwt.t
  end
module FHttp_receiver :
  functor (C : Http_frame.HTTP_CONTENT) -> RECEIVER with type t = C.t
type sender_type
val create_sender :
  ?server_name:string ->
  mode:s_http_mode ->
  ?headers:(string * string) list ->
  ?proto:Http_frame.Http_header.proto -> Lwt_ssl.socket -> sender_type
module type SENDER =
  sig
    type t
    val really_write :
      ?chunked:bool ->
      Lwt_chan.out_channel ->
      (unit -> unit Lwt.t) -> Ocsistream.stream -> unit Lwt.t
    val send :
      ?filter:('a option ->
               int64 option * Http_frame.etag * Ocsistream.stream *
               (unit -> unit Lwt.t) ->
               (int64 option * Http_frame.etag * Ocsistream.stream *
                (unit -> unit Lwt.t))
               Lwt.t) ->
      unit Lwt.t ->
      clientproto:Http_frame.Http_header.proto ->
      ?etag:Http_frame.etag ->
      mode:Http_frame.Http_header.http_mode ->
      ?proto:Http_frame.Http_header.proto ->
      ?headers:(string * string) list ->
      ?contenttype:'a ->
      ?content:t -> head:bool -> sender_type -> unit Lwt.t
  end
module FHttp_sender :
  functor (C : Http_frame.HTTP_CONTENT) -> SENDER with type t = C.t
