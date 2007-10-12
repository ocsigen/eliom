type etag = string

type full_stream =
  int64 option * etag * Ocsistream.stream * (unit -> unit Lwt.t)
(** The type of streams to be send by the server.
   The [int64 option] is the content-length.
   [None] means Transfer-encoding: chunked
   The last function is the termination function
   (for ex closing a file if needed),
   that must be called when the stream is not needed any more.
   Your new termination function should probably call the former one. *)

module type HTTP_CONTENT =
  sig
    type t
    val content_of_stream : Ocsistream.stream -> t Lwt.t
    val stream_of_content :
      t -> full_stream Lwt.t
    val get_etag : t -> etag
  end
module Http_header :
  sig
    type http_method =
        GET | POST | HEAD | PUT | DELETE | TRACE
      | OPTIONS | CONNECT | LINK | UNLINK | PATCH
    type http_mode =
        Query of (http_method * string)
      | Answer of int
      | Nofirstline
    type proto = HTTP10 | HTTP11
    type http_header = {
      mode : http_mode;
      proto : proto;
      headers : (string * string) list;
    }
    val get_firstline : http_header -> http_mode
    val get_headers : http_header -> (string * string) list
    val get_headers_value : http_header -> string -> string
    val get_proto : http_header -> proto
    val add_headers : http_header -> string -> string -> http_header
  end
module Http_error :
  sig
    exception Http_exception of int option * string list
    val expl_of_code : int -> string
    val display_list : string list -> unit
    val string_of_list : string list -> string
    val display_http_exception : exn -> unit
    val string_of_http_exception : exn -> string
  end
module FHttp_frame :
  sig
    type 'a frame_content = 'a option
    type 'a http_frame = {
      header : Http_header.http_header;
      content : 'a frame_content;
      waiter_thread : unit Lwt.t;
    }
  end
