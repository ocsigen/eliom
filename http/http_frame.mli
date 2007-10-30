type etag = string

type full_stream =
  int64 option * etag * string Ocsistream.t
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
    val content_of_stream : string Ocsistream.t -> t Lwt.t
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
      headers : Http_headers.t;
    }
    val get_firstline : http_header -> http_mode
    val get_headers : http_header -> Http_headers.t
    val get_headers_value : http_header -> Http_headers.name -> string
    val get_headers_values : http_header -> Http_headers.name -> string list
    val get_proto : http_header -> proto
    val add_headers : http_header -> Http_headers.name -> string -> http_header
  end
module Http_error :
  sig
    exception Http_exception of int * string option
    val expl_of_code : int -> string
    val display_http_exception : exn -> unit
    val string_of_http_exception : exn -> string
  end
type t =
  { header : Http_header.http_header;
    content : string Ocsistream.t option }
