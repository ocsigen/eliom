exception Stream_too_small
exception Stream_error of string
exception String_too_large

type stream = private
    Finished of stream option
  | Cont of string * int * (unit -> stream Lwt.t)

val empty_stream : stream option -> stream
val new_stream : string -> (unit -> stream Lwt.t) -> stream
val string_of_stream : stream -> string Lwt.t
val enlarge_stream : stream -> stream Lwt.t
val stream_want : stream -> int -> stream Lwt.t
val current_buffer : stream -> string
val skip : stream -> int -> stream Lwt.t
val substream : string -> stream -> stream Lwt.t
