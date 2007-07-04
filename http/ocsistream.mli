exception Stream_too_small
exception Stream_error of string
exception String_too_large

(** Streams are a means to read data block by block *)

(** A stream may be composed by several substreams.
   Thus a stream is either something that contains the current buffer and
   a function to retrieve the following data, 
   or a finished stream with possibly another stream following.
   The integer is the size of the current buffer.
 *)
type stream = private
  | Finished of stream option
  | Cont of string * int * (unit -> stream Lwt.t)

(** creates an empty stream *)
val empty_stream : stream option -> stream

(** creates a new (non empty) stream. 
   [?len] is the length of the string, if you know it (not checked).
 *)
val new_stream : ?len:int -> string -> (unit -> stream Lwt.t) -> stream

(** true if the stream is finished *)
val is_finished : stream -> bool

(** Creates a string from a stream *)
val string_of_stream : stream -> string Lwt.t

(** Read more data in the buffer *)
val enlarge_stream : stream -> stream Lwt.t

(** [stream_want s len =] Returns a stream with at most len
   bytes in the buffer if possible *)
val stream_want : stream -> int -> stream Lwt.t

(** Returns the value of the current buffer *)
val current_buffer : stream -> string

(** Skips data *)
val skip : stream -> int -> stream Lwt.t

(** Cut the stream at the position given by a string delimiter *)
val substream : string -> stream -> stream Lwt.t

(** read the stream until the end, without decoding *)
val consume : stream -> unit Lwt.t
