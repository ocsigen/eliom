exception Interrupted of exn
exception Cancelled
exception Already_failed
exception Already_read

(** Streams are a means to read data block by block *)

(** A stream may be composed by several substreams.
   Thus a stream is either something that contains the current buffer and
   a function to retrieve the following data, 
   or a finished stream with possibly another stream following.
   The integer is the size of the current buffer.
 *)
type 'a stream

type 'a step =
  | Finished of 'a stream option
  | Cont of 'a * 'a stream

type 'a t

val make : (unit -> 'a step Lwt.t) -> 'a t

val get : 'a t -> 'a stream

val next : 'a stream -> 'a step Lwt.t


(** creates an empty stream *)
val empty : (unit -> 'a step Lwt.t) option -> 'a step Lwt.t

(** creates a non empty stream. *)
val cont : 'a -> (unit -> 'a step Lwt.t) -> 'a step Lwt.t


(** read the stream until the end, without decoding *)
val consume : 'a t -> unit Lwt.t


exception Stream_too_small
exception Stream_error of string
exception String_too_large

(** Creates a string from a stream *)
val string_of_stream : string stream -> string Lwt.t

(** Read more data in the buffer *)
val enlarge_stream : string step -> string step Lwt.t

(** [stream_want s len =] Returns a stream with at most len
   bytes in the buffer if possible *)
val stream_want : string step -> int -> string step Lwt.t

(** Returns the value of the current buffer *)
val current_buffer : string step -> string

(** Skips data *)
val skip : string step -> int -> string step Lwt.t

(** Cut the stream at the position given by a string delimiter *)
val substream : string -> string step -> string step Lwt.t
