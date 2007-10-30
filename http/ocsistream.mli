exception Interrupted of exn
exception Cancelled
exception Already_read
exception Finalized

(** Streams are a means to read data block by block *)

type 'a stream

(** A stream may be composed by several substreams.
   Thus a stream is either something that contains the current buffer and
   a function to retrieve the following data, 
   or a finished stream with possibly another stream following.
 *)
type 'a step =
  | Finished of 'a stream option
  | Cont of 'a * 'a stream

type 'a t

(** creates a new stream *)
val make : ?finalize:(unit -> unit Lwt.t) -> (unit -> 'a step Lwt.t) -> 'a t

(** call this function if you decide to start reading a stream.
    @raise [Already_read] if the stream has already been read. *)
val get : 'a t -> 'a stream

(** get the next step of a stream.
    Fails with [Interrupted e] if reading the thread failed with exception [e],
    and with [Cancelled] if the thread has been consumed. *)
val next : 'a stream -> 'a step Lwt.t


(** creates an empty step *)
val empty : (unit -> 'a step Lwt.t) option -> 'a step Lwt.t

(** creates a non empty step. *)
val cont : 'a -> (unit -> 'a step Lwt.t) -> 'a step Lwt.t


(** Add a finalizer function *)
val add_finalizer : 'a t -> (unit -> unit Lwt.t) -> unit

(** Finalize the stream *)
val finalize : 'a t -> unit Lwt.t

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



(*VVV à revoir : *)

(** returns a stream reading from a file.
   Do not forget to finalize the stream to close the file.
 *)
val of_file : string -> string t

(** returns a stream containing a string. *)
val of_string : string -> string t
