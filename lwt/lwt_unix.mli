(** Module [Lwt_unix]: thread-compatible system calls *)
type descr = Plain of Unix.file_descr
           | Encrypted of Unix.file_descr * Ssl.socket
   
val sleep : float -> unit Lwt.t
      (** [sleep d] is a threads which remain suspended for [d] seconds
         (letting other threads run) and then terminates. *)
val yield : unit -> unit Lwt.t
      (** [yield ()] is a threads which suspends itself (letting other
         thread run) and then resumes as soon as possible and
         terminates. *)

val run : 'a Lwt.t -> 'a
      (** [run t] lets the thread [t] run until it terminates.  It
         evaluates to the return value of [t], or raise the exception
         associated to [t] if [t] fails.

         You should avoid using [run] inside threads:
         - The calling threads will not resume before [run]
           returns.
         - Successive invocations of [run] are serialized: an
           invocation of [run] will not terminate before all
           subsequent invocations are terminated. *)

(****)

(** These functions behave as their [Unix] counterparts, but let other
   threads run while waiting for the completion of the system call.

   PITFALL
   If you want to read or write from stdin, stdout or stderr using
   this library, you must first turn them into non-blocking mode
   using [Unix.set_nonblock]. *)

val read : descr -> string -> int -> int -> int Lwt.t
val write : descr -> string -> int -> int -> int Lwt.t
val pipe : unit -> (descr * descr) Lwt.t
val socket :
  Unix.socket_domain -> Unix.socket_type -> int -> Unix.file_descr Lwt.t
val socketpair :
  Unix.socket_domain -> Unix.socket_type -> int ->
  (Unix.file_descr * Unix.file_descr) Lwt.t
val shutdown : descr -> Unix.shutdown_command -> unit
val accept : descr -> (descr * Unix.sockaddr) Lwt.t
val connect : descr -> Unix.sockaddr -> unit Lwt.t

val wait : unit -> (int * Unix.process_status) Lwt.t
val waitpid : Unix.wait_flag list -> int -> (int * Unix.process_status) Lwt.t

val system : string -> Unix.process_status Lwt.t

type lwt_in_channel
type lwt_out_channel

val input_value : lwt_in_channel -> 'a Lwt.t
val input_char : lwt_in_channel -> char Lwt.t
val input_line : lwt_in_channel -> string Lwt.t
val input_binary_int : lwt_in_channel -> int Lwt.t
val input : lwt_in_channel -> string -> int -> int -> int Lwt.t
val really_input : lwt_in_channel -> string -> int -> int -> unit Lwt.t

val output_binary_int : lwt_out_channel -> int -> unit Lwt.t
val output_value : lwt_out_channel -> 'a -> unit Lwt.t
val output_string : lwt_out_channel -> string -> unit Lwt.t
val output : lwt_out_channel -> string -> int -> int -> unit Lwt.t
val flush : lwt_out_channel -> unit Lwt.t

val open_process_in: string -> lwt_in_channel Lwt.t
val open_process_out: string -> lwt_out_channel Lwt.t
val open_process: string -> (lwt_in_channel * lwt_out_channel) Lwt.t
val open_process_full:
  string -> string array ->
  (lwt_in_channel * lwt_out_channel * lwt_in_channel) Lwt.t
val close_process_in: lwt_in_channel -> Unix.process_status Lwt.t
val close_process_out: lwt_out_channel -> Unix.process_status Lwt.t
val close_process:
  lwt_in_channel * lwt_out_channel -> Unix.process_status Lwt.t
val close_process_full:
  lwt_in_channel * lwt_out_channel * lwt_in_channel ->
  Unix.process_status Lwt.t

val in_channel_of_descr : descr -> lwt_in_channel
val out_channel_of_descr : descr -> lwt_out_channel
val in_channel_of_unixdescr : Unix.file_descr -> lwt_in_channel
val out_channel_of_unixdescr : Unix.file_descr -> lwt_out_channel
 
val set_close_on_exec : descr -> unit

val fd_of_descr : descr -> Unix.file_descr

(**/**)
(* monitoring *)
val inputs_length : unit -> int
val outputs_length : unit -> int
val wait_children_length : unit -> int
val get_new_sleeps : unit -> int
val sleep_queue_size : unit -> int
