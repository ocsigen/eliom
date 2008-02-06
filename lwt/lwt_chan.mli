(** Module [Lwt_chan]: cooperative, [Pervasives]-like, I/O functions *)

(** {2 Cooperative input channels} *)
type in_channel

val make_in_channel : (string -> int -> int -> int Lwt.t) -> in_channel
(** [make_in_channel read] creates an input channel from the [read]
    function. [read s ofs len] should (cooperatively) read [len] bytes from
    the source, and put them in [s], at offset [ofs], and return the number
    of bytes effectively read. *)

val input_line : in_channel -> string Lwt.t
val input_value : in_channel -> 'a Lwt.t
val input : in_channel -> string -> int -> int -> int Lwt.t
val really_input : in_channel -> string -> int -> int -> unit Lwt.t
val input_char : in_channel -> char Lwt.t

(** {2 Cooperative output channels} *)

type out_channel

val make_out_channel : (string -> int -> int -> int Lwt.t) -> out_channel
(** [make_out_channel write] creates an output channel from the [write]
    function. [write s ofs len] should (cooperatively) write [len] bytes from
    [s], starting at offset [ofs], to the backend, and return the number of
    bytes effectively written. *)

val output : out_channel -> string -> int -> int -> unit Lwt.t
val flush : out_channel -> unit Lwt.t
val output_string : out_channel -> string -> unit Lwt.t
val output_value : out_channel -> 'a -> unit Lwt.t

