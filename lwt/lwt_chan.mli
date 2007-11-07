type in_channel
type out_channel

val make_in_channel : (string -> int -> int -> int Lwt.t) -> in_channel
val input_line : in_channel -> string Lwt.t
val input_value : in_channel -> 'a Lwt.t
val input : in_channel -> string -> int -> int -> int Lwt.t
val really_input : in_channel -> string -> int -> int -> unit Lwt.t
val input_char : in_channel -> char Lwt.t

val make_out_channel : (string -> int -> int -> int Lwt.t) -> out_channel
val output : out_channel -> string -> int -> int -> unit Lwt.t
val flush : out_channel -> unit Lwt.t
val output_string : out_channel -> string -> unit Lwt.t
val output_value : out_channel -> 'a -> unit Lwt.t

