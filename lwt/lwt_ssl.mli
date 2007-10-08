
type socket

val ssl_accept : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val ssl_connect : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val plain : Lwt_unix.file_descr -> socket

val read : socket -> string -> int -> int -> int Lwt.t
val write : socket -> string -> int -> int -> int Lwt.t

val shutdown : socket -> Unix.shutdown_command -> unit
val close : socket -> unit

val out_channel_of_descr : socket -> Lwt_chan.out_channel
val in_channel_of_descr : socket -> Lwt_chan.in_channel

val ssl_shutdown : socket -> unit Lwt.t
