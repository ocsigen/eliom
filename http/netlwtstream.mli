(* Heavily inspired from OcamlNet: netchannels.mli, netstream.mli *)

class type in_descr_channel = 
object 
   method pos_in : int64
   method close_in : unit -> unit
   method input : string -> int -> int -> int Lwt.t
   method really_input : string -> int -> int -> unit Lwt.t
   method input_char : unit -> char Lwt.t
   method input_line : unit -> string Lwt.t
   method input_byte : unit -> int Lwt.t
end

class type in_descr_stream =
object
  inherit in_descr_channel

  method block_size : int
  method window : Netbuffer.t
  method want : int -> unit Lwt.t
  method want_another_block : unit -> unit Lwt.t
  method window_length : int
  method window_at_eof : bool
  method skip : int -> unit Lwt.t
end

class input_descr : 
	Lwt_unix.descr ->
	  in_descr_channel

class input_stream : 
	?init:string ->
        ?len:int64 -> 
	?block_size:int -> 
	in_descr_channel -> 
	  in_descr_stream
	  
class sub_stream :
        ?len:int64 ->             (* default: no maximum length *)
	?delimiter:string ->    (* default: no delimiter *)
	in_descr_stream ->
	  in_descr_stream 
