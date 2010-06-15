(* The Comet server extension only provides untyped channels (channels that
 * ransport string content. Here we add type information and we keep only
 * usefull functions. *)

module Ecc = Eliom_common_comet

(* Type of typed channels *)
type 'a chan = Comet.Channels.chan

(* A module that provides primitive for server-side channel handling. The only
 * needed operations are : creating, writing, getting id. *)
module Channels :
sig

  val new_channel : unit -> 'a chan

  (* /!\ Uses Marshaling to pass information /!\ *)
  val write  : 'a chan -> 'a -> unit

  val get_id : 'a chan -> 'a Ecc.chan_id

end = struct

  let new_channel () = Comet.Channels.new_channel ()
  let encode_data r = Ocsigen_lib.encode ~plus:false (Marshal.to_string r [])
  let write c x =
    Comet.Channels.write c
      (Ocsigen_lib.encode ~plus:false (Marshal.to_string x []))
  let get_id c = Comet.Channels.get_id c

end


(* Here is a wrap for channels. This can be used to transmit it to a client. *)
let wrap_channel ~sp (c : 'a chan)
      : 'a Ecc.chan_id Eliom_client_types.data_key =
  Eliom_client.wrap ~sp (Channels.get_id c)


(*TODO: high level functions to handle channels (buffered...)*)
