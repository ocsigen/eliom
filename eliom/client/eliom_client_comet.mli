
module Engine :
sig
  val start : unit -> unit
  val stop : unit -> unit
  val running : bool React.S.t
end

module Registration :
sig
  val register : 'a Eliom_common_comet.chan_id -> ('a -> unit Lwt.t) -> unit
  val unregister : 'a Eliom_common_comet.chan_id -> unit
end

val unwrap_channel :
  'a Eliom_common_comet.chan_id Eliom_client_types.data_key
  -> 'a Eliom_common_comet.chan_id
