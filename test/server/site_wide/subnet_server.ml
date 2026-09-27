(* At most two volatile data sessions without group per subnet. *)

let () =
  Eliom.State.set_default_max_volatile_data_sessions_per_subnet
    ~override_configfile:true 2

let () = Services.start ()
