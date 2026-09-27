(* Volatile data sessions expire after 1 s and are collected every second. *)

let () =
  Eliom.State.set_default_global_volatile_data_state_timeout
    ~cookie_level:`Session ~override_configfile:true (Some 1.);
  Eliom.Config.set_data_session_gc_frequency (Some 1)

let () = Services.start ()
