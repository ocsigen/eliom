(* Module for event unwrapping *)


let unwrap_down_event
      (c : 'a Eliom_common_comet.chan_id Eliom_client_types.data_key)
      : 'a React.E.t
  =
  let chan : string = Eliom_client_comet.unwrap_channel c in
  let (e, push) = React.E.create () in
  Eliom_client_comet.Registration.register
    chan
    (fun s -> push s ; Lwt.return ()) ;
  e

