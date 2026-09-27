type t = {channel : Comet_info.t; write : Service_info.t}

let of_bus bus =
  let (channel, Eliom.Comet_base.Bus_send_service service), _unwrapper =
    (Page.receive bus : (_, _) Eliom.Comet_base.wrapped_bus * _)
  in
  { channel = Comet_info.of_wrapped channel
  ; write = Service_info.of_service service }

let json = [%json: string * string]

let to_string {channel; write} =
  Deriving_Json.to_string json
    (Comet_info.to_string channel, Service_info.to_string write)

let of_string s =
  let channel, write = Deriving_Json.from_string json s in
  {channel = Comet_info.of_string channel; write = Service_info.of_string write}
