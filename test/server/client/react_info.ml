let of_down down =
  (* The event is sent as a channel, itself sent as its wrapped form. *)
  let (channel, _), _ =
    (Page.receive down : (_ Eliom.Comet_base.wrapped_channel * _) * _)
  in
  Comet_info.of_wrapped channel

let of_up up =
  let service, _ =
    (Page.receive up
     : ( unit
         , _
         , Eliom.Service.post
         , _
         , _
         , _
         , _
         , [`WithoutSuffix]
         , unit
         , [`One of _] Eliom.Parameter.param_name
         , _ )
         Eliom.Service.t
       * _)
  in
  Service_info.of_service service
