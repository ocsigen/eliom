type t =
  { url : string
  ; post_params : (string * string) list
  ; idle_param : string
  ; request_param : string
  ; channel : string }

let of_wrapped wrapped =
  let Eliom.Comet_base.Comet_service (service, _), id =
    match wrapped with
    | Eliom.Comet_base.Stateful_channel (service, id)
    | Eliom.Comet_base.Stateless_channel (service, id, _) ->
        service, id
  in
  let path, get_params, _, post_params =
    Eliom.Eliom_uri.make_post_uri_components_ ~absolute_path:true ~service () ()
  in
  (* The parameters of coservices are prefixed. *)
  let _, (idle_param, request_param) =
    Eliom.Parameter.make_params_names (Eliom.Service.post_params_type service)
  in
  { url =
      Eliom.Eliom_uri.make_string_uri_from_components (path, get_params, None)
  ; post_params
  ; idle_param = Eliom.Parameter.string_of_param_name idle_param
  ; request_param = Eliom.Parameter.string_of_param_name request_param
  ; channel = Eliom.Comet_base.string_of_chan_id id }

let of_channel channel = of_wrapped (Eliom.Comet.Channel.get_wrapped channel)
let json = [%json: string * (string * string) list * string * string * string]

let to_string {url; post_params; idle_param; request_param; channel} =
  Deriving_Json.to_string json
    (url, post_params, idle_param, request_param, channel)

let of_string s =
  let url, post_params, idle_param, request_param, channel =
    Deriving_Json.from_string json s
  in
  {url; post_params; idle_param; request_param; channel}
