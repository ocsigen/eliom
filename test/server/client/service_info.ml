type t = {url : string; post_params : (string * string) list; param : string}

let of_service service =
  let path, get_params, _, post_params =
    Eliom.Eliom_uri.make_post_uri_components_ ~absolute_path:true ~service () ()
  in
  let _, param =
    Eliom.Parameter.make_params_names (Eliom.Service.post_params_type service)
  in
  { url =
      Eliom.Eliom_uri.make_string_uri_from_components (path, get_params, None)
  ; post_params
  ; param = Eliom.Parameter.string_of_param_name param }

let post tab {url; post_params; param} v =
  Tab.post tab url (post_params @ [param, v])

let json = [%json: string * (string * string) list * string]

let to_string {url; post_params; param} =
  Deriving_Json.to_string json (url, post_params, param)

let of_string s =
  let url, post_params, param = Deriving_Json.from_string json s in
  {url; post_params; param}
