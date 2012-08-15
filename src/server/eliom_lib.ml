
include Ocsigen_lib
include Eliom_lib_base

type 'a client_value =
    'a Eliom_server.Client_value.t * Eliom_wrap.unwrapper

let create_client_value cv =
  cv, Eliom_wrap.create_unwrapper
        (Eliom_wrap.id_of_int
           Eliom_lib_base.client_value_unwrap_id_int)

let client_value_client_value = fst

let debug f = Printf.ksprintf (fun s -> Printf.eprintf "%s\n%!" s) f

let to_json ?typ v =
  match typ with
    | Some typ -> Deriving_Json.to_string typ v
    | None -> assert false (* implemented only client side *)

let of_json ?typ s =
  match typ with
    | Some typ -> Deriving_Json.from_string typ s
    | None -> assert false (* implemented only client side *)


type file_info = Ocsigen_extensions.file_info

