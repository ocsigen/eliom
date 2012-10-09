
open Eliom_lib
open Eliom_pervasives_base

type 'a client_value = 'a Eliom_lib.client_value

(* exception False = Eliom_lib.False *)

type ('a, 'b) server_function = 'a -> 'b Lwt.t

type ('a, 'b) server_function_service =
  (unit, string,
   [ `Nonattached of [ `Post] Eliom_service.na_s ], [ `WithoutSuffix ],
   unit, [ `One of string ] Eliom_parameter.param_name,
   [ `Registrable ], string Eliom_parameter.caml)
  Eliom_service.service

let call_server_function : ('a, 'b) server_function_service -> 'a -> 'b Lwt.t =
  fun sfs argument ->
    let jsonified_argument = Url.encode (to_json argument) in
    lwt marshalled_res =
      Eliom_client.call_caml_service
        ~service:(sfs :> (_, _, Eliom_service.service_kind, _, _, _, _, _) Eliom_service.service)
        () jsonified_argument
    in
    let res = Marshal.from_string (Url.decode marshalled_res) 0 in
    match res with
      | `Success value -> Lwt.return value
      | `Exception exc -> Lwt.fail exc

let () =
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)
    (fun (sf, _) -> (fun x -> call_server_function sf x))

let _force_link = ()
