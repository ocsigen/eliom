
open Eliom_lib
open Eliom_pervasives_base

type 'a client_value = 'a Eliom_lib.client_value

(* exception False = Eliom_lib.False *)

type ('a, 'b) server_function = 'a -> 'b Lwt.t

let call_server_function : ('a, 'b) server_function_service -> 'a -> 'b Lwt.t =
  fun sfs argument ->
    match_lwt
      Eliom_client.call_caml_service
        ~service:(sfs :> (_, _, Eliom_service.service_kind, _, _, _, _, _) Eliom_service.service)
        () argument
    with
      | `Success res -> Lwt.return res
      | `Failure str -> Lwt.fail (Exception_on_server str)

let () =
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)
    (fun (sfs, _) -> (fun x -> call_server_function sfs x))

let _force_link = ()
