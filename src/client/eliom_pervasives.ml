
open Eliom_lib
open Eliom_pervasives_base

type 'a client_value = 'a Eliom_lib.client_value

(* exception False = Eliom_lib.False *)

type ('a, 'b) server_function = 'a -> 'b Lwt.t

let () =
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)
    (fun (service, _) ->
      Eliom_client.call_caml_service ~service ())

let _force_link = ()
