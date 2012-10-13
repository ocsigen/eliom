
open Eliom_lib
open Eliom_pervasives_base

type 'a client_value = 'a Eliom_lib.client_value

type ('a, 'b) server_function = ('a, 'b) server_function_service * Eliom_wrap.unwrapper

let server_function argument_type f : (_, _) server_function =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:Eliom_parameter.(caml "argument" argument_type)
    (fun () argument ->
      try_lwt
        lwt value = f argument in
        Lwt.return (`Success value)
      with exc ->
        Lwt.return (`Exception exc)),
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)
