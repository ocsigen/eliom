
open Eliom_lib
open Eliom_pervasives_base

type 'a client_value = 'a Eliom_lib.client_value

type ('a, 'b) server_function = ('a, 'b) server_function_service * Eliom_wrap.unwrapper

let server_function
    ?scope ?options ?charset ?code ?content_type ?headers ?secure_session ?name
    ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https ?error_handler
    argument_type f : (_, _) server_function =
  let error_handler =
    Option.map
      (fun f errors ->
        Lwt.map
          (fun res -> `Success res)
          (f errors))
      error_handler
  in
  Eliom_registration.Ocaml.register_post_coservice'
    ?scope ?options ?charset ?code ?content_type ?headers ?secure_session ?name
    ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https ?error_handler
    ~post_params:Eliom_parameter.(caml "argument" argument_type)
    (fun () argument ->
      try_lwt
        lwt value = f argument in
        Lwt.return (`Success value)
      with exc ->
        Lwt.return (`Exception exc)),
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)
