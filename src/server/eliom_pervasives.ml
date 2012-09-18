
open Eliom_lib
open Eliom_pervasives_base

type ('a, 'b) server_function = ('a, 'b) server_function_service * Eliom_wrap.unwrapper

let server_function argument_type f : (_, _) server_function =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:Eliom_parameter.(string "jsonified_argument")
    (fun () jsonified_argument ->
       let argument =
         Deriving_Json.from_string argument_type
           (Url.decode jsonified_argument)
       in
       lwt res =
         try_lwt
           lwt value = f argument in
           Lwt.return (`Success value)
         with exc ->
           Lwt.return (`Exception exc)
       in
       let marshalled_res = Url.encode (Marshal.to_string res []) in
       Lwt.return marshalled_res),
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)

