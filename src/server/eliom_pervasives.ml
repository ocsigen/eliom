
open Eliom_lib
open Eliom_pervasives_base

type ('a, 'b) server_function = ('a, 'b) server_function_service * Eliom_wrap.unwrapper

(* BB FIXME [argument] must not be sent marshalled, this may segfault the server.
   Better send the argument jsonified. *)
let server_function f : (_, _) server_function =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:Eliom_parameter.(string "marshalled_argument")
    (fun () marshalled_argument ->
       let argument =
         Marshal.from_string
           (Url.decode marshalled_argument) 0
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

