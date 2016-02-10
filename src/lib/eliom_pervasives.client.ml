
type 'a client_value = 'a Eliom_lib.client_value

type 'a fragment = 'a client_value

(* exception False = Eliom_lib.False *)

type ('a, +'b) server_function = 'a -> 'b Lwt.t


let server_function
    ?scope ?options ?charset ?code ?content_type ?headers ?secure_session ~name
    ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https ?error_handler
    argument_type () =
  let service =
    Eliom_service.Ocaml.post_coservice'
      ~name
      ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https
      ~post_params:Eliom_parameter.(ocaml "argument" argument_type)
      ()
  in
  fun a -> Eliom_client.call_ocaml_service ~absolute:true ~service () a


let () =
  Eliom_unwrap.register_unwrapper
    (Eliom_unwrap.id_of_int Eliom_common_base.server_function_unwrap_id_int)
    (fun (service, _) ->
      (* 2013-07-31 I make all RPC's absolute because otherwise
         it does not work with mobile apps.
         Is it a problem?
         -- Vincent *)
      Eliom_client.call_ocaml_service ~absolute:true ~service ())

let _force_link = ()
