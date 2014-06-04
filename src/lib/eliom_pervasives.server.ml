
open Eliom_lib
open Eliom_pervasives_base

type 'a client_value = 'a Eliom_lib.client_value

type ('a, 'b) server_function =
  ('a, 'b) server_function_service * Eliom_wrap.unwrapper


type hook =
  {mutable a : 'a 'b 'c 'd.
     ?scope:([< Eliom_common.scope ] as 'd) ->
     ?options:unit ->
     ?charset:string ->
     ?code:int ->
     ?content_type:string ->
     ?headers:Http_headers.t ->
     ?secure_session:bool ->
     ?name:string ->
     ?csrf_safe:bool ->
     ?csrf_scope:([< Eliom_common.user_scope ] as 'c) ->
     ?csrf_secure:bool ->
     ?max_use:int ->
     ?timeout:float ->
     ?https:bool ->
     ?error_handler:((string * exn) list -> 'b Lwt.t) ->
     'a Deriving_Json.t -> ('a -> 'b Lwt.t) -> ('a, 'b) server_function}

let server_function_hook =
  {a = (fun ?scope ?options ?charset ?code ?content_type ?headers
         ?secure_session ?name
         ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https
         ?error_handler
         argument_type f -> failwith "server_function") }

let server_function =
  fun
    ?scope ?options ?charset ?code ?content_type ?headers ?secure_session ?name
    ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https ?error_handler
    argument_type f ->
  server_function_hook.a
    ?scope ?options ?charset ?code ?content_type ?headers ?secure_session ?name
    ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https ?error_handler
    argument_type f


let mk_serv_fun a b : ('a, 'b) server_function = (a, b)
