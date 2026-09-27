(* The test server of test.ml: server functions, whose services are sent to
   the test as the client-side program gets them. *)

open Eliom
module P = Parameter
module Info = Eliom_test_client.Service_info

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f

(* [info path f] registers the service /path, whose answer is the service of
   the server function [f] *)
let info path f =
  string (get [path] P.unit) (fun () () ->
    text (Info.to_string (Info.of_server_function f)))

let () =
  info "incr"
    (Client.server_function [%json: int] (fun i ->
       if i < 0 then failwith "negative" else Lwt.return (i + 1)))

let () =
  info "with_error_handler"
    (Client.server_function
       ~error_handler:(fun _ -> Lwt.return (-1))
       [%json: int] Lwt.return)

let () = info "once" (Client.server_function ~max_use:1 [%json: int] Lwt.return)

(* A server function of the session, created by a request *)
let () =
  string (get ["of_the_session"] P.unit) (fun () () ->
    let f =
      Client.server_function ~scope:Common.default_session_scope [%json: string]
        (fun s -> Lwt.return ("of the session " ^ s))
    in
    text (Info.to_string (Info.of_server_function f)))

let () = Eliom_test_server.Server_harness.start [App.run ()]
