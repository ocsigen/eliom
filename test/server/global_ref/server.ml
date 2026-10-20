(* The test server of test.ml: services that set, read and unset a
   persistent reference of global scope created at the toplevel of the
   module [Toplevel_ref], and answer in plain text. *)

open Eliom
module P = Parameter
open Lwt.Syntax

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f

let () =
  string
    (get ["set"] P.(string "v"))
    (fun v () ->
       let* () = Reference.set Toplevel_ref.value v in
       text "set")

let () =
  string (get ["get"] P.unit) (fun () () ->
    let* v = Reference.get Toplevel_ref.value in
    text v)

let () =
  string (get ["unset"] P.unit) (fun () () ->
    let* () = Reference.unset Toplevel_ref.value in
    text "unset")

let () = Eliom_test_server.Server_harness.start [App.run ()]
