(* Tests of a persistent reference of global scope created at the toplevel
   of a module of the server, before the location of the database is set. *)

open Eliom_test_server
open Lwt.Syntax

(* [text b url] is the body of the response to [url] for [b], checked to be a
   success. *)
let text b url =
  let+ r = Browser.get b url in
  if r.status <> 200 then Alcotest.failf "%s: status %d" url r.status;
  r.body

let check msg expected b url =
  let+ body = text b url in
  Alcotest.(check string) msg expected body

let case name f = Alcotest.test_case name `Quick (fun () -> Lwt_main.run (f ()))

let tests server =
  ( "global persistent reference"
  , [ case "created at the toplevel" (fun () ->
        let a = Browser.create server and b = Browser.create server in
        let* () = check "default value" "" a "/get" in
        let* _ = text a "/set?v=a" in
        let* () = check "other browser" "a" b "/get" in
        Server_harness.restart server;
        let* () = check "kept after a restart" "a" a "/get" in
        let* _ = text a "/unset" in
        check "unset" "" b "/get") ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-global-ref" [tests server])
