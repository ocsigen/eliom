(* Tests of server functions, called as the client-side program does: a
   POST request with the argument in JSON, answered with an OCaml value. *)

open Eliom_test_server
open Eliom_test_client
open Lwt.Syntax

let info tab path =
  let* r = Tab.get tab path in
  if r.status <> 200 then Alcotest.failf "%s: status %d" path r.status;
  Lwt.return (Service_info.of_string r.body)

let call = Service_info.post

(* A server function that is gone: as for any non-attached coservice not
   found, the page at the path of the request answers, not an OCaml value. *)
let gone msg (r : Browser.response) =
  Alcotest.(check int) (msg ^ ": status") 200 r.status;
  Alcotest.check_raises msg
    (Failure "Ocaml_answer: not an answer of an OCaml service") (fun () ->
    ignore (Ocaml_answer.decode r : [`Success of unit | `Failure of string]))

let success msg expected tab info argument =
  let+ r = call tab info argument in
  match Ocaml_answer.decode r with
  | `Success v -> Alcotest.(check int) msg expected v
  | `Failure code -> Alcotest.failf "%s: failure %s" msg code

let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (Tab.create (Browser.create server))))

let server_functions server =
  let case = case server in
  ( "server functions"
  , [ case "call" (fun tab ->
        let* info = info tab "/incr" in
        success "result" 2 tab info "1")
    ; case "exception" (fun tab ->
        let* info = info tab "/incr" in
        let+ r = call tab info "-1" in
        match Ocaml_answer.decode r with
        | `Failure code -> Alcotest.(check int) "code" 6 (String.length code)
        | `Success (_ : int) -> Alcotest.fail "success")
    ; case "malformed argument" (fun tab ->
        let* info = info tab "/incr" in
        let+ r = call tab info "x" in
        Alcotest.(check int) "typing error" 400 r.status)
    ; case "error handler" (fun tab ->
        (* The answer is compared with the encoding of the expected one: an
           answer of another shape cannot be decoded safely. *)
        let* info = info tab "/with_error_handler" in
        let+ r = call tab info "x" in
        Alcotest.(check string)
          "result of the handler"
          (Eliom.Types.encode_eliom_data
             {Eliom.Runtime.ecs_request_data = [||]; ecs_data = `Success (-1)})
          r.body)
    ; case "max_use" (fun tab ->
        let* info = info tab "/once" in
        let* () = success "first" 1 tab info "1" in
        let+ r = call tab info "1" in
        gone "second" r)
    ; case "of the session" (fun tab ->
        let* info = info tab "/of_the_session" in
        let* r = call tab info "\"s\"" in
        (match Ocaml_answer.decode r with
        | `Success v ->
            Alcotest.(check string) "own session" "of the session s" v
        | `Failure code -> Alcotest.failf "failure %s" code);
        let other = Tab.create (Browser.create server) in
        let+ r = call other info "\"s\"" in
        gone "other browser" r) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-function"
      [server_functions server])
