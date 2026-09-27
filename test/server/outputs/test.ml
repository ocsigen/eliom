(* Tests of actions that reload the page. *)

open Eliom_test_server
open Lwt.Syntax

let check_response ?status ?body msg (r : Browser.response) =
  Option.iter
    (fun s -> Alcotest.(check int) (msg ^ ": status") s r.status)
    status;
  Option.iter (fun b -> Alcotest.(check string) (msg ^ ": body") b r.body) body

(* [body b url] is the body of the page [url], answered with 200 *)
let body b url =
  let+ r = Browser.get b url in
  check_response url ~status:200 r;
  r.body

(* [case server name f] is a test that runs [f b] with a new browser [b]. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (Browser.create server)))

(* A GET action reloads the page without its coservice. *)
let actions server =
  let case = case server in
  ( "actions"
  , [ case "non-attached" (fun b ->
        (* The other GET parameters of the page are kept. *)
        let* url = body b "/link?to=non-attached" in
        let uri = Uri.of_string url in
        let url =
          Uri.path_and_query
            (Uri.add_query_param'
               (Uri.with_path (Uri.remove_query_param uri "to") "/page")
               ("x", "1"))
        in
        let+ r = Browser.get b url in
        check_response "page" ~status:200 ~body:"page x=1 value=non-attached" r)
    ; case "attached" (fun b ->
        let* url = body b "/link?to=attached" in
        let+ r = Browser.get b url in
        check_response "fallback" ~status:200 ~body:"fallback value=attached" r)
    ; case "on a path" (fun b ->
        (* There is no page to reload. *)
        let* r = Browser.get b "/path_action" in
        check_response "action" ~status:200 ~body:"" r;
        let+ r = Browser.get b "/page" in
        check_response "done" ~status:200 ~body:"page x=none value=path" r) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-outputs" [actions server])
