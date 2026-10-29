(* Tests of actions that reload the page, of services sending OCaml values,
   and of the pages of an application. *)

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

(* The number of occurrences of [sub] in [s] *)
let occurrences s sub =
  let n = String.length sub in
  let rec count i acc =
    if i + n > String.length s
    then acc
    else if String.sub s i n = sub
    then count (i + n) (acc + 1)
    else count (i + 1) acc
  in
  count 0 0

let check_occurrences msg expected s sub =
  Alcotest.(check int) msg expected (occurrences s sub)

(* [case server name f] is a test that runs [f b] with a new browser [b]. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (Browser.create server)))

(* [page_url link] is the URL of the page [/page?x=1] with the parameters
   of [link], the link to a non-attached action *)
let page_url link =
  let uri = Uri.of_string link in
  Uri.path_and_query
    (Uri.add_query_param'
       (Uri.with_path (Uri.remove_query_param uri "to") "/page")
       ("x", "1"))

(* A GET action reloads the page without its coservice. *)
let actions server =
  let case = case server in
  ( "actions"
  , [ case "non-attached" (fun b ->
        (* The other GET parameters of the page are kept. *)
        let* url = body b "/link?to=non-attached" in
        let+ r = Browser.get b (page_url url) in
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

(* The answer of a service sending an OCaml value to the client-side
   program *)
let ocaml_value r : [`Success of int | `Failure of string] =
  Eliom_test_client.Ocaml_answer.decode r

let ocaml server =
  let case = case server in
  ( "Ocaml"
  , [ case "value" (fun b ->
        let+ r = Browser.get b "/ocaml?i=1" in
        check_response "answer" ~status:200 r;
        match ocaml_value r with
        | `Success n -> Alcotest.(check int) "value" 2 n
        | `Failure _ -> Alcotest.fail "failure")
    ; case "exception" (fun b ->
        (* The client gets the code of the error, which the server logs. *)
        let+ r = Browser.get b "/ocaml?i=-1" in
        check_response "answer" ~status:200 r;
        match ocaml_value r with
        | `Failure code -> Alcotest.(check int) "code" 6 (String.length code)
        | `Success _ -> Alcotest.fail "success") ] )

let app server =
  let case = case server in
  ( "App"
  , [ case "page" (fun b ->
        let+ r = Browser.get b "/app" in
        check_response "page" ~status:200 r;
        Alcotest.(check (option string))
          "application" (Some "test_app")
          (Browser.header r "x-eliom-application");
        Alcotest.(check (option string))
          "content type" (Some "text/html")
          (Browser.header r "content-type");
        check_occurrences "data of the site" 1 r.body "__eliom_appl_sitedata =";
        check_occurrences "data of the request" 1 r.body
          "__eliom_request_data =";
        check_occurrences "program" 1 r.body {|src="./test_app.js"|};
        check_occurrences "content" 1 r.body "<p>initial request</p>")
    ; case "program given by the page" (fun b ->
        let+ page = body b "/app_with_script" in
        check_occurrences "program" 1 page {|src="./test_app.js"|})
    ; case "not launched" (fun b ->
        let+ page = body b "/app_not_launched" in
        check_occurrences "program" 0 page "test_app.js";
        check_occurrences "data of the request" 0 page "__eliom_request_data";
        check_occurrences "content" 1 page "<p>initial request</p>")
    ; case "request of the client process" (fun b ->
        (* The tab cookies of the first page tell the application. *)
        let tab = Eliom_test_client.Tab.create b in
        let* r = Eliom_test_client.Tab.get tab "/app" in
        check_occurrences "first" 1 r.body "<p>initial request</p>";
        let+ r = Eliom_test_client.Tab.get tab "/app" in
        check_occurrences "next" 1 r.body "<p>request of the client process</p>")
    ] )

(* The HTTP status set by the handler with [Registration.set_status] *)
let status server =
  let case = case server in
  ( "status"
  , [ case "set by the handler" (fun b ->
        let+ r = Browser.get b "/status" in
        check_response "page" ~status:404 ~body:"404 Not Found" r)
    ; case "none by default" (fun b ->
        let+ r = Browser.get b "/no_status" in
        check_response "page" ~status:200 ~body:"none" r)
    ; case "over the code of the registration" (fun b ->
        let+ r = Browser.get b "/status_over_code" in
        check_response "page" ~status:404 r)
    ; case "page of an application" (fun b ->
        let+ r = Browser.get b "/app_status" in
        check_response "page" ~status:404 r;
        Alcotest.(check (option string))
          "application" (Some "test_app")
          (Browser.header r "x-eliom-application"))
    ; case "action without reload" (fun b ->
        let+ r = Browser.get b "/status_no_reload" in
        check_response "action" ~status:400 ~body:"" r)
    ; case "dropped when the page is reloaded" (fun b ->
        let* url = body b "/link?to=status" in
        let+ r = Browser.get b (page_url url) in
        check_response "page" ~status:200 ~body:"page x=1 value=status" r)
    ; case "dropped when the handler raises" (fun b ->
        let+ r = Browser.get b "/status_raise" in
        check_response "error page" ~status:200 r;
        check_occurrences "content" 1 r.body "<p>error page</p>")
    ; case "page of an application, from the client process" (fun b ->
        let tab = Eliom_test_client.Tab.create b in
        let* _ = Eliom_test_client.Tab.get tab "/app" in
        let+ r =
          Eliom_test_client.Tab.get ~expecting_process_page:true tab
            "/app_status"
        in
        check_response "page" ~status:404 r;
        Alcotest.(check (option string))
          "application" (Some "test_app")
          (Browser.header r "x-eliom-application")) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-outputs"
      [actions server; ocaml server; app server; status server])
