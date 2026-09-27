open Eliom_test_server
open Lwt.Syntax

let check_response ?status ?body msg (r : Browser.response) =
  Option.iter
    (fun s -> Alcotest.(check int) (msg ^ ": status") s r.status)
    status;
  Option.iter (fun b -> Alcotest.(check string) (msg ^ ": body") b r.body) body

(* [case server name f] is a test that runs [f b] with a new browser [b]. *)
let case ?(speed = `Quick) server name f =
  Alcotest.test_case name speed (fun () ->
    Lwt_main.run (f (Browser.create server)))

(* [link b target] is the URL of the coservice [target] and, for a POST
   coservice, its POST parameters, as given by the service /link of the
   server. *)
let link b target =
  let+ r = Browser.get b ("/link?to=" ^ target) in
  match String.split_on_char '\n' r.body with
  | url :: params ->
      ( url
      , List.map
          (fun p ->
             match String.index_opt p '\t' with
             | Some i ->
                 String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1)
             | None -> Alcotest.failf "wrong POST parameter %S" p)
          params )
  | [] -> Alcotest.failf "no link to %s" target

let get_link b target = Lwt.map fst (link b target)

(* [on_path path url] is [url], a link to a non-attached coservice given by
   /link, on [path] and without the parameter of /link. *)
let on_path path url =
  let uri = Uri.of_string url in
  Uri.path_and_query (Uri.with_path (Uri.remove_query_param uri "to") path)

let attached server =
  let case = case server in
  ( "attached"
  , [ case "anonymous" (fun b ->
        let* url = get_link b "anonymous" in
        Alcotest.(check string)
          "path of the fallback" "/main"
          (Uri.path (Uri.of_string url));
        let* r = Browser.get b url in
        check_response "first call" ~status:200 ~body:"anonymous" r;
        let+ r = Browser.get b url in
        check_response "second call" ~status:200 ~body:"anonymous" r)
    ; case "fallback" (fun b ->
        let* r = Browser.get b "/main" in
        check_response "without coservice" ~status:200 ~body:"main" r;
        let* r = Browser.get b "/main?__eliom_n__=unknown" in
        check_response "unknown anonymous coservice" ~status:200
          ~body:"main, link too old" r;
        let+ r = Browser.get b "/main?__eliom__=unknown" in
        check_response "unknown named coservice" ~status:200
          ~body:"main, link too old" r)
    ; case "named" (fun b ->
        (* The URL of a named coservice can be written by hand. *)
        let* r = Browser.get b "/main?__eliom__=named&__co_eliom_n=1" in
        check_response "by hand" ~status:200 ~body:"named 1" r;
        let* url = get_link b "named" in
        let+ r = Browser.get b url in
        check_response "generated" ~status:200 ~body:"named 3" r)
    ; case "parameters" (fun b ->
        let* r = Browser.get b "/main?__eliom__=named&__co_eliom_n=x" in
        check_response "wrong type" ~status:400 r;
        (* Without its parameters, the coservice is not found. *)
        let* r = Browser.get b "/main?__eliom__=named" in
        check_response "missing" ~status:200 ~body:"main, link too old" r;
        let+ r = Browser.get b "/main?__eliom__=named&__co_eliom_n=1&z=2" in
        check_response "other parameters" ~status:200 ~body:"named 1 z=2" r)
    ; case "POST" (fun b ->
        let* url, params = link b "attached_post" in
        let* r = Browser.post b url params in
        check_response "coservice" ~status:200 ~body:"attached post x" r;
        let* r = Browser.get b url in
        check_response "GET" ~status:200 ~body:"main" r;
        let+ r =
          Browser.post b url
            (List.map
               (fun (n, v) -> n, if n = "__eliom_np__" then "unknown" else v)
               params)
        in
        check_response "unknown coservice" ~status:200
          ~body:"main, link too old" r) ] )

let non_attached server =
  let case = case server in
  ( "non-attached"
  , [ case "anonymous" (fun b ->
        let* url = get_link b "na_anonymous" in
        (* A link to the current page *)
        Alcotest.(check string)
          "path of the page" "/link"
          (Uri.path (Uri.of_string url));
        let* r = Browser.get b url in
        check_response "current page" ~status:200 ~body:"na anonymous" r;
        let* r = Browser.get b (on_path "/main" url) in
        check_response "another page" ~status:200 ~body:"na anonymous" r;
        let+ r = Browser.get b (on_path "/nowhere" url) in
        check_response "no page" ~status:200 ~body:"na anonymous" r)
    ; case "named" (fun b ->
        let* r =
          Browser.get b "/nowhere?__eliom_na__name=na_named&__na_eliom_n=1"
        in
        check_response "by hand" ~status:200 ~body:"na named 1" r;
        let* url = get_link b "na_named" in
        let* r = Browser.get b url in
        check_response "generated" ~status:200 ~body:"na named 4" r;
        let+ r =
          Browser.get b "/main?__eliom_na__name=na_named&__na_eliom_n=x"
        in
        check_response "wrong type" ~status:400 r)
    ; case "POST" (fun b ->
        let* url, params = link b "na_post" in
        let+ r = Browser.post b url params in
        check_response "coservice" ~status:200 ~body:"na post y" r)
    ; case "unknown" (fun b ->
        (* The service of the page is used instead. *)
        let* r = Browser.get b "/main?__eliom_na__num=unknown" in
        check_response "anonymous" ~status:200 ~body:"main, link too old" r;
        let* r = Browser.get b "/main?__eliom_na__name=unknown" in
        check_response "named" ~status:200 ~body:"main, link too old" r;
        let* r = Browser.post b "/main" ["__eliom_na__num", "unknown"] in
        check_response "POST" ~status:200 ~body:"main, link too old" r;
        let+ r = Browser.get b "/nowhere?__eliom_na__num=unknown" in
        check_response "no page" ~status:404 r)
    ; case "attached to a path" (fun b ->
        let* url = get_link b "na_attached" in
        Alcotest.(check string) "path" "/other" (Uri.path (Uri.of_string url));
        let+ r = Browser.get b url in
        check_response "coservice" ~status:200 ~body:"na named 5" r)
    ; case "before attached coservices" (fun b ->
        let+ r =
          Browser.get b
            "/main?__eliom_na__name=na_named&__na_eliom_n=1&__eliom_n__=unknown"
        in
        check_response "non-attached" ~status:200 ~body:"na named 1" r) ] )

(* [calls b url bodies] calls [url] once for each element of [bodies], and
   checks the answers. *)
let calls b url bodies =
  Lwt_list.iteri_s
    (fun i body ->
       let+ r = Browser.get b url in
       check_response (Printf.sprintf "call %d" (i + 1)) ~status:200 ~body r)
    bodies

let limits server =
  let case ?speed = case ?speed server in
  ( "limits"
  , [ case "max_use, attached" (fun b ->
        let* url = get_link b "once" in
        calls b url ["once"; "main, link too old"])
    ; case "max_use, non-attached" (fun b ->
        let* url = get_link b "na_once" in
        calls b (on_path "/main" url) ["na once"; "main, link too old"])
    ; case "created by a request" (fun b ->
        let* url = get_link b "created_once" in
        calls b url ["created"; "main, link too old"])
    ; case ~speed:`Slow "timeout" (fun b ->
        (* 1 s after the creation or the last call *)
        let* url = get_link b "created_timeout" in
        let* () = calls b url ["created"] in
        let* () = Lwt_unix.sleep 0.5 in
        let* () = calls b url ["created"] in
        let* () = Lwt_unix.sleep 0.5 in
        let* () = calls b url ["created"] in
        let* () = Lwt_unix.sleep 1.5 in
        calls b url ["main, link too old"]) ] )

(* Each link to a CSRF-safe coservice is a new coservice of the session of the
   request, which an attacker cannot know nor use from another browser. *)
let csrf_safe server =
  let case = case server in
  let too_old = "main, link too old" in
  ( "CSRF-safe"
  , [ case "a coservice for each link" (fun b ->
        let* first = get_link b "csrf" in
        let* second = get_link b "csrf" in
        Alcotest.(check bool) "new coservice" true (first <> second);
        let* () = calls b first ["csrf"] in
        calls b second ["csrf"])
    ; case "of the session of the link" (fun b ->
        let* url = get_link b "csrf" in
        let* () = calls (Browser.create server) url [too_old] in
        calls b url ["csrf"])
    ; case "max_use" (fun b ->
        let* url = get_link b "csrf_once" in
        calls b url ["csrf once"; too_old])
    ; case "POST" (fun b ->
        let* url, params = link b "csrf_post" in
        let* r = Browser.post (Browser.create server) url params in
        check_response "other browser" ~status:200 ~body:too_old r;
        let+ r = Browser.post b url params in
        check_response "own browser" ~status:200 ~body:"csrf post x" r)
    ; case "non-attached" (fun b ->
        let* url = get_link b "na_csrf" in
        let url = on_path "/main" url in
        let* () = calls (Browser.create server) url [too_old] in
        calls b url ["na csrf"])
    ; case "non-attached, POST" (fun b ->
        let* url, params = link b "na_csrf_post" in
        let url = on_path "/main" url in
        let* r = Browser.post (Browser.create server) url params in
        check_response "other browser" ~status:200 ~body:too_old r;
        let+ r = Browser.post b url params in
        check_response "own browser" ~status:200 ~body:"na csrf post y" r)
    ; case "of a client process" (fun b ->
        let tab = Eliom_test_client.Tab.create b in
        let* r = Eliom_test_client.Tab.get tab "/link?to=csrf_tab" in
        let url = r.body in
        let* r =
          Eliom_test_client.Tab.get (Eliom_test_client.Tab.create b) url
        in
        check_response "other tab" ~status:200 ~body:too_old r;
        let+ r = Eliom_test_client.Tab.get tab url in
        check_response "own tab" ~status:200 ~body:"csrf of the tab" r) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-coservices"
      [attached server; non_attached server; limits server; csrf_safe server])
