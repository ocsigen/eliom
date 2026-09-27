(* Tests of the states of browsers: each simulated browser has its own
   cookies, hence its own sessions. *)

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

(* [case server name f] is a test that runs [f browser], where [browser ()]
   is a new browser. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (fun () -> Browser.create server)))

let cookie_header cookies =
  "cookie", String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) cookies)

(* [check_replay msg expected cookies b url] checks the body of the response
   to [url] for [b] sending [cookies] instead of its own: a closed state must
   be gone on the server, not only forgotten by the browser. *)
let check_replay msg expected cookies b url =
  let+ r = Browser.get ~headers:[cookie_header cookies] b url in
  Alcotest.(check string) (msg ^ " (old cookies)") expected r.body

let isolation server =
  let case = case server in
  ( "isolation"
  , [ case "session data" (fun browser ->
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let* () = check "own value" "a" a "/get" in
        let* () = check "other browser" "" b "/get" in
        let* _ = text b "/set?v=b" in
        check "unchanged by the other browser" "a" a "/get")
    ; case "persistent data" (fun browser ->
        let a = browser () and b = browser () in
        let* _ = text a "/persistent/set?v=a" in
        let* () = check "own value" "a" a "/persistent/get" in
        check "other browser" "" b "/persistent/get")
    ; case "status" (fun browser ->
        let a = browser () in
        let* () = check "new browser" "data=empty services=empty" a "/status" in
        let* _ = text a "/set?v=a" in
        check "with data" "data=alive services=empty" a "/status")
    ; case "session coservice" (fun browser ->
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let* url = text a "/coservice" in
        let* () = check "owner" "coservice of a" a url in
        let* () = check "services" "data=alive services=alive" a "/status" in
        check "other browser" "fallback" b url)
    ; case "cookie" (fun browser ->
        (* A session is its cookie: whoever sends it gets the session. *)
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let+ r =
          Browser.get ~headers:[cookie_header (Browser.cookies a)] b "/get"
        in
        Alcotest.(check string) "value of the cookie" "a" r.body) ] )

let closing server =
  let case = case server in
  ( "closing"
  , [ case "discard the session" (fun browser ->
        let a = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text a "/persistent/set?v=a" in
        let* url = text a "/coservice" in
        let old_cookies = Browser.cookies a in
        let* _ = text a "/discard?scope=session" in
        Alcotest.(check (list (pair string string)))
          "cookies removed" [] (Browser.cookies a);
        let* () = check "status" "data=empty services=empty" a "/status" in
        let* () = check "persistent data" "" a "/persistent/get" in
        let* () = check "data" "" a "/get" in
        let* () = check "services" "fallback" a url in
        let c = browser () in
        let* () = check_replay "data" "" old_cookies c "/get" in
        let* () =
          check_replay "persistent data" "" old_cookies c "/persistent/get"
        in
        check_replay "services" "fallback" old_cookies c url)
    ; case "discard the data only" (fun browser ->
        let a = browser () in
        let* _ = text a "/set?v=a" in
        let* url = text a "/coservice" in
        let* _ = text a "/discard_data?scope=session" in
        let* () = check "data" "" a "/get" in
        check "services kept" "coservice of a" a url)
    ; case "discard the services only" (fun browser ->
        let a = browser () in
        let* _ = text a "/set?v=a" in
        let* url = text a "/coservice" in
        let* _ = text a "/discard_services?scope=session" in
        let* () = check "services" "fallback" a url in
        check "data kept" "a" a "/get")
    ; case "other sessions" (fun browser ->
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text b "/set?v=b" in
        let* url = text b "/coservice" in
        let* _ = text a "/discard?scope=session" in
        let* () = check "data of the other browser" "b" b "/get" in
        check "services of the other browser" "coservice of b" b url) ] )

(* Each test uses groups of its own, since the server is shared. *)
let groups server =
  let case = case server in
  let join b ?max name =
    text b
      ("/group/join?name=" ^ name
      ^ match max with Some m -> "&max=" ^ string_of_int m | None -> "")
  in
  ( "groups"
  , [ case "group data" (fun browser ->
        let a = browser () and b = browser () and c = browser () in
        let* _ = join a "data-g" in
        let* _ = join b "data-g" in
        let* _ = join c "data-h" in
        let* _ = text a "/group/set?v=g" in
        let* _ = text a "/set?v=a" in
        let* () = check "same group" "g" b "/group/get" in
        let* () = check "other group" "" c "/group/get" in
        let* () = check "session data stays private" "" b "/get" in
        check "size" "2" a "/group/size")
    ; case "discard a group" (fun browser ->
        let a = browser () and b = browser () and c = browser () in
        let* _ = join a "discard-g" in
        let* _ = join b "discard-g" in
        let* _ = join c "discard-h" in
        let* _ = text a "/set?v=a" in
        let* _ = text b "/set?v=b" in
        let* _ = text c "/set?v=c" in
        let* url = text b "/coservice" in
        let* _ = text a "/group/set?v=g" in
        let* _ = text b "/persistent/set?v=b" in
        let* _ = text a "/group/persistent/set?v=g" in
        let* _ = text a "/discard?scope=group" in
        (* Closing a group closes all its sessions. *)
        let* () = check "session of the browser" "" a "/get" in
        let* () = check "session of the other member" "" b "/get" in
        let* () = check "services of the other member" "fallback" b url in
        let* () = check "group data" "" b "/group/get" in
        let* () =
          check "persistent data of the other member" "" b "/persistent/get"
        in
        let* () = check "persistent group data" "" b "/group/persistent/get" in
        check "other group" "c" c "/get")
    ; case "discard a group without a named group" (fun browser ->
        (* Without a session group, the group of a session is its subnet,
           which holds the sessions of other users: only the session is
           closed. The browsers of the tests all have the same address. *)
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text a "/persistent/set?v=a" in
        let* url_a = text a "/coservice" in
        let* _ = text b "/set?v=b" in
        let* _ = text b "/persistent/set?v=b" in
        let* url_b = text b "/coservice" in
        let old_cookies = Browser.cookies a in
        let* _ = text a "/discard?scope=group" in
        let* () = check "data" "" a "/get" in
        let* () = check "persistent data" "" a "/persistent/get" in
        let* () = check "services" "fallback" a url_a in
        let* () = check "data of the other browser" "b" b "/get" in
        let* () =
          check "persistent data of the other browser" "b" b "/persistent/get"
        in
        let* () =
          check "services of the other browser" "coservice of b" b url_b
        in
        let c = browser () in
        let* () = check_replay "data" "" old_cookies c "/get" in
        let* () =
          check_replay "persistent data" "" old_cookies c "/persistent/get"
        in
        check_replay "services" "fallback" old_cookies c url_a)
    ; case "limit" (fun browser ->
        let a = browser () and b = browser () and c = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = join a ~max:2 "limit-g" in
        let* _ = text b "/set?v=b" in
        let* _ = join b ~max:2 "limit-g" in
        let* _ = text c "/set?v=c" in
        let* _ = join c ~max:2 "limit-g" in
        (* The oldest session of the group is closed. *)
        let* () = check "oldest" "" a "/get" in
        let* () = check "second" "b" b "/get" in
        let* () = check "newest" "c" c "/get" in
        check "size" "2" c "/group/size") ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-state"
      [isolation server; closing server; groups server])
