(* Tests of Comet channels, requested by a native client as the client-side
   program of Eliom does. Requests of data that expect none are idle: the
   test server answers a waiting request after 1 s. *)

open Eliom_test_server
open Eliom_test_client
open Lwt.Syntax
module C = Comet_client

let text tab url =
  let+ r = Tab.get tab url in
  if r.status <> 200 then Alcotest.failf "%s: status %d" url r.status;
  r.body

let message =
  Alcotest.testable
    (fun fmt -> function
       | C.Data s -> Format.fprintf fmt "Data %S" s
       | C.Full -> Format.fprintf fmt "Full"
       | C.Closed -> Format.fprintf fmt "Closed")
    ( = )

(* [check msg expected messages] checks the messages of one channel. *)
let check msg expected messages =
  Alcotest.(check (list message)) msg expected (List.map snd messages)

(* A new tab of a new browser, with a new channel of its client process,
   registered *)
let channel ?size server =
  let tab = Tab.create (Browser.create server) in
  let* info =
    text tab
      ("/stateful/create"
      ^ match size with Some s -> "?size=" ^ string_of_int s | None -> "")
  in
  let info = Comet_info.of_string info in
  let+ () = C.register tab info in
  tab, info

(* [raises msg exn p] checks that the request [p ()] fails with [exn]. *)
let raises msg exn p =
  Lwt.catch
    (fun () ->
       let+ _ = p () in
       Alcotest.failf "%s: answered" msg)
    (fun e -> if e = exn then Lwt.return_unit else Lwt.fail e)

let push tab v = Lwt.map ignore (text tab ("/stateful/push?v=" ^ v))

let case server name f =
  Alcotest.test_case name `Quick (fun () -> Lwt_main.run (f server))

let stateful server =
  let case = case server in
  ( "stateful"
  , [ case "messages in order" (fun server ->
        let* tab, info = channel server in
        let* () = push tab "a" in
        let* () = push tab "b" in
        let* m = C.request tab info 1 in
        check "first request" [C.Data "a"; C.Data "b"] m;
        let* () = push tab "c" in
        let+ m = C.request tab info 2 in
        check "second request" [C.Data "c"] m)
    ; case "no data" (fun server ->
        let* tab, info = channel server in
        let+ m = C.request ~idle:true tab info 1 in
        check "idle request" [] m)
    ; case "waiting request" (fun server ->
        (* A request without data waits for the next message. *)
        let* tab, info = channel server in
        let+ m, () =
          Lwt.both (C.request tab info 1)
            (let* () = Lwt_unix.sleep 0.2 in
             push tab "late")
        in
        check "woken up" [C.Data "late"] m)
    ; case "same request number" (fun server ->
        (* A request sent again gets the same answer. *)
        let* tab, info = channel server in
        let* () = push tab "a" in
        let* m = C.request tab info 1 in
        check "first" [C.Data "a"] m;
        let+ m = C.request ~idle:true tab info 1 in
        check "again" [C.Data "a"] m)
    ; case "tabs" (fun server ->
        let* t1, i1 = channel server in
        let* t2, i2 = channel server in
        let* () = push t1 "for t1" in
        let* m = C.request ~idle:true t2 i2 1 in
        check "other tab" [] m;
        let+ m = C.request t1 i1 1 in
        check "own tab" [C.Data "for t1"] m)
    ; case "full buffer" (fun server ->
        let* tab, info = channel ~size:2 server in
        let* () = push tab "a" in
        let* () = push tab "b" in
        let* () = push tab "c" in
        let+ m = C.request tab info 1 in
        check "full" [C.Full] m)
    ; case "close" (fun server ->
        let* tab, info = channel server in
        let* () = C.close tab info in
        let* () = push tab "a" in
        let+ m = C.request ~idle:true tab info 1 in
        check "not sent" [] m)
    ; case "closed client process" (fun server ->
        let* tab, info = channel server in
        let* _ = text tab "/discard" in
        raises "request" C.State_closed (fun () ->
          C.request ~idle:true tab info 1))
    ; case "other browser" (fun server ->
        (* The service of the channel is registered for its client process
           only. *)
        let* _, info = channel server in
        let other = Tab.create (Browser.create server) in
        raises "request" C.State_closed (fun () ->
          C.request ~idle:true other info 1))
    ; case "timeout" (fun server ->
        let* tab, info = channel server in
        let start = Unix.gettimeofday () in
        let+ () = raises "request" C.Timeout (fun () -> C.request tab info 1) in
        let d = Unix.gettimeofday () -. start in
        (* The test server answers after 1 s. *)
        if d < 0.8 || d > 5. then Alcotest.failf "answered after %.2f s" d)
    ; case "unknown channel" (fun server ->
        (* The channel may be created later: it is waited for. *)
        let* tab, info = channel server in
        let unknown = {info with channel = info.channel ^ "-unknown"} in
        let* () = C.register tab unknown in
        let+ m = C.request ~idle:true tab unknown 1 in
        check "no data" [] m) ] )

let stateless server =
  let site name =
    let b = Browser.create server in
    let tab = Tab.create b in
    let+ info = text tab ("/" ^ name ^ "/info") in
    b, tab, Comet_info.of_string info
  in
  let values =
    List.map (fun (_, m) -> match m with C.Data (v, _) -> Some v | _ -> None)
  in
  let case = case server in
  ( "stateless"
  , [ case "several clients" (fun _ ->
        let* a, tab, info = site "stateless" in
        let* b, _, _ = site "stateless" in
        let* _ = text tab "/stateless/push?v=x" in
        let* _ = text tab "/stateless/push?v=y" in
        let* ma = C.request_stateless a info (Eliom.Comet_base.Last (Some 2)) in
        let+ mb = C.request_stateless b info (Eliom.Comet_base.Last (Some 2)) in
        Alcotest.(check (list (option string)))
          "a" [Some "x"; Some "y"] (values ma);
        Alcotest.(check (list (option string)))
          "b" [Some "x"; Some "y"] (values mb))
    ; case "after an index" (fun _ ->
        let* a, tab, info = site "stateless" in
        let* _ = text tab "/stateless/push?v=z" in
        let* m = C.request_stateless a info (Eliom.Comet_base.Newest 1) in
        match m with
        | [(_, C.Data ("z", i))] ->
            let* _ = text tab "/stateless/push?v=w" in
            (* From the index after the last message received *)
            let+ m =
              C.request_stateless a info (Eliom.Comet_base.After (i + 1))
            in
            Alcotest.(check (list (option string))) "next" [Some "w"] (values m)
        | _ -> Alcotest.fail "last message expected")
    ; case "last messages" (fun _ ->
        let* a, tab, info = site "stateless" in
        let* _ = text tab "/stateless/push?v=p" in
        let* _ = text tab "/stateless/push?v=q" in
        let+ m = C.request_stateless a info (Eliom.Comet_base.Last (Some 1)) in
        Alcotest.(check (list (option string))) "last" [Some "q"] (values m))
    ; case "newest" (fun _ ->
        (* Only the last message is kept. *)
        let* a, tab, info = site "newest" in
        let* _ = text tab "/newest/push?v=1" in
        let* _ = text tab "/newest/push?v=2" in
        let+ m = C.request_stateless a info (Eliom.Comet_base.Last (Some 5)) in
        Alcotest.(check (list (option string))) "last" [Some "2"] (values m))
    ; case "timeout" (fun _ ->
        (* No message comes after this index: answered with no data *)
        let* a, _, info = site "stateless" in
        let+ m = C.request_stateless a info (Eliom.Comet_base.After max_int) in
        Alcotest.(check (list (option string))) "no data" [] (values m))
    ; case "unknown channel" (fun _ ->
        let* a, _, info = site "stateless" in
        let unknown = {info with channel = info.channel ^ "-unknown"} in
        let+ m =
          C.request_stateless a unknown (Eliom.Comet_base.Last (Some 1))
        in
        match m with
        | [(id, C.Closed)] when id = unknown.channel -> ()
        | _ -> Alcotest.fail "closed expected")
    ; case "external channel" (fun _ ->
        (* The channel "stateless", declared as a channel of another server *)
        let* a, tab, info = site "stateless" in
        let* ext = text tab "/external/info" in
        let ext = Comet_info.of_string ext in
        Alcotest.(check string) "url" ("http://eliom-test" ^ info.url) ext.url;
        Alcotest.(check (list (pair string string)))
          "hidden parameters" info.post_params ext.post_params;
        Alcotest.(check string) "idle" info.idle_param ext.idle_param;
        Alcotest.(check string) "request" info.request_param ext.request_param;
        Alcotest.(check string) "channel" info.channel ext.channel;
        let* _ = text tab "/stateless/push?v=e" in
        let+ m =
          C.request_stateless a {ext with url = info.url}
            (Eliom.Comet_base.Last (Some 1))
        in
        Alcotest.(check (list (option string))) "last" [Some "e"] (values m)) ]
  )

(* Requests that the client-side program does not send *)
let errors server =
  let module B = Eliom.Comet_base in
  let post tab (c : Comet_info.t) params =
    Tab.post tab c.url (c.post_params @ params)
  in
  let request (c : Comet_info.t) r =
    [c.request_param, Deriving_Json.to_string B.comet_request_json r]
  in
  let status msg expected (r : Browser.response) =
    Alcotest.(check int) msg expected r.status
  in
  let comet_error msg (r : Browser.response) =
    status msg 200 r;
    match Deriving_Json.from_string B.answer_json r.body with
    | B.Comet_error _ -> ()
    | _ -> Alcotest.failf "%s: Comet_error expected" msg
  in
  let stateless tab =
    let+ info = text tab "/stateless/info" in
    Comet_info.of_string info
  in
  let case = case server in
  ( "errors"
  , [ case "stateless, without parameters" (fun server ->
        let tab = Tab.create (Browser.create server) in
        let* info = stateless tab in
        let+ r = Tab.get tab info.url in
        comet_error "answer" r)
    ; case "stateless, malformed" (fun server ->
        (* Typing error of the parameters *)
        let tab = Tab.create (Browser.create server) in
        let* info = stateless tab in
        let+ r = post tab info [info.request_param, "garbage"] in
        status "status" 400 r)
    ; case "stateless, stateful request" (fun server ->
        let tab = Tab.create (Browser.create server) in
        let* info = stateless tab in
        let+ r = post tab info (request info (B.Stateful (B.Request_data 1))) in
        comet_error "answer" r)
    ; case "stateful, malformed" (fun server ->
        let* tab, info = channel server in
        let+ r = post tab info [info.request_param, "garbage"] in
        status "status" 400 r)
    ; case "stateful, stateless request" (fun server ->
        let* tab, info = channel server in
        let+ r = post tab info (request info (B.Stateless [||])) in
        comet_error "answer" r) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-comet"
      [stateful server; stateless server; errors server])
