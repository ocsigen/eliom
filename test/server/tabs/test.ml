(* Tests of the states of tabs (client processes): the tabs of a browser share
   its cookies, and each has its own tab cookies, which a client-side program
   sends in a header. *)

open Eliom_test_server
open Lwt.Syntax

let tab = Eliom_test_client.Tab.create

let text tab url =
  let+ r = Eliom_test_client.Tab.get tab url in
  if r.status <> 200 then Alcotest.failf "%s: status %d" url r.status;
  r.body

let check msg expected tab url =
  let+ body = text tab url in
  Alcotest.(check string) msg expected body

(* [case server name f] is a test that runs [f browser], where [browser ()]
   is a new browser. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (fun () -> Browser.create server)))

let tabs server =
  let case = case server in
  ( "tabs"
  , [ case "tab data" (fun browser ->
        let b = browser () in
        let t1 = tab b and t2 = tab b in
        let* _ = text t1 "/tab/set?v=1" in
        let* _ = text t1 "/session/set?v=s" in
        let* () = check "own tab" "1" t1 "/tab/get" in
        let* () = check "other tab" "" t2 "/tab/get" in
        check "session shared by the tabs" "s" t2 "/session/get")
    ; case "tab coservice" (fun browser ->
        let b = browser () in
        let t1 = tab b and t2 = tab b in
        let* url = text t1 "/tab/coservice" in
        let* () = check "own tab" "coservice of the tab" t1 url in
        check "other tab" "fallback" t2 url)
    ; case "leave a group of services" (fun browser ->
        (* The service session of the browser has no service, but it is not
           closed, as a tab has one. *)
        let t = tab (browser ()) in
        let* _ = text t "/group/join?name=leave-g" in
        let* url = text t "/tab/coservice" in
        let* _ = text t "/group/leave_services" in
        check "tab coservice" "coservice of the tab" t url)
    ; case "tab cookies" (fun browser ->
        (* A tab is its tab cookies, as a session is its cookie: whoever
           sends them gets the tab, even from another browser. *)
        let t1 = tab (browser ()) and t2 = tab (browser ()) in
        let* _ = text t1 "/tab/set?v=1" in
        Eliom_test_client.Tab.set_tab_cookies t2
          (Eliom_test_client.Tab.tab_cookies t1);
        check "value of the tab cookies" "1" t2 "/tab/get") ] )

let closing server =
  let case = case server in
  ( "closing"
  , [ case "close a tab" (fun browser ->
        let b = browser () in
        let t1 = tab b and t2 = tab b in
        let* _ = text t1 "/tab/set?v=1" in
        let* _ = text t2 "/tab/set?v=2" in
        let* _ = text t1 "/session/set?v=s" in
        let* _ = text t1 "/discard?scope=tab" in
        let* () = check "closed tab" "" t1 "/tab/get" in
        let* () = check "other tab" "2" t2 "/tab/get" in
        check "session" "s" t2 "/session/get")
    ; case "close the session" (fun browser ->
        (* Closing a browser session closes its tabs. *)
        let b = browser () in
        let t1 = tab b and t2 = tab b in
        let* _ = text t1 "/session/set?v=s" in
        let* _ = text t1 "/tab/set?v=1" in
        let* _ = text t2 "/tab/set?v=2" in
        let* url = text t2 "/tab/coservice" in
        let* _ = text t1 "/discard?scope=session" in
        let* () = check "session" "" t2 "/session/get" in
        let* () = check "tab of the request" "" t1 "/tab/get" in
        let* () = check "other tab" "" t2 "/tab/get" in
        check "coservice of the other tab" "fallback" t2 url)
    ; case "close the group" (fun browser ->
        (* Closing a group closes the tabs of its sessions. *)
        let a = tab (browser ()) and b = tab (browser ()) in
        let* _ = text a "/group/join?name=tabs-g" in
        let* _ = text b "/group/join?name=tabs-g" in
        let* _ = text b "/tab/set?v=b" in
        let* _ = text a "/discard?scope=group" in
        check "tab of another session of the group" "" b "/tab/get") ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-tabs"
      [tabs server; closing server])
