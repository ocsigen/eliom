(* Tests of the states of tabs (client processes): the tabs of a browser share
   its cookies, and each has its own tab cookies, which a client-side program
   sends in a header. *)

open Eliom_test_server
open Lwt.Syntax

(* A tab of a browser, with its tab cookies *)
type tab = {browser : Browser.t; mutable tab_cookies : (string * string) list}

let tab browser = {browser; tab_cookies = []}

(* The tab cookies set by a response, as by the client-side program *)
let store_tab_cookies tab (r : Browser.response) =
  match Browser.header r Eliom.Common_base.set_tab_cookies_header_name with
  | None -> ()
  | Some json ->
      Ocsigen_cookie_map.Map_path.iter
        (fun _path cookies ->
           Ocsigen_cookie_map.Map_inner.iter
             (fun name cookie ->
                let others = List.remove_assoc name tab.tab_cookies in
                tab.tab_cookies <-
                  (match cookie with
                  | Ocsigen_cookie_map.OSet (_, value, _) ->
                      (name, value) :: others
                  | Ocsigen_cookie_map.OUnset -> others))
             cookies)
        (Eliom.Cookies_base.cookieset_of_json json)

let get tab url =
  let headers =
    [ ( Eliom.Common_base.tab_cookies_header_name
      , Deriving_Json.to_string [%json: (string * string) list] tab.tab_cookies
      ) ]
  in
  let+ r = Browser.get ~headers tab.browser url in
  store_tab_cookies tab r; r

let text tab url =
  let+ r = get tab url in
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
    ; case "tab cookies" (fun browser ->
        (* A tab is its tab cookies, as a session is its cookie: whoever
           sends them gets the tab, even from another browser. *)
        let t1 = tab (browser ()) and t2 = tab (browser ()) in
        let* _ = text t1 "/tab/set?v=1" in
        t2.tab_cookies <- t1.tab_cookies;
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
