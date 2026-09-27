(* Tests of the timeouts of states, coservices and cookies, and of persistent
   data across a restart of the server. Expiry is checked when a state is
   accessed, so timeouts of 1 s are enough. *)

open Eliom_test_server
open Lwt.Syntax

let text b url =
  let+ r = Browser.get b url in
  if r.status <> 200 then Alcotest.failf "%s: status %d" url r.status;
  r.body

let check msg expected b url =
  let+ body = text b url in
  Alcotest.(check string) msg expected body

let sleep = Lwt_unix.sleep

(* [case server name f] is a test that runs [f b] with a new browser [b]. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (Browser.create server)))

let states server =
  let case = case server in
  ( "states"
  , [ case "data" (fun b ->
        let* _ = text b "/set?v=a" in
        let* _ = text b "/timeout?kind=data&t=1" in
        let* () = check "before" "a" b "/get" in
        let* () = sleep 1.5 in
        check "after" "" b "/get")
    ; case "access extends the timeout" (fun b ->
        let* _ = text b "/set?v=a" in
        let* _ = text b "/timeout?kind=data&t=1" in
        let rec access n =
          if n = 0
          then Lwt.return_unit
          else
            let* () = sleep 0.5 in
            let* () = check "within the timeout" "a" b "/get" in
            access (n - 1)
        in
        let* () = access 4 in
        let* () = sleep 1.5 in
        check "after" "" b "/get")
    ; case "services" (fun b ->
        let* _ = text b "/set?v=a" in
        let* url = text b "/coservice" in
        let* _ = text b "/timeout?kind=service&t=1" in
        let* () = check "before" "coservice" b url in
        let* () = sleep 1.5 in
        let* () = check "after" "fallback" b url in
        check "data kept" "a" b "/get")
    ; case "persistent data" (fun b ->
        let* _ = text b "/persistent/set?v=a" in
        let* _ = text b "/timeout?kind=persistent&t=1" in
        let* () = check "before" "a" b "/persistent/get" in
        let* () = sleep 1.5 in
        check "after" "" b "/persistent/get")
    ; case "no timeout" (fun b ->
        let* _ = text b "/set?v=a" in
        let* _ = text b "/timeout?kind=data&t=1" in
        let* _ = text b "/timeout?kind=data" in
        let* () = sleep 1.5 in
        check "kept" "a" b "/get") ] )

let coservices server =
  let case = case server in
  ( "coservices"
  , [ case "timeout" (fun b ->
        let* url = text b "/timed_coservice?t=1" in
        let* () = check "before" "coservice" b url in
        let* () = sleep 1.5 in
        check "after" "fallback" b url)
    ; case "maximum number of uses" (fun b ->
        let* url = text b "/counted_coservice?n=2" in
        let* () = check "first use" "coservice" b url in
        let* () = check "second use" "coservice" b url in
        check "third use" "fallback" b url) ] )

(* The expiration dates of the cookies set by a response *)
let expirations (r : Browser.response) =
  List.map
    (fun h ->
       let expires =
         List.find_map
           (fun a ->
              match String.split_on_char '=' (String.trim a) with
              | [n; v] when String.lowercase_ascii n = "expires" ->
                  Cookie_jar.parse_date v
              | _ -> None)
           (String.split_on_char ';' h)
       in
       expires)
    (Cohttp.Header.get_multi r.headers "set-cookie")

(* [check_expiration msg ~now d r] checks that [r] sets one cookie, expiring
   [d] seconds after [now]. *)
let check_expiration msg ~now d r =
  match expirations r with
  | [Some e] ->
      if e < now +. d -. 10. || e > now +. d +. 10.
      then Alcotest.failf "%s: expires at %.0f, %.0f s from now" msg e (e -. now)
  | _ -> Alcotest.failf "%s: one cookie with an expiration date expected" msg

let cookies server =
  let case = case server in
  ( "cookies"
  , [ case "default" (fun b ->
        (* Ten years after the opening of the session *)
        let now = Unix.time () in
        let+ r = Browser.get b "/set?v=a" in
        check_expiration "in ten years" ~now 315532800. r)
    ; case "browser session" (fun b ->
        let* _ = text b "/set?v=a" in
        let+ r = Browser.get b "/cookie_expiration" in
        Alcotest.(check (list (option (float 0.))))
          "no expiration date" [None] (expirations r))
    ; case "expiration date" (fun b ->
        let* _ = text b "/set?v=a" in
        let now = Unix.time () in
        let+ r = Browser.get b "/cookie_expiration?t=3600" in
        check_expiration "in one hour" ~now 3600. r) ] )

let restart server =
  ( "restart"
  , [ Alcotest.test_case "persistent data" `Quick (fun () ->
        Lwt_main.run
          (let b = Browser.create server in
           let* _ = text b "/set?v=a" in
           let* _ = text b "/persistent/set?v=p" in
           Server_harness.restart server;
           let* () = check "persistent data kept" "p" b "/persistent/get" in
           check "volatile data lost" "" b "/get")) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-timeouts"
      [states server; coservices server; cookies server; restart server])
