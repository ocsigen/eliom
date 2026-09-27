(* Tests of the collectors of expired states, run on demand: states expire
   after 1 s of inactivity, and /collect runs each collector once. Each test
   ends with all the states of the server collected. *)

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

(* [case server name f] is a test that runs [f browser admin], where
   [browser ()] is a new browser and [admin], which opens no session, counts
   and collects the states. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run
      (let admin = Browser.create server in
       let* () =
         check "no session at the beginning"
           "data=0 services=0 persistent=0 groups=[]" admin "/count"
       in
       f (fun () -> Browser.create server) admin))

let collect admin = Lwt.map ignore (text admin "/collect")

let collection server =
  let case = case server in
  ( "collection"
  , [ case "not expired" (fun browser admin ->
        let a = browser () in
        let* _ = text a "/set?v=a" in
        let* () = collect admin in
        let* () =
          check "kept" "data=1 services=0 persistent=0 groups=[]" admin "/count"
        in
        let* () = check "value" "a" a "/get" in
        let* () = sleep 1.5 in
        collect admin)
    ; case "volatile data sessions" (fun browser admin ->
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text b "/set?v=b" in
        let* () =
          check "open" "data=2 services=0 persistent=0 groups=[]" admin "/count"
        in
        let* () = sleep 1.5 in
        let* () =
          check "not collected yet" "data=2 services=0 persistent=0 groups=[]"
            admin "/count"
        in
        let* () = collect admin in
        check "collected" "data=0 services=0 persistent=0 groups=[]" admin
          "/count")
    ; case "sessions in use" (fun browser admin ->
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text b "/set?v=b" in
        (* a is accessed within its timeout, while b is left for 2.4 s *)
        let rec use n =
          if n = 0
          then Lwt.return_unit
          else
            let* () = sleep 0.6 in
            let* () = check "access" "a" a "/get" in
            use (n - 1)
        in
        let* () = use 4 in
        let* () = collect admin in
        let* () =
          check "only the expired one"
            "data=1 services=0 persistent=0 groups=[]" admin "/count"
        in
        let* () = check "value kept" "a" a "/get" in
        let* () = sleep 1.5 in
        collect admin)
    ; case "service sessions" (fun browser admin ->
        let a = browser () in
        let* _ = text a "/coservice" in
        let* () =
          check "open" "data=0 services=1 persistent=0 groups=[]" admin "/count"
        in
        let* () = sleep 1.5 in
        let* () = collect admin in
        check "collected" "data=0 services=0 persistent=0 groups=[]" admin
          "/count")
    ; case "persistent sessions" (fun browser admin ->
        let a = browser () in
        let* _ = text a "/persistent/set?v=a" in
        let* () =
          check "open" "data=0 services=0 persistent=1 groups=[]" admin "/count"
        in
        let* () = sleep 1.5 in
        let* () = collect admin in
        check "collected" "data=0 services=0 persistent=0 groups=[]" admin
          "/count")
    ; case "session groups" (fun browser admin ->
        (* A group goes with the last of its sessions. *)
        let a = browser () and b = browser () in
        let* _ = text a "/group/join?name=gc-g&v=g" in
        let* _ = text b "/group/join?name=gc-g&v=g" in
        let* () =
          check "open" "data=2 services=0 persistent=0 groups=[gc-g]" admin
            "/count"
        in
        let* () = sleep 1.5 in
        let* () = collect admin in
        check "collected" "data=0 services=0 persistent=0 groups=[]" admin
          "/count") ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-gc" [collection server])
