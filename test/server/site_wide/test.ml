(* Tests of the settings of states that apply to a whole site: global and
   default timeouts, scope hierarchies, session groups, the limit of sessions
   per subnet and the collection of expired states. *)

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

(* [case server name f] is a test that runs [f browser], where [browser ()]
   is a new browser. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (fun () -> Browser.create server)))

(* [with_setting b set reset f] runs [f ()] with the setting of the site [set],
   then restores it with [reset]. *)
let with_setting b set reset f =
  let* _ = text b set in
  Lwt.finalize f (fun () -> Lwt.map ignore (text b reset))

let timeouts server =
  let case = case server in
  ( "timeouts"
  , [ case "global timeout" (fun browser ->
        let admin = browser () and b = browser () in
        with_setting admin "/global_timeout?t=1" "/global_timeout" (fun () ->
          let* _ = text b "/set?v=a" in
          let* () = check "before" "a" b "/get" in
          let* () = sleep 1.5 in
          check "after" "" b "/get"))
    ; case "timeout of a user first" (fun browser ->
        let admin = browser () and b = browser () and c = browser () in
        with_setting admin "/global_timeout?t=1" "/global_timeout" (fun () ->
          let* _ = text b "/set?v=b" in
          let* _ = text b "/timeout" in
          let* _ = text c "/set?v=c" in
          let* _ = text c "/timeout?t=5" in
          let* () = sleep 1.5 in
          let* () = check "no timeout" "b" b "/get" in
          check "longer timeout" "c" c "/get"))
    ; case "default timeout" (fun browser ->
        (* For all scope hierarchies *)
        let admin = browser () and b = browser () in
        with_setting admin "/default_timeout?t=1" "/default_timeout" (fun () ->
          let* _ = text b "/other/set?v=o" in
          let* () = check "before" "o" b "/other/get" in
          let* () = sleep 1.5 in
          check "after" "" b "/other/get")) ] )

let hierarchies server =
  let case = case server in
  ( "scope hierarchies"
  , [ case "independent" (fun browser ->
        let b = browser () in
        let* _ = text b "/set?v=a" in
        let* _ = text b "/other/set?v=o" in
        let* _ = text b "/discard?scope=session" in
        let* () = check "discarded" "" b "/get" in
        let* () = check "other hierarchy" "o" b "/other/get" in
        let* _ = text b "/discard?scope=other" in
        check "other hierarchy discarded" "" b "/other/get")
    ; case "discard all scopes" (fun browser ->
        let b = browser () in
        let* _ = text b "/set?v=a" in
        let* _ = text b "/other/set?v=o" in
        let* _ = text b "/group/join?name=all-g" in
        let* _ = text b "/group/set?v=g" in
        let* _ = text b "/discard_all_scopes" in
        let* () = check "session" "" b "/get" in
        let* () = check "other hierarchy" "" b "/other/get" in
        check "group" "" b "/group/get") ] )

let groups server =
  let case = case server in
  ( "groups"
  , [ case "leave a group" (fun browser ->
        let a = browser () and b = browser () in
        let* _ = text a "/group/join?name=leave-g" in
        let* _ = text b "/group/join?name=leave-g" in
        let* _ = text a "/group/set?v=g" in
        let* () = check "name" "leave-g" a "/group/name" in
        let* _ = text a "/group/leave" in
        let* () = check "no group" "none" a "/group/name" in
        (* Without a group, the data of the group scope are those of the
           session. *)
        let* () = check "group data" "" a "/group/get" in
        check "other member" "g" b "/group/get")
    ; case "list of groups" (fun browser ->
        let a = browser () and b = browser () in
        let* _ = text a "/group/join?name=list-a" in
        let* _ = text b "/group/join?name=list-b" in
        let+ groups = text a "/groups" in
        let groups = String.split_on_char ',' groups in
        Alcotest.(check (list bool))
          "listed" [true; true]
          [List.mem "list-a" groups; List.mem "list-b" groups]) ] )

let subnet server =
  let case = case server in
  ( "subnet"
  , [ case "limit of sessions" (fun browser ->
        (* The browsers of the tests all have the same address. *)
        let a = browser () and b = browser () and c = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text b "/set?v=b" in
        let* _ = text c "/set?v=c" in
        let* () = check "number of sessions" "2" a "/count" in
        let* () = check "second" "b" b "/get" in
        let* () = check "newest" "c" c "/get" in
        (* Last: reading a session reference without value opens a session,
           which would close another one. *)
        check "oldest" "" a "/get") ] )

let collection server =
  let case = case server in
  ( "collection"
  , [ case "expired sessions" (fun browser ->
        let a = browser () and b = browser () and c = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text b "/set?v=b" in
        let* _ = text c "/set?v=c" in
        let* () = check "open" "3" a "/count" in
        let* () = sleep 3. in
        (* Collected without being accessed *)
        check "collected" "0" a "/count") ] )

let run name exe suites =
  Server_harness.with_server exe (fun server ->
    Alcotest.run ~and_exit:false name (suites server))

let () =
  run "eliom-server-site-wide" "./timeouts_server.exe" (fun server ->
    [timeouts server; hierarchies server; groups server]);
  run "eliom-server-subnet" "./subnet_server.exe" (fun server ->
    [subnet server]);
  run "eliom-server-collection" "./gc_server.exe" (fun server ->
    [collection server])
