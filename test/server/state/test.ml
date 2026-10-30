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
        (* Without a session group, a session is counted with the sessions
           without a group of the site, which belong to other users: only
           the session is closed. *)
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
        check "size" "2" c "/group/size")
    ; case "limit, services and persistent data" (fun browser ->
        let a = browser () and b = browser () and c = browser () in
        let open_states b v =
          let* _ = text b ("/set?v=" ^ v) in
          let* url = text b "/coservice" in
          let* _ = text b ("/persistent/set?v=" ^ v) in
          let+ _ = join b ~max:2 "limit-sp" in
          url
        in
        let* url_a = open_states a "a" in
        let* url_b = open_states b "b" in
        let* url_c = open_states c "c" in
        (* The oldest session of the group is closed. *)
        let* () = check "services, oldest" "fallback" a url_a in
        let* () = check "services, second" "coservice of b" b url_b in
        let* () = check "services, newest" "coservice of c" c url_c in
        let* () = check "persistent, oldest" "" a "/persistent/get" in
        let* () = check "persistent, second" "b" b "/persistent/get" in
        check "persistent, newest" "c" c "/persistent/get")
    ; case "limit, least recently used" (fun browser ->
        (* The session closed is the one used least recently, not the
           oldest one: [a] is used after [b] joins, so [b] is closed. *)
        let a = browser () and b = browser () and c = browser () in
        let open_states b v =
          let* _ = text b ("/set?v=" ^ v) in
          let* url = text b "/coservice" in
          let* _ = text b ("/persistent/set?v=" ^ v) in
          let+ _ = join b ~max:2 "limit-lru" in
          url
        in
        let* url_a = open_states a "a" in
        let* url_b = open_states b "b" in
        let* () = check "data, used" "a" a "/get" in
        let* () = check "services, used" "coservice of a" a url_a in
        let* () = check "persistent, used" "a" a "/persistent/get" in
        let* url_c = open_states c "c" in
        let* () = check "data, kept" "a" a "/get" in
        let* () = check "services, kept" "coservice of a" a url_a in
        let* () = check "persistent, kept" "a" a "/persistent/get" in
        let* () = check "data, newest" "c" c "/get" in
        let* () = check "services, newest" "coservice of c" c url_c in
        let* () = check "persistent, newest" "c" c "/persistent/get" in
        (* Last: reading a session reference without value opens a
           session. *)
        let* () = check "services, closed" "fallback" b url_b in
        let* () = check "persistent, closed" "" b "/persistent/get" in
        check "data, closed" "" b "/get")
    ; case "limit, simultaneous logins" (fun browser ->
        (* Browsers join a full group at the same time: the group keeps
           exactly its maximum number of sessions, and the other ones are
           closed. A session lost from the list of a persistent group would
           neither count nor be closed. *)
        let admin = browser () in
        let browsers = List.init 8 (fun _ -> browser ()) in
        let* () =
          Lwt_list.iteri_s
            (fun i b ->
               let v = string_of_int i in
               let* _ = text b ("/set?v=" ^ v) in
               Lwt.map ignore (text b ("/persistent/set?v=" ^ v)))
            browsers
        in
        let* () =
          Lwt_list.iter_p
            (fun b -> Lwt.map ignore (join b ~max:3 "limit-simultaneous"))
            browsers
        in
        let alive url =
          Lwt_list.fold_left_s
            (fun n b ->
               let+ v = text b url in
               if v = "" then n else n + 1)
            0 browsers
        in
        let* () =
          check "persistent sessions of the group" "3" admin
            "/group/persistent_sessions?name=limit-simultaneous"
        in
        let* () =
          check "volatile sessions of the group" "3" admin
            "/group/sessions?name=limit-simultaneous"
        in
        let* n = alive "/persistent/get" in
        Alcotest.(check int) "persistent sessions alive" 3 n;
        let+ n = alive "/get" in
        Alcotest.(check int) "data sessions alive" 3 n)
    ; case "group changed by another request" (fun browser ->
        (* The end of a request writes back the persistent session group
           only if the request changed it: here a slow request of the
           session ends after another one has taken the session out of its
           group. *)
        let admin = browser () and a = browser () in
        let* _ = text a "/persistent/set?v=a" in
        let* _ = join a "changed-g" in
        (* A short timeout, and a pause: the expiration date, computed from
           the beginning of the slow request, moves enough to be written at
           its end. *)
        let* _ = text a "/persistent/timeout?t=30" in
        let* () = Lwt_unix.sleep 1. in
        let slow = text a "/persistent/slow?t=1" in
        let* () = Lwt_unix.sleep 0.3 in
        let* _ = text a "/group/persistent/leave" in
        let* v = slow in
        (* The slow request has read the group before it was left *)
        Alcotest.(check string) "group read by the slow request" "changed-g" v;
        let* () = check "group" "none" a "/group/persistent/name" in
        check "sessions of the group" "0" admin
          "/group/persistent_sessions?name=changed-g")
    ; case "group coservice without a group" (fun browser ->
        (* Without a session group, the session is its own group: a service
           of the group scope is not visible to the other sessions without a
           group. *)
        let a = browser () and b = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = text b "/set?v=b" in
        let* url = text a "/group/coservice" in
        let* () = check "own" "group coservice of a" a url in
        check "other session without a group" "fallback" b url)
    ; case "group coservices" (fun browser ->
        (* The services of the group scope are found for the sessions of
           the group, and only for them *)
        let a = browser () and b = browser () and c = browser () in
        let d = browser () in
        let* _ = text a "/set?v=a" in
        let* _ = join a "coservices-g" in
        let* _ = text b "/set?v=b" in
        let* _ = join b "coservices-g" in
        let* _ = text c "/set?v=c" in
        let* _ = join c "coservices-h" in
        let* _ = text d "/set?v=d" in
        let* url = text a "/group/coservice" in
        let* na = text a "/group/nacoservice" in
        (* A link to a non-attached coservice is on the current page: on
           /fallback, the sessions that do not find it get the fallback *)
        let na =
          let i = String.index na '?' in
          "/fallback" ^ String.sub na i (String.length na - i)
        in
        let* () = check "owner" "group coservice of a" a url in
        let* () = check "other member" "group coservice of a" b url in
        let* () = check "other group" "fallback" c url in
        let* () = check "no group" "fallback" d url in
        let* () = check "non-attached, owner" "group nacoservice of a" a na in
        let* () =
          check "non-attached, other member" "group nacoservice of a" b na
        in
        let* () = check "non-attached, other group" "fallback" c na in
        check "non-attached, no group" "fallback" d na)
    ; case "expired session" (fun browser ->
        (* A persistent session that has expired leaves its group when it is
           used again, and does not count in the group any more. *)
        let admin = browser () and a = browser () and b = browser () in
        let* _ = text a "/persistent/set?v=a" in
        let* _ = join a "expired-g" in
        let* _ = text b "/persistent/set?v=b" in
        let* _ = join b "expired-g" in
        let* _ = text a "/persistent/timeout?t=1" in
        let* () =
          check "before" "2" admin "/group/persistent_sessions?name=expired-g"
        in
        let* () = Lwt_unix.sleep 2. in
        let* () = check "expired" "" a "/persistent/get" in
        let* () = check "other member" "b" b "/persistent/get" in
        check "after" "1" admin "/group/persistent_sessions?name=expired-g")
    ; case "close the sessions of a group" (fun browser ->
        (* From outside the group, as Os.Session.disconnect_all does *)
        let admin = browser () and a = browser () and b = browser () in
        let c = browser () in
        let open_states b ?(group = "sessions-g") v =
          let* _ = text b ("/set?v=" ^ v) in
          let* url = text b "/coservice" in
          let* _ = text b ("/persistent/set?v=" ^ v) in
          let+ _ = join b group in
          url
        in
        let* url_a = open_states a "a" in
        let* _ = open_states b "b" in
        let* url_c = open_states c ~group:"sessions-h" "c" in
        let* () =
          check "sessions" "2" admin "/group/sessions?name=sessions-g"
        in
        let* _ = text admin "/group/close_sessions?name=sessions-g" in
        let* () = check "closed" "0" admin "/group/sessions?name=sessions-g" in
        let* () = check "other group" "c" c "/get" in
        let* () = check "other group, services" "coservice of c" c url_c in
        let* () = check "other group, persistent" "c" c "/persistent/get" in
        let* () = check "services" "fallback" a url_a in
        let* () = check "persistent" "" a "/persistent/get" in
        let* () = check "data" "" a "/get" in
        check "other member" "" b "/get")
    ; case "simultaneous requests" (fun browser ->
        (* The sessions of a persistent group are listed in one entry of the
           database, which each request that opens or uses a session of the
           group reads and writes back. Two simultaneous requests must not
           lose each other's change: here pairs of browsers join the group
           at the same time, and every session must be closed with the
           group. *)
        let admin = browser () and a = browser () in
        let* _ = text a "/persistent/set?v=a" in
        let* _ = join a "simultaneous-g" in
        let* members =
          Lwt_list.map_s
            (fun _ ->
               let b = browser () and c = browser () in
               let* _ = text b "/persistent/set?v=b" in
               let* _ = text c "/persistent/set?v=c" in
               let+ _ =
                 Lwt.both (join b "simultaneous-g") (join c "simultaneous-g")
               in
               [b; c])
            (List.init 5 Fun.id)
        in
        let* _ = text admin "/group/close_sessions?name=simultaneous-g" in
        Lwt_list.iter_s
          (fun b -> check "closed" "" b "/persistent/get")
          (a :: List.concat members)) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-state"
      [isolation server; closing server; groups server])
