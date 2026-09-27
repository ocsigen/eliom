module Volatile = Eliom.Reference.Volatile
module Common = Eliom.Common

let test_site_scope () =
  let r = Volatile.eref ~scope:Common.site_scope 0 in
  let a =
    Site.init ~site_dir:["a"] ~app:"site-a" (fun () ->
      Volatile.set r 1; Volatile.get r)
  in
  let b = Site.init ~site_dir:["b"] ~app:"site-b" (fun () -> Volatile.get r) in
  Alcotest.(check int) "set on the site" 1 a;
  Alcotest.(check int) "other site" 0 b

let test_site_scope_outside_a_site () =
  (* A program starts in the initialisation phase of Ocsigen Server, where
     site references are those of the default site. *)
  Ocsigen.Extensions.end_initialisation ();
  let r = Volatile.eref ~scope:Common.site_scope 0 in
  let check msg f =
    match f () with
    | () -> Alcotest.failf "%s: no error" msg
    | exception Common.Site_information_not_available _ -> ()
  in
  check "get" (fun () -> ignore (Volatile.get r : int));
  check "set" (fun () -> Volatile.set r 1);
  check "unset" (fun () -> Volatile.unset r)

let test_global_scope () =
  let r = Volatile.eref ~scope:Common.global_scope 0 in
  Site.init ~site_dir:["a"] ~app:"global-a" (fun () -> Volatile.set r 1);
  Alcotest.(check int)
    "other site" 1
    (Site.init ~site_dir:["b"] ~app:"global-b" (fun () -> Volatile.get r));
  Alcotest.(check int) "outside a site" 1 (Volatile.get r)

(* [counter ()] is a function returning 1, 2... at each call, and the number
   of its calls. *)
let counter () =
  let calls = ref 0 in
  (fun () -> incr calls; !calls), calls

let test_from_fun_site_scope () =
  let f, calls = counter () in
  let r = Volatile.eref_from_fun ~scope:Common.site_scope f in
  Alcotest.(check int) "not called at creation" 0 !calls;
  let reads ~app site_dir =
    Site.init ~site_dir ~app (fun () -> List.init 2 (fun _ -> Volatile.get r))
  in
  Alcotest.(check (list int))
    "first site" [1; 1]
    (reads ~app:"from-fun-a" ["a"]);
  Alcotest.(check (list int))
    "second site" [2; 2]
    (reads ~app:"from-fun-b" ["b"])

let test_from_fun_global_scope () =
  let f, calls = counter () in
  let r = Volatile.eref_from_fun ~scope:Common.global_scope f in
  Alcotest.(check int) "not called at creation" 0 !calls;
  Alcotest.(check int) "first read" 1 (Volatile.get r);
  Alcotest.(check int) "second read" 1 (Volatile.get r);
  (* [unset] restores the default value, computed again. *)
  Volatile.unset r;
  Alcotest.(check int) "after unset" 2 (Volatile.get r)

let test_scopes_needing_a_request () =
  let check msg scope =
    let r = Volatile.eref ~scope 0 in
    let check_operation name f =
      match f () with
      | () -> Alcotest.failf "%s, %s: no error" msg name
      | exception Common.Request_information_not_available _ -> ()
    in
    Site.init ~app:msg (fun () ->
      check_operation "get" (fun () -> ignore (Volatile.get r : int));
      check_operation "set" (fun () -> Volatile.set r 1);
      check_operation "modify" (fun () -> Volatile.modify r succ);
      check_operation "unset" (fun () -> Volatile.unset r))
  in
  check "session" Common.default_session_scope;
  check "session group" Common.default_group_scope;
  check "client process" Common.default_process_scope;
  check "request" Common.request_scope

let suite =
  ( "references"
  , [ Alcotest.test_case "site scope" `Quick test_site_scope
    ; Alcotest.test_case "site scope outside a site" `Quick
        test_site_scope_outside_a_site
    ; Alcotest.test_case "global scope" `Quick test_global_scope
    ; Alcotest.test_case "from a function, site scope" `Quick
        test_from_fun_site_scope
    ; Alcotest.test_case "from a function, global scope" `Quick
        test_from_fun_global_scope
    ; Alcotest.test_case "scopes needing a request" `Quick
        test_scopes_needing_a_request ] )
