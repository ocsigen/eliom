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

let test_scopes_needing_a_request () =
  let check msg scope =
    let r = Volatile.eref ~scope 0 in
    match Site.init ~app:msg (fun () -> Volatile.get r) with
    | v -> Alcotest.failf "%s: got %d" msg v
    | exception Common.Request_information_not_available _ -> ()
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
    ; Alcotest.test_case "scopes needing a request" `Quick
        test_scopes_needing_a_request ] )
