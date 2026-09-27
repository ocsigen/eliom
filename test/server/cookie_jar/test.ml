(* Tests of the cookie jar of the simulated browsers, with an explicit
   clock. *)

open Eliom_test_server

let now = 1000.

(* [jar ?now ~path set_cookies] is the empty jar with [set_cookies], received
   for [path] at [now]. *)
let jar ?(now = now) ~path set_cookies =
  Cookie_jar.store ~now ~path set_cookies Cookie_jar.empty

let check_header ?(now = now) msg expected ~path j =
  Alcotest.(check (option string)) msg expected (Cookie_jar.header ~now ~path j)

let test_dates () =
  Alcotest.(check (option (float 0.)))
    "epoch" (Some 0.)
    (Cookie_jar.parse_date "Thu, 01 Jan 1970 00:00:00 GMT");
  (* Dates written by Ocsigen Server, around leap days and centuries *)
  List.iter
    (fun t ->
       let s = Ocsigen_base.Lib.Date.to_string t in
       Alcotest.(check (option (float 0.))) s (Some t) (Cookie_jar.parse_date s))
    [ 951782400. (* 2000-02-29 *)
    ; 951868800. (* 2000-03-01 *)
    ; 1709164799. (* 2024-02-28 23:59:59 *)
    ; 1790512496. (* 2026-09-27 12:34:56 *)
    ; 4107542400. (* 2100-03-01 *) ];
  List.iter
    (fun s ->
       Alcotest.(check (option (float 0.))) s None (Cookie_jar.parse_date s))
    ["tomorrow"; "Thu, 01 Foo 1970 00:00:00 GMT"; "Thu, 01 Jan 1970 00:00"]

let test_paths () =
  let j = jar ~path:"/" ["a=1; Path=/foo"] in
  check_header "same path" (Some "a=1") ~path:"/foo" j;
  check_header "sub path" (Some "a=1") ~path:"/foo/bar" j;
  check_header "prefix, not a sub path" None ~path:"/foobar" j;
  check_header "parent" None ~path:"/" j;
  let j = jar ~path:"/" ["a=1; Path=/foo/"] in
  check_header "with a slash, sub path" (Some "a=1") ~path:"/foo/bar" j;
  check_header "with a slash, without" None ~path:"/foo" j

let test_default_path () =
  (* The directory of the request *)
  let j = jar ~path:"/a/b/c" ["a=1"] in
  check_header "directory" (Some "a=1") ~path:"/a/b" j;
  check_header "sub path" (Some "a=1") ~path:"/a/b/x" j;
  check_header "parent" None ~path:"/a" j;
  let j = jar ~path:"/a/b/c" ["a=1; Path="; "b=2; Path=rel"] in
  check_header "empty or relative" (Some "a=1; b=2") ~path:"/a/b" j;
  check_header "empty or relative, parent" None ~path:"/a" j;
  check_header "at the root" (Some "a=1") ~path:"/" (jar ~path:"/x" ["a=1"])

let test_expiry () =
  let past = "Thu, 01 Jan 1970 00:00:00 GMT" in
  let j = jar ~path:"/" ["a=1; Path=/"; "b=2; Path=/"] in
  let j = Cookie_jar.store ~now ~path:"/" ["a=; Path=/; Expires=" ^ past] j in
  check_header "removed by expires" (Some "b=2") ~path:"/" j;
  let j = Cookie_jar.store ~now ~path:"/" ["b=; Path=/; Max-Age=0"] j in
  check_header "removed by max-age" None ~path:"/" j;
  let j = jar ~path:"/" ["a=1; Path=/; Max-Age=10"] in
  check_header "before max-age" (Some "a=1") ~now:(now +. 9.) ~path:"/" j;
  check_header "after max-age" None ~now:(now +. 10.) ~path:"/" j;
  Alcotest.(check (list (pair string string)))
    "not listed after max-age" []
    (Cookie_jar.cookies ~now:(now +. 10.) j);
  (* Max-Age takes precedence over Expires, in any order *)
  let j =
    jar ~path:"/"
      [ "a=1; Path=/; Max-Age=10; Expires=" ^ past
      ; "b=2; Path=/; Expires=" ^ past ^ "; Max-Age=10" ]
  in
  check_header "max-age first" (Some "a=1; b=2") ~path:"/" j;
  (* A wrong Max-Age is ignored. *)
  check_header "wrong max-age" (Some "a=1") ~now:(now +. 1e6) ~path:"/"
    (jar ~path:"/" ["a=1; Path=/; Max-Age=0x10"])

let test_replacement () =
  let j = jar ~path:"/" ["a=1; Path=/"; "b=2; Path=/"; "a=3; Path=/x"] in
  let j = Cookie_jar.store ~now ~path:"/" ["a=4; Path=/"] j in
  (* The longer paths first, then the order of creation *)
  check_header "replaced in place" (Some "a=3; a=4; b=2") ~path:"/x" j;
  Alcotest.(check (list (pair string string)))
    "cookies"
    ["a", "3"; "a", "4"; "b", "2"]
    (Cookie_jar.cookies ~now j)

let test_attributes () =
  check_header "last path" (Some "a=1") ~path:"/y"
    (jar ~path:"/" ["a=1; Path=/x; Path=/y"]);
  check_header "case of names" (Some "a=1") ~path:"/y"
    (jar ~path:"/" ["a=1; pATh=/y"]);
  check_header "secure" None ~path:"/" (jar ~path:"/" ["a=1; Path=/; Secure"]);
  check_header "no name or no value" None ~path:"/"
    (jar ~path:"/" ["novalue; Path=/"; "=x; Path=/"]);
  check_header "empty value" (Some "a=") ~path:"/"
    (jar ~path:"/" ["a=; Path=/"])

let () =
  Alcotest.run "eliom-cookie-jar"
    [ ( "cookie jar"
      , [ Alcotest.test_case "dates" `Quick test_dates
        ; Alcotest.test_case "paths" `Quick test_paths
        ; Alcotest.test_case "default path" `Quick test_default_path
        ; Alcotest.test_case "expiry" `Quick test_expiry
        ; Alcotest.test_case "replacement" `Quick test_replacement
        ; Alcotest.test_case "attributes" `Quick test_attributes ] ) ]
