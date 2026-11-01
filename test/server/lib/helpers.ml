open Lwt.Syntax

let contains s sub =
  let n = String.length sub in
  let rec loop i =
    i + n <= String.length s && (String.sub s i n = sub || loop (i + 1))
  in
  loop 0

let check_response ?status ?body msg (r : Browser.response) =
  Option.iter
    (fun s -> Alcotest.(check int) (msg ^ ": status") s r.status)
    status;
  Option.iter (fun b -> Alcotest.(check string) (msg ^ ": body") b r.body) body

let text b url =
  let+ r = Browser.get b url in
  if r.status <> 200 then Alcotest.failf "%s: status %d" url r.status;
  r.body

let check msg expected b url =
  let+ body = text b url in
  Alcotest.(check string) msg expected body

let case ?(speed = `Quick) server name f =
  Alcotest.test_case name speed (fun () ->
    Lwt_main.run (f (Browser.create server)))

let case_browsers server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (fun () -> Browser.create server)))
