(* Tests of the services sending files: content type, validators,
   conditional and range requests. *)

open Eliom_test_server
open Lwt.Syntax

let check_status msg expected (r : Browser.response) =
  Alcotest.(check int) (msg ^ ": status") expected r.status

let check_header msg name expected r =
  Alcotest.(check (option string))
    (msg ^ ": " ^ name)
    expected (Browser.header r name)

(* [case server name f] is a test that runs [f b] with a new browser [b]. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (Browser.create server)))

(* [check_content_type b url expected] checks that the file [url] is sent
   with 200 and the content type [expected] *)
let check_content_type b url expected =
  let+ r = Browser.get b url in
  check_status url 200 r;
  check_header url "content-type" (Some expected) r

let content_type server =
  let case = case server in
  ( "content type"
  , [ case "from a rule of the configuration" (fun b ->
        check_content_type b "/file?name=page.foo" "text/x-foo; charset=utf-8")
    ; case "from magic-mime" (fun b ->
        (* No charset for a type that is not text *)
        check_content_type b "/file?name=image.png" "image/png")
    ; case "given at registration" (fun b ->
        check_content_type b "/file_content_type?name=page.foo"
          "text/x-test; charset=utf-8")
    ; case "given at registration, over the one of the headers" (fun b ->
        check_content_type b "/file_content_type_headers?name=page.foo"
          "text/x-test; charset=utf-8")
    ; case "charset given at registration" (fun b ->
        check_content_type b "/file_charset?name=page.foo"
          "text/x-foo; charset=iso-8859-1")
    ; case "given by File_ct" (fun b ->
        check_content_type b "/file_ct" "application/x-test") ] )

let headers server =
  let case = case server in
  ( "headers"
  , [ case "given at registration" (fun b ->
        let+ r = Browser.get b "/file_options?name=text.txt" in
        check_status "file" 200 r;
        check_header "file" "x-test" (Some "yes") r;
        check_header "file" "cache-control" (Some "max-age=60") r)
    ; case "validators" (fun b ->
        let+ r = Browser.get b "/file?name=text.txt" in
        check_status "file" 200 r;
        Alcotest.(check bool)
          "etag" true
          (Option.is_some (Browser.header r "etag"));
        Alcotest.(check bool)
          "last-modified" true
          (Option.is_some (Browser.header r "last-modified"));
        check_header "file" "accept-ranges" (Some "bytes") r;
        Alcotest.(check string) "body" "0123456789" r.body) ] )

(* [header_of b url name] is the header [name] of the file [url] *)
let header_of b url name =
  let+ r = Browser.get b url in
  match Browser.header r name with
  | Some v -> v
  | None -> Alcotest.failf "%s: no %s header" url name

let conditional server =
  let case = case server in
  ( "conditional requests"
  , [ case "If-None-Match" (fun b ->
        let url = "/file_options?name=text.txt" in
        let* etag = header_of b url "etag" in
        let+ r = Browser.get ~headers:["if-none-match", etag] b url in
        check_status "file" 304 r;
        Alcotest.(check string) "body" "" r.body;
        check_header "file" "etag" (Some etag) r;
        (* The headers given at registration are also sent with 304. *)
        check_header "file" "cache-control" (Some "max-age=60") r)
    ; case "If-Modified-Since" (fun b ->
        let url = "/file?name=text.txt" in
        let* date = header_of b url "last-modified" in
        let+ r = Browser.get ~headers:["if-modified-since", date] b url in
        check_status "file" 304 r)
    ; case "other entity tag" (fun b ->
        let+ r =
          Browser.get
            ~headers:["if-none-match", {|W/"other"|}]
            b "/file?name=text.txt"
        in
        check_status "file" 200 r) ] )

let range server =
  let case = case server in
  ( "range requests"
  , [ case "range" (fun b ->
        let+ r =
          Browser.get ~headers:["range", "bytes=2-5"] b "/file?name=text.txt"
        in
        check_status "file" 206 r;
        check_header "file" "content-range" (Some "bytes 2-5/10") r;
        Alcotest.(check string) "body" "2345" r.body)
    ; case "unsatisfiable" (fun b ->
        let+ r =
          Browser.get ~headers:["range", "bytes=20-"] b "/file?name=text.txt"
        in
        check_status "file" 416 r)
    ; case "If-Range with an entity tag" (fun b ->
        (* The entity tag is weak: the whole file is sent. *)
        let url = "/file?name=text.txt" in
        let* etag = header_of b url "etag" in
        let+ r =
          Browser.get ~headers:["range", "bytes=2-5"; "if-range", etag] b url
        in
        check_status "file" 200 r;
        Alcotest.(check string) "body" "0123456789" r.body) ] )

let missing server =
  let case = case server in
  ( "missing file"
  , [ case "404" (fun b ->
        let+ r = Browser.get b "/file?name=missing.txt" in
        check_status "file" 404 r) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-files"
      [ content_type server
      ; headers server
      ; conditional server
      ; range server
      ; missing server ])
