open Eliom_test_server

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

let check_header msg name expected r =
  Alcotest.(check (option string)) msg expected (Browser.header r name)

(* Absolute locations: the host and port depend on the connection. *)
let check_location msg expected r =
  Alcotest.(check (option string))
    msg (Some expected)
    (Option.map
       (fun l -> Uri.path_and_query (Uri.of_string l))
       (Browser.header r "location"))

let content_type r =
  Option.map
    (fun ct -> List.hd (String.split_on_char ';' ct))
    (Browser.header r "content-type")

(* [case server name f] is a test that runs [f b] with a new browser [b]. *)
let case server name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f (Browser.create server)))

open Lwt.Syntax

let dispatch server =
  let case = case server in
  ( "dispatch"
  , [ case "path" (fun b ->
        let+ r = Browser.get b "/hello" in
        check_response "hello" ~status:200 ~body:"hello" r)
    ; case "unknown path" (fun b ->
        let+ r = Browser.get b "/nope" in
        check_response "404" ~status:404 r)
    ; case "directory" (fun b ->
        let* r = Browser.get b "/dir/" in
        check_response "with slash" ~status:200 ~body:"directory" r;
        let+ r = Browser.get b "/dir" in
        check_response "without slash" ~status:301 r;
        check_location "location" "/dir/" r)
    ; case "GET and POST" (fun b ->
        let* r = Browser.get b "/form" in
        check_response "GET" ~status:200 ~body:"get" r;
        let+ r = Browser.post b "/form" ["v", "x y"] in
        check_response "POST" ~status:200 ~body:"post x y" r)
    ; case "POST on a GET service" (fun b ->
        let+ r = Browser.post b "/hello" ["v", "x"] in
        check_response "404" ~status:404 r)
    ; case "next service" (fun b ->
        let* r = Browser.get b "/alternatives?i=1" in
        check_response "first service" ~status:200 ~body:"int 1" r;
        let+ r = Browser.get b "/alternatives?s=x" in
        check_response "second service" ~status:200 ~body:"string x" r)
    ; case "priority" (fun b ->
        let+ r = Browser.get b "/priority?x=1" in
        check_response "higher priority" ~status:200 ~body:"string 1" r)
    ; case "unregister" (fun b ->
        let* r = Browser.get b "/temporary" in
        check_response "before" ~status:200 ~body:"temporary" r;
        let* _ = Browser.get b "/unregister" in
        let+ r = Browser.get b "/temporary" in
        check_response "after" ~status:404 r)
    ; case "handler failure" (fun b ->
        let+ r = Browser.get b "/failure" in
        check_response "500" ~status:500 r;
        Alcotest.(check bool)
          "no details" false
          (contains r.body "handler failure")) ] )

let parameters server =
  let case = case server in
  ( "parameters"
  , [ case "atoms" (fun b ->
        let+ r = Browser.get b "/params?i=12&s=a+b%26c" in
        check_response "decoded" ~status:200 ~body:"i=12 s=a b&c" r)
    ; case "suffix" (fun b ->
        let* r = Browser.get b "/suffix/3/x%20y%26%C3%A9" in
        check_response "suffix" ~status:200 ~body:"i=3 s=x y&\xc3\xa9" r;
        let+ r = Browser.get b "/suffix/x/y" in
        check_response "wrong type" ~status:400 r)
    ; case "suffix and parameters" (fun b ->
        let+ r = Browser.get b "/prod/3?q=a" in
        check_response "decoded" ~status:200 ~body:"i=3 q=a" r)
    ; case "whole suffix as a parameter" (fun b ->
        (* The version of the service without suffix redirects to the one
           with suffix, without ".." segments. *)
        let+ r = Browser.get b "/files/__eliom_suffix__?path=../a/../b" in
        check_response "redirection" ~status:307 r;
        check_location "location" "/files/a/b" r)
    ; case "optional" (fun b ->
        let* r = Browser.get b "/opt" in
        check_response "absent" ~status:200 ~body:"none" r;
        let+ r = Browser.get b "/opt?i=4" in
        check_response "present" ~status:200 ~body:"4" r)
    ; case "list and set" (fun b ->
        let* r = Browser.get b "/list?l.x[0]=a&l.x[1]=b" in
        check_response "list" ~status:200 ~body:"a,b" r;
        let+ r = Browser.get b "/set?i=3&i=1&i=2" in
        check_response "set" ~status:200 ~body:"1,2,3" r)
    ; case "wrong type" (fun b ->
        let+ r = Browser.get b "/params?i=x&s=a" in
        check_response "400" ~status:400 r)
    ; case "missing parameter" (fun b ->
        let+ r = Browser.get b "/params?i=1" in
        check_response "400" ~status:400 r)
    ; case "unexpected parameter" (fun b ->
        let+ r = Browser.get b "/params?i=1&s=a&z=2" in
        check_response "400" ~status:400 r)
    ; case "POST parameters" (fun b ->
        let* r = Browser.post b "/post_int" ["n", "7"] in
        check_response "decoded" ~status:200 ~body:"7" r;
        let* r = Browser.post b "/post_int" ["n", "x"] in
        check_response "wrong type" ~status:400 r;
        let* r = Browser.post b "/post_int" [] in
        check_response "missing" ~status:400 r;
        let+ r = Browser.post b "/post_int?z=1" ["n", "7"] in
        check_response "unexpected GET parameter" ~status:400 r) ] )

(* Values of suffix parameters containing slashes, written %2F *)
let suffixes server =
  let case = case server in
  ( "suffixes"
  , [ case "slash in a value" (fun b ->
        let+ r = Browser.get b "/suffix/3/x%2Fy" in
        check_response "decoded" ~status:200 ~body:"i=3 s=x/y" r)
    ; case "generated URL" (fun b ->
        let* r = Browser.get b "/suffix_link" in
        check_response "URL" ~status:200 ~body:"/suffix/3/x%2Fy" r;
        let+ r = Browser.get b r.body in
        check_response "followed" ~status:200 ~body:"i=3 s=x/y" r)
    ; case "whole suffix" (fun b ->
        let* r = Browser.get b "/files/a/b%2Fc" in
        check_response "split" ~status:200 ~body:"a|b|c" r;
        let+ r = Browser.get b "/files/a/..%2F..%2Fsecret" in
        check_response "no dot dot" ~status:200 ~body:"a|secret" r)
    ; case "whole suffix as a string" (fun b ->
        let+ r = Browser.get b "/tree/a/b%2Fc/..%2Fd" in
        check_response "no dot dot" ~status:200 ~body:"a/b/c/d" r)
    ; case "dot dot in a value" (fun b ->
        (* Only the ".." segments of the path are removed. *)
        let+ r = Browser.get b "/suffix/3/..%2Fetc" in
        check_response "kept" ~status:200 ~body:"i=3 s=../etc" r)
    ; case "malformed escape" (fun b ->
        (* The segment is kept as it is, as by Ocsigen Server. *)
        let+ r = Browser.get b "/suffix/3/%ZZ" in
        check_response "kept" ~status:200 ~body:"i=3 s=%ZZ" r)
    ; case "version without suffix" (fun b ->
        let+ r = Browser.get b "/suffix/__eliom_suffix__?i=3&s=x%2Fy" in
        check_response "redirection" ~status:307 r;
        check_location "location" "/suffix/3/x%2Fy" r)
    ; case "relative URL" (fun b ->
        (* The browser sees two segments in /relative/a%2Fb. *)
        let+ r = Browser.get b "/relative/a%2Fb" in
        check_response "one level up" ~status:200 ~body:"../hello" r)
    ; case "request information" (fun b ->
        let+ r = Browser.get b "/info/a%2Fb" in
        check_response "paths" ~status:200 ~body:"info|a/b info|a/b" r) ] )

let outputs server =
  let case = case server in
  ( "outputs"
  , [ case "Html" (fun b ->
        let+ r = Browser.get b "/html" in
        check_response "status" ~status:200 r;
        Alcotest.(check (option string))
          "content type" (Some "text/html") (content_type r);
        Alcotest.(check bool) "document" true (contains r.body "<p>body</p>"))
    ; case "Html_text" (fun b ->
        let+ r = Browser.get b "/html_text" in
        check_response "body" ~status:200 ~body:"<p>text</p>" r;
        Alcotest.(check (option string))
          "content type" (Some "text/html") (content_type r))
    ; case "CssText" (fun b ->
        let+ r = Browser.get b "/css" in
        check_response "body" ~status:200 ~body:"p {}" r;
        Alcotest.(check (option string))
          "content type" (Some "text/css") (content_type r);
        check_header "cache" "cache-control" (Some "max-age=3600") r)
    ; case "no default charset" (fun b ->
        (* The test server has no default charset. *)
        let+ r = Browser.get b "/hello" in
        check_header "content type" "content-type" (Some "text/plain") r)
    ; case "no cache" (fun b ->
        let+ r = Browser.get b "/no_cache" in
        check_header "cache" "cache-control" (Some "no-cache") r)
    ; case "code and headers" (fun b ->
        let+ r = Browser.get b "/code" in
        check_response "code" ~status:201 ~body:"created" r;
        check_header "header" "x-test" (Some "yes") r)
    ; case "Unit" (fun b ->
        let+ r = Browser.get b "/unit" in
        check_response "no content" ~status:204 ~body:"" r)
    ; case "Action" (fun b ->
        let+ r = Browser.post b "/action" ["v", "x"] in
        check_response "no content" ~status:204 ~body:"" r)
    ; case "Redirection" (fun b ->
        let* r = Browser.get b "/redirect" in
        check_response "found" ~status:302 r;
        check_header "location" "location" (Some "hello") r;
        let+ r = Browser.get b "/moved" in
        check_response "moved permanently" ~status:301 r;
        check_header "moved location" "location" (Some "hello") r)
    ; case "String_redirection" (fun b ->
        let+ r = Browser.get b "/redirect_string" in
        check_response "found" ~status:302 r;
        check_header "location" "location" (Some "http://example.org/elsewhere")
          r)
    ; case "File" (fun b ->
        let* r = Browser.get b "/file" in
        check_response "content" ~status:200 ~body:"file content" r;
        Alcotest.(check (option string))
          "content type" (Some "text/plain") (content_type r);
        check_header "cache" "cache-control" (Some "max-age=60") r;
        let+ r = Browser.get b "/missing_file" in
        check_response "missing" ~status:404 r)
    ; case "File_ct" (fun b ->
        let+ r = Browser.get b "/file_ct" in
        check_response "content" ~status:200 ~body:"file content" r;
        Alcotest.(check (option string))
          "content type" (Some "text/x-test") (content_type r);
        check_header "cache" "cache-control" (Some "max-age=60") r)
    ; case "Any" (fun b ->
        let* r = Browser.get b "/any?html=on" in
        check_response "HTML" ~status:200 ~body:"<p>any</p>" r;
        Alcotest.(check (option string))
          "HTML content type" (Some "text/html") (content_type r);
        let+ r = Browser.get b "/any" in
        check_response "text" ~status:200 ~body:"any" r;
        Alcotest.(check (option string))
          "text content type" (Some "text/plain") (content_type r)) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-services"
      [dispatch server; parameters server; suffixes server; outputs server])
