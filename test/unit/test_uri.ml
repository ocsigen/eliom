module Uri = Eliom.Eliom_uri
module Service = Eliom.Service
module Parameter = Eliom.Parameter

let path = Alcotest.(list string)

let test_relative_path () =
  let check msg expected src dest =
    Alcotest.check path msg expected
      (Uri.reconstruct_relative_url_path src dest)
  in
  check "same directory" ["c"] ["a"; "b"] ["a"; "c"];
  check "same page" ["b"] ["a"; "b"] ["a"; "b"];
  check "subdirectory" ["b"; "c"] ["a"; "b"] ["a"; "b"; "c"];
  check "from a directory" ["b"] ["a"; ""] ["a"; "b"];
  check "sibling directory" [".."; "x"; "y"] ["a"; "b"] ["x"; "y"];
  check "two levels up" [".."; ".."; "x"] ["a"; "b"; "c"] ["x"];
  check "from the root" ["a"; "b"] [""] ["a"; "b"]

let test_actual_path () =
  let check msg expected p =
    Alcotest.check path msg expected (Uri.make_actual_path p)
  in
  check "no dots" ["a"; "b"] ["a"; "b"];
  check "dot dot" ["b"] ["a"; ".."; "b"];
  check "absolute" [""; "b"] [""; "a"; ".."; "b"];
  check "above the root" [""; "b"] [""; ".."; ".."; "b"];
  check "leading dot dot" ["a"] [".."; "a"]

let test_from_components () =
  let check msg expected components =
    Alcotest.(check string)
      msg expected
      (Uri.make_string_uri_from_components components)
  in
  check "path only" "/a/b" ("/a/b", [], None);
  check "parameters" "/a/b?x=1&y=a+b" ("/a/b", ["x", "1"; "y", "a b"], None);
  check "fragment" "/a#f" ("/a", [], Some "f");
  check "parameters and fragment" "/a?x=1#f" ("/a", ["x", "1"], Some "f")

let test_proto_prefix () =
  let check msg expected ~https =
    Alcotest.(check string)
      msg expected
      (Uri.make_proto_prefix ~hostname:"example.org"
         ~port:(if https then 443 else 80)
         https)
  in
  check "http" "http://example.org/" ~https:false;
  check "https" "https://example.org/" ~https:true;
  Alcotest.(check string)
    "other port" "http://example.org:8080/"
    (Uri.make_proto_prefix ~hostname:"example.org" ~port:8080 false)

(* External services: their URL does not depend on the current site, as long
   as the protocol is given. *)

let extern get_params =
  Service.extern ~prefix:"http://example.org" ~path:["a"; "b"]
    ~meth:(Service.Get get_params) ()

let components ?fragment ?nl_params service v =
  let uri, params, fragment =
    Uri.make_uri_components ~https:false ?fragment ?nl_params ~service v
  in
  uri, List.sort compare params, fragment

let components_t =
  Alcotest.(triple string (list (pair string string)) (option string))

let test_extern () =
  let check msg expected actual =
    Alcotest.check components_t msg expected actual
  in
  check "no parameter"
    ("http://example.org/a/b", [], None)
    (components (extern Parameter.unit) ());
  check "parameters"
    ("http://example.org/a/b", ["i", "1"; "s", "x y"], None)
    (components (extern Parameter.(int "i" ** string "s")) (1, "x y"));
  check "suffix"
    ("http://example.org/a/b/3/x", [], None)
    (components (extern Parameter.(suffix (int "i" ** string "s"))) (3, "x"));
  check "encoded path"
    ("http://example.org/a/b/x%2Fy", [], None)
    (components (extern Parameter.(suffix (string "s"))) "x/y");
  check "fragment"
    ("http://example.org/a/b", [], Some "top")
    (components ~fragment:"top" (extern Parameter.unit) ());
  let preapplied =
    (* [preapply] creates a client value: allowed in a request or at module
       initialisation, which the ppx marks as a global context. *)
    Eliom.Syntax.set_global true;
    Fun.protect
      ~finally:(fun () -> Eliom.Syntax.set_global false)
      (fun () ->
         Service.preapply
           ~service:(extern Parameter.(int "i" ** string "s"))
           (1, "x"))
  in
  check "preapplied"
    ("http://example.org/a/b", ["i", "1"; "s", "x"], None)
    (components preapplied ());
  let nl =
    Parameter.make_non_localized_parameters ~prefix:"app" ~name:"n"
      (Parameter.int "i")
  in
  check "non-localized parameters"
    ("http://example.org/a/b", ["__nl_n_app-n.i", "7"], None)
    (components
       ~nl_params:
         (Parameter.add_nl_parameter Parameter.empty_nl_params_set nl 7)
       (extern Parameter.unit) ());
  Alcotest.(check string)
    "string URI" "http://example.org/a/b?i=1#top"
    (Uri.make_string_uri ~https:false ~fragment:"top"
       ~service:(extern (Parameter.int "i"))
       1)

let suite =
  ( "uri"
  , [ Alcotest.test_case "relative path" `Quick test_relative_path
    ; Alcotest.test_case "actual path" `Quick test_actual_path
    ; Alcotest.test_case "from components" `Quick test_from_components
    ; Alcotest.test_case "protocol prefix" `Quick test_proto_prefix
    ; Alcotest.test_case "external services" `Quick test_extern ] )
