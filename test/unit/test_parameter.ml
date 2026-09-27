(* The tests go through [Eliom.Parameter_base], whose parameter types are
   concrete: [Eliom.Parameter.reconstruct_params] needs a request, while
   [reconstruct_params_] does the same decoding from explicit lists. *)

open Eliom.Parameter_base
module Common = Eliom.Common

let no_nl = Eliom.Lib.String.Table.empty
let params = Alcotest.(list (pair string string))
let sorted l = List.sort compare l

(* [encode typ v] is the URL suffix and the sorted parameters of [v]. *)
let encode typ v =
  let suffix, l = construct_params_list no_nl typ v in
  suffix, sorted l

let decode ?suffix ?(nosuffix = false) typ l =
  reconstruct_params_ typ l [] nosuffix suffix

let round_trip typ v =
  let suffix, l = construct_params_list no_nl typ v in
  decode ?suffix typ l

let check_params msg expected (typ, v) =
  Alcotest.check params msg (sorted expected) (snd (encode typ v))

let check_round_trip ty msg typ v = Alcotest.check ty msg v (round_trip typ v)

let check_wrong_parameter msg f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Eliom_Wrong_parameter" msg
  | exception Common.Eliom_Wrong_parameter -> ()

(* [check_typing_error msg names f] checks that [f ()] fails on the
   parameters [names] (in any order). *)
let check_typing_error msg names f =
  match f () with
  | _ -> Alcotest.failf "%s: expected Eliom_Typing_Error" msg
  | exception Common.Eliom_Typing_Error errors ->
      Alcotest.(check (list string))
        msg (List.sort compare names)
        (List.sort compare (List.map fst errors))

let point =
  Alcotest.testable
    (fun fmt {abscissa; ordinate} ->
       Format.fprintf fmt "(%d, %d)" abscissa ordinate)
    ( = )

let binsum a b =
  Alcotest.testable
    (fun fmt -> function
       | Inj1 x -> Format.fprintf fmt "Inj1 %a" (Alcotest.pp a) x
       | Inj2 y -> Format.fprintf fmt "Inj2 %a" (Alcotest.pp b) y)
    (fun x y ->
       match x, y with
       | Inj1 x, Inj1 y -> Alcotest.equal a x y
       | Inj2 x, Inj2 y -> Alcotest.equal b x y
       | _ -> false)

(* Encoding *)

let test_encode_atoms () =
  check_params "int" ["i", "42"] (int "i", 42);
  check_params "negative int" ["i", "-3"] (int "i", -3);
  check_params "int32" ["i", "2147483647"] (int32 "i", Int32.max_int);
  check_params "int64" ["i", "9223372036854775807"] (int64 "i", Int64.max_int);
  check_params "string" ["s", "a b&c"] (string "s", "a b&c");
  check_params "true bool" ["b", "on"] (bool "b", true);
  check_params "false bool is absent" [] (bool "b", false);
  check_params "unit" [] (unit, ())

let test_encode_compound () =
  check_params "product" ["a", "1"; "b", "x"] (int "a" ** string "b", (1, "x"));
  check_params "missing option" [] (opt (int "i"), None);
  check_params "present option" ["i", "3"] (opt (int "i"), Some 3);
  check_params "set"
    ["i", "4"; "i", "22"; "i", "111"]
    (set int "i", [4; 22; 111]);
  check_params "empty set" [] (set int "i", []);
  check_params "list"
    ["l.x[0]", "1"; "l.y[0]", "a"; "l.x[1]", "2"; "l.y[1]", "b"]
    (list "l" (int "x" ** string "y"), [1, "a"; 2, "b"]);
  check_params "first case of a sum"
    ["a", "1"]
    (sum (int "a") (string "b"), Inj1 1);
  check_params "second case of a sum"
    ["b", "x"]
    (sum (int "a") (string "b"), Inj2 "x");
  check_params "coordinates"
    ["c.x", "3"; "c.y", "4"]
    (coordinates "c", {abscissa = 3; ordinate = 4});
  check_params "any" ["u", "1"; "v", "2"] (any, ["u", "1"; "v", "2"])

let test_encode_suffix () =
  let check msg expected typ v =
    Alcotest.(check (pair (option (list string)) params))
      msg expected (encode typ v)
  in
  check "suffix"
    (Some ["380"; "yo"], [])
    (suffix (int "i" ** string "s"))
    (380, "yo");
  check "suffix and parameters"
    (Some ["777"; "go"; "go"], ["i", "320"])
    (suffix_prod (int "suff" ** all_suffix "endsuff") (int "i"))
    ((777, ["go"; "go"]), 320);
  check "constant in suffix"
    (Some ["a"; "const"; "2"], [])
    (suffix (string "x" ** suffix_const "const" ** int "y"))
    ("a", ((), 2));
  check "no suffix" (None, ["i", "1"]) (int "i") 1

let test_encode_query_string () =
  (* Form encoding: spaces become [+]. *)
  Alcotest.(check (pair (option (list string)) string))
    "escaped query string" (None, "s=a+b%26c%3Dd%2B")
    (construct_params no_nl (string "s") "a b&c=d+")

(* Decoding *)

let test_round_trip () =
  check_round_trip Alcotest.int "int" (int "i") (-12);
  check_round_trip Alcotest.int32 "int32" (int32 "i") Int32.min_int;
  check_round_trip Alcotest.int64 "int64" (int64 "i") Int64.min_int;
  check_round_trip (Alcotest.float 0.) "float" (float "f") 3.25;
  check_round_trip Alcotest.string "string" (string "s") "a b&c=d/é?";
  check_round_trip Alcotest.bool "true" (bool "b") true;
  check_round_trip Alcotest.bool "false" (bool "b") false;
  check_round_trip
    Alcotest.(pair int (pair string bool))
    "product"
    (int "a" ** string "b" ** bool "c")
    (1, ("x", true));
  check_round_trip Alcotest.(option int) "missing option" (opt (int "i")) None;
  check_round_trip
    Alcotest.(option int)
    "present option"
    (opt (int "i"))
    (Some 5);
  check_round_trip
    Alcotest.(list (pair int string))
    "list"
    (list "l" (int "x" ** string "y"))
    [1, "a"; 2, "b"; 3, "c"];
  check_round_trip Alcotest.(list int) "empty list" (list "l" (int "x")) [];
  check_round_trip
    Alcotest.(list (list int))
    "nested lists"
    (list "l" (list "m" (int "x")))
    [[1; 2]; []; [3]];
  check_round_trip point "coordinates" (coordinates "c")
    {abscissa = -1; ordinate = 7};
  check_round_trip
    (binsum Alcotest.int Alcotest.string)
    "first case of a sum"
    (sum (int "a") (string "b"))
    (Inj1 1);
  check_round_trip
    (binsum Alcotest.int Alcotest.string)
    "second case of a sum"
    (sum (int "a") (string "b"))
    (Inj2 "x");
  check_round_trip
    Alcotest.(pair int string)
    "suffix"
    (suffix (int "i" ** string "s"))
    (380, "yo");
  check_round_trip
    Alcotest.(pair (pair int (list string)) int)
    "suffix and parameters"
    (suffix_prod (int "suff" ** all_suffix "endsuff") (int "i"))
    ((777, ["go"; "go"; "go"]), 320)

let test_decode_set () =
  (* The order of a set is unspecified. *)
  Alcotest.(check (list int))
    "set" [4; 22; 111]
    (List.sort compare (decode (set int "i") ["i", "111"; "i", "4"; "i", "22"]));
  Alcotest.(check (list int)) "empty set" [] (decode (set int "i") [])

let test_decode_optional () =
  Alcotest.(check bool) "absent bool" false (decode (bool "b") []);
  Alcotest.(check bool)
    "bool with any value" true
    (decode (bool "b") ["b", "x"]);
  Alcotest.(check (option int))
    "empty int" None
    (decode (opt (int "i")) ["i", ""]);
  Alcotest.(check (option string))
    "empty string with opt" (Some "")
    (decode (opt (string "s")) ["s", ""]);
  Alcotest.(check (option string))
    "empty string with neopt" None
    (decode (neopt (string "s")) ["s", ""]);
  Alcotest.(check (option string))
    "non-empty string with neopt" (Some "x")
    (decode (neopt (string "s")) ["s", "x"])

let test_decode_errors () =
  check_wrong_parameter "missing parameter" (fun () -> decode (int "i") []);
  check_wrong_parameter "unexpected parameter" (fun () ->
    decode (int "i") ["i", "1"; "j", "2"]);
  check_wrong_parameter "unit with a parameter" (fun () ->
    decode unit ["i", "1"]);
  check_typing_error "not an int" ["i"] (fun () -> decode (int "i") ["i", "x"]);
  check_typing_error "all the errors of a product" ["a"; "b"] (fun () ->
    decode (int "a" ** float "b") ["a", "x"; "b", "y"]);
  check_typing_error "error in a list" ["l.x[1]"] (fun () ->
    decode (list "l" (int "x")) ["l.x[0]", "1"; "l.x[1]", "z"]);
  check_typing_error "rejected by the type checker" ["<type_check>"] (fun () ->
    decode
      (type_checker (fun i -> if i < 0 then failwith "negative") (int "i"))
      ["i", "-1"])

let test_decode_suffix () =
  let typ = suffix (int "i" ** string "s") in
  Alcotest.(check (pair int string))
    "from the URL" (3, "x")
    (decode ~suffix:["3"; "x"] typ []);
  check_wrong_parameter "suffix too short" (fun () ->
    decode ~suffix:["3"] typ []);
  check_wrong_parameter "suffix too long" (fun () ->
    decode ~suffix:["3"; "x"; "y"] typ []);
  check_typing_error "not an int" ["<suffix>"] (fun () ->
    decode ~suffix:["x"; "x"] typ []);
  check_wrong_parameter "wrong constant" (fun () ->
    decode ~suffix:["a"; "other"; "2"]
      (suffix (string "x" ** suffix_const "const" ** int "y"))
      []);
  Alcotest.(check (pair int string))
    "version without suffix" (3, "x")
    (decode ~nosuffix:true typ ["i", "3"; "s", "x"]);
  check_wrong_parameter "no suffix" (fun () -> decode typ ["i", "3"; "s", "x"])

let test_decode_whole_suffix_parameter () =
  (* In the version of a service without suffix, the whole suffix is a
     parameter, without ".." segments as well. *)
  Alcotest.(check (list string))
    "list" ["a"; "b"]
    (decode ~nosuffix:true (suffix (all_suffix "p")) ["p", "../a/../b"]);
  Alcotest.(check string)
    "string" "a/b"
    (decode ~nosuffix:true (suffix (all_suffix_string "p")) ["p", "../a/b"])

(* Names *)

let test_names () =
  Alcotest.(check (pair bool (pair string string)))
    "product"
    (false, ("a", "b"))
    (make_params_names (int "a" ** opt (string "b")));
  Alcotest.(check bool)
    "suffix" true
    (fst (make_params_names (suffix (int "i"))));
  let _, names = make_params_names (list "l" (int "x" ** string "y")) in
  Alcotest.(check (list (pair string string)))
    "list iterator"
    ["l.x[0]", "l.y[0]"; "l.x[1]", "l.y[1]"]
    (names.it (fun (x, y) () acc -> (x, y) :: acc) [(); ()] []);
  Alcotest.(check (pair string string))
    "prefix" ("p.a", "p.b")
    (snd (make_params_names (add_pref_params "p." (int "a" ** string "b"))))

(* Non-localised parameters *)

let test_non_localized () =
  let nl = make_non_localized_parameters ~prefix:"app" ~name:"n" (int "i") in
  let pnl =
    make_non_localized_parameters ~prefix:"app" ~name:"p" ~persistent:true
      (string "s")
  in
  Alcotest.(check string) "name" "__nl_n_app-n.i" (get_nl_params_names nl);
  Alcotest.(check string)
    "persistent name" "__nl_p_app-p.s" (get_nl_params_names pnl);
  let set =
    add_nl_parameter (add_nl_parameter empty_nl_params_set nl 1) pnl "x"
  in
  Alcotest.check params "set"
    ["__nl_n_app-n.i", "1"; "__nl_p_app-p.s", "x"]
    (sorted (list_of_nl_params_set set));
  Alcotest.check params "with service parameters"
    ["__nl_n_app-n.i", "2"; "a", "1"]
    (snd (encode (nl_prod (int "a") nl) (1, 2)));
  Alcotest.check_raises "dot in the name"
    (Failure "Non localized parameters names cannot contain dots.") (fun () ->
    ignore (make_non_localized_parameters ~prefix:"app" ~name:"a.b" (int "i")))

let suite =
  ( "parameter"
  , [ Alcotest.test_case "encode atoms" `Quick test_encode_atoms
    ; Alcotest.test_case "encode compound" `Quick test_encode_compound
    ; Alcotest.test_case "encode suffix" `Quick test_encode_suffix
    ; Alcotest.test_case "encode query string" `Quick test_encode_query_string
    ; Alcotest.test_case "round trip" `Quick test_round_trip
    ; Alcotest.test_case "decode set" `Quick test_decode_set
    ; Alcotest.test_case "decode optional" `Quick test_decode_optional
    ; Alcotest.test_case "decode errors" `Quick test_decode_errors
    ; Alcotest.test_case "decode suffix" `Quick test_decode_suffix
    ; Alcotest.test_case "decode whole suffix parameter" `Quick
        test_decode_whole_suffix_parameter
    ; Alcotest.test_case "names" `Quick test_names
    ; Alcotest.test_case "non-localized" `Quick test_non_localized ] )
