module Html = Eliom.Content.Html

let print elt = Format.asprintf "%a" (Html.Printer.pp_elt ()) elt

let print_doc doc =
  Format.asprintf "%a" (Eliom.Content_core.Html.Printer.pp ()) doc

let contains s sub =
  let n = String.length sub in
  let rec loop i =
    i + n <= String.length s && (String.sub s i n = sub || loop (i + 1))
  in
  loop 0

let check_contains msg s sub =
  if not (contains s sub) then Alcotest.failf "%s: %S not in %S" msg sub s

let check_absent msg s sub =
  if contains s sub then Alcotest.failf "%s: %S in %S" msg sub s

(* [ids s] are the values of the node identifier attributes in [s]. *)
let ids s =
  let attr = Eliom.Runtime.RawXML.node_id_attrib ^ "=\"" in
  let n = String.length attr in
  let rec loop i acc =
    if i + n > String.length s
    then List.rev acc
    else if String.sub s i n = attr
    then
      let j = String.index_from s (i + n) '"' in
      loop j (String.sub s (i + n) (j - i - n) :: acc)
    else loop (i + 1) acc
  in
  loop 0 []

let test_functional_nodes () =
  Alcotest.(check string)
    "no identifier" "<div class=\"c\"><p>a</p></div>"
    (print Html.F.(div ~a:[a_class ["c"]] [p [txt "a"]]))

let test_escaping () =
  let s =
    print
      Html.F.(
        div ~a:[a_title "\"><script>x</script>"] [txt "<script>&</script>"])
  in
  check_absent "text" s "<script>";
  check_contains "escaped text" s "&lt;script&gt;&amp;&lt;/script&gt;";
  check_absent "attribute" s "\"><"

let test_dom_nodes () =
  let a = Html.D.div [] and b = Html.D.div [] in
  let ids_a = ids (print a) and ids_b = ids (print b) in
  Alcotest.(check int) "one identifier" 1 (List.length ids_a);
  Alcotest.(check (list string)) "stable" ids_a (ids (print a));
  Alcotest.(check bool) "distinct" true (ids_a <> ids_b);
  Alcotest.(check (list string))
    "no identifier for functional nodes" []
    (ids (print (Html.F.div [])))

let test_named_nodes () =
  let id = Html.Id.new_elt_id () in
  let a = Html.Id.create_named_elt ~id (Html.F.div []) in
  let b = Html.Id.create_named_elt ~id (Html.F.span []) in
  Alcotest.(check bool) "has the id" true (Html.Id.have_id id a);
  Alcotest.(check bool)
    "other element" false
    (Html.Id.have_id (Html.Id.new_elt_id ()) a);
  Alcotest.(check (list string))
    "same identifier"
    (ids (print a))
    (ids (print b));
  let g = Html.Id.create_global_elt (Html.F.div []) in
  Alcotest.(check int) "global element" 1 (List.length (ids (print g)))

let test_custom_data () =
  let open Html.Custom_data in
  let d =
    create ~name:"count" ~to_string:string_of_int ~of_string:int_of_string ()
  in
  check_contains "string conversion"
    (print (Html.F.div ~a:[attrib d 42] []))
    "data-count=\"42\"";
  let j = create_json ~name:"pair" [%json: int * string] in
  let s = print (Html.F.div ~a:[attrib j (1, "<a\"")] []) in
  check_contains "JSON" s "data-pair=\"";
  check_absent "JSON escaped" s "<a"

(* Error pages show names and values of parameters sent by the client. *)

let evil = "<script>alert(1)</script>"

let test_error_param_type () =
  let s =
    print_doc
      (Eliom.Error_pages.page_error_param_type
         [evil, Failure "x"; "j", Failure "y"])
  in
  check_contains "names" s "<em>j</em>";
  check_absent "escaped" s "<script>";
  check_contains "escaped name" s
    "<em>&lt;script&gt;alert(1)&lt;/script&gt;</em>"

let test_bad_param () =
  let page () =
    print_doc
      (Eliom.Error_pages.page_bad_param false [evil, evil; "i", "1"] [evil])
  in
  let debugmode = Ocsigen.Config.get_debugmode () in
  Fun.protect
    ~finally:(fun () -> Ocsigen.Config.set_debugmode debugmode)
    (fun () ->
       Ocsigen.Config.set_debugmode false;
       check_absent "no details" (page ()) "i=1";
       Ocsigen.Config.set_debugmode true;
       let s = page () in
       check_contains "details" s "i=1";
       check_absent "escaped" s "<script>")

let suite =
  ( "content"
  , [ Alcotest.test_case "functional nodes" `Quick test_functional_nodes
    ; Alcotest.test_case "escaping" `Quick test_escaping
    ; Alcotest.test_case "DOM nodes" `Quick test_dom_nodes
    ; Alcotest.test_case "named nodes" `Quick test_named_nodes
    ; Alcotest.test_case "custom data" `Quick test_custom_data
    ; Alcotest.test_case "wrong parameter type page" `Quick
        test_error_param_type
    ; Alcotest.test_case "wrong parameters page" `Quick test_bad_param ] )
