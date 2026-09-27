module Common = Eliom.Common
module Cookie_map = Ocsigen_cookie_map

(* Cookie sets, sent to client processes in JSON *)

let cookie_set =
  Alcotest.testable
    (fun fmt set ->
       Cookie_map.Map_path.iter
         (fun path inner ->
            Cookie_map.Map_inner.iter
              (fun name cookie ->
                 Format.fprintf fmt "/%s %s=%s@ " (String.concat "/" path) name
                   (match cookie with
                   | Cookie_map.OSet (exp, v, secure) ->
                       Printf.sprintf "%S (exp: %s, secure: %b)" v
                         (match exp with
                         | None -> "none"
                         | Some e -> Printf.sprintf "%h" e)
                         secure
                   | Cookie_map.OUnset -> "unset"))
              inner)
         set)
    (Cookie_map.Map_path.equal (Cookie_map.Map_inner.equal ( = )))

let test_cookie_set_json () =
  let check msg set =
    Alcotest.check cookie_set msg set
      Eliom.Cookies_base.(cookieset_of_json (cookieset_to_json set))
  in
  check "empty" Cookie_map.empty;
  check "cookies"
    Cookie_map.(
      empty
      |> add ~path:[] "a" (OSet (None, "1", false))
      |> add ~path:[] "b" (OSet (Some 1789.25, "v a|l=u;e", true))
      |> add ~path:["x"; "y"] "a" (OSet (Some 1.5e9, "2", false))
      |> add ~path:["x"] "c" OUnset)

(* Hashed cookie values *)

let hash v = Common.Hashed_cookies.(to_string (hash v))

let test_hashed_cookies () =
  Alcotest.(check string)
    "SHA-256 in base64" "ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0"
    (Common.Hashed_cookies.sha256 "abc");
  (* Only values ending with 'H' are hashed, for compatibility with the
     cookies of older versions. *)
  Alcotest.(check string) "old cookie" "abc" (hash "abc");
  Alcotest.(check string) "empty" "" (hash "");
  Alcotest.(check string)
    "new cookie"
    (Common.Hashed_cookies.sha256 "abcH")
    (hash "abcH")

let test_session_ids () =
  let ids = List.init 100 (fun _ -> Eliom.Mod_cookies.make_new_session_id ()) in
  List.iter
    (fun id ->
       Alcotest.(check bool)
         (Printf.sprintf "%S is hashed" id)
         true
         (hash id <> id))
    ids;
  Alcotest.(check int)
    "distinct" (List.length ids)
    (List.length (List.sort_uniq compare ids))

(* Names of state cookies *)

let hierarchies =
  Eliom.Common_base.
    [User_hier "h"; User_hier "with|bar"; Default_ref_hier; Default_comet_hier]

let site_dirs = [""; "a/b"]

let full_state_names level =
  List.concat_map
    (fun hier ->
       List.concat_map
         (fun site_dir_str ->
            List.map
              (fun secure ->
                 let user_scope =
                   match level with
                   | `Session -> `Session hier
                   | `Client_process -> `Client_process hier
                 in
                 {Common.user_scope; secure; site_dir_str})
              [false; true])
         site_dirs)
    hierarchies

let full_state_names_t = Alcotest.(list (pair string string))

(* [check_names level] checks that the state cookies of [level], named by
   [make_full_cookie_name], are found back with their kind, security and
   scope. *)
let check_names level =
  let fsns = full_state_names level in
  let kinds =
    [ Common.servicecookiename, "service"
    ; Common.datacookiename, "data"
    ; Common.persistentcookiename, "persistent" ]
  in
  let value kind fsn =
    kind ^ ":" ^ Deriving_Json.to_string Common.full_state_name_json fsn
  in
  let cookies =
    List.fold_left
      (fun cookies (prefix, kind) ->
         List.fold_left
           (fun cookies fsn ->
              Cookie_map.Map_inner.add
                (Common.make_full_cookie_name prefix fsn)
                (value kind fsn) cookies)
           cookies fsns)
      Cookie_map.Map_inner.empty kinds
    |> Cookie_map.Map_inner.add "unrelated" "x"
    |> Cookie_map.Map_inner.add (Common.datacookiename ^ "|malformed") "x"
  in
  List.iter
    (fun secure ->
       let state_cookies = Common.get_state_cookies secure level cookies in
       let expected kind =
         List.sort compare
           (List.filter_map
              (fun fsn ->
                 if fsn.Common.secure = secure
                 then
                   Some
                     ( Deriving_Json.to_string Common.full_state_name_json fsn
                     , value kind fsn )
                 else None)
              fsns)
       in
       let actual t =
         List.sort compare
           (List.map
              (fun (fsn, v) ->
                 Deriving_Json.to_string Common.full_state_name_json fsn, v)
              (Common.Full_state_name_table.bindings t))
       in
       let msg kind = Printf.sprintf "%s cookies (secure: %b)" kind secure in
       Alcotest.check full_state_names_t (msg "service") (expected "service")
         (actual state_cookies.service_cookies);
       Alcotest.check full_state_names_t (msg "data") (expected "data")
         (actual state_cookies.data_cookies);
       Alcotest.check full_state_names_t (msg "persistent")
         (expected "persistent")
         (actual state_cookies.persistent_cookies))
    [false; true]

let test_session_cookie_names () = check_names `Session
let test_tab_cookie_names () = check_names `Client_process

let test_scope_hierarchies () =
  Alcotest.(check bool)
    "created" true
    (Common.create_scope_hierarchy "test-hierarchy"
    = Eliom.Common_base.User_hier "test-hierarchy");
  Alcotest.check_raises "created twice"
    (Failure "the scope hierarchy test-hierarchy has already been registered")
    (fun () -> ignore (Common.create_scope_hierarchy "test-hierarchy"))

let suite =
  ( "cookies"
  , [ Alcotest.test_case "cookie set JSON" `Quick test_cookie_set_json
    ; Alcotest.test_case "hashed cookies" `Quick test_hashed_cookies
    ; Alcotest.test_case "session ids" `Quick test_session_ids
    ; Alcotest.test_case "session cookie names" `Quick test_session_cookie_names
    ; Alcotest.test_case "tab cookie names" `Quick test_tab_cookie_names
    ; Alcotest.test_case "scope hierarchies" `Quick test_scope_hierarchies ] )
