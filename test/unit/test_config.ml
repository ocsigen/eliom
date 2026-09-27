module Main = Eliom.Mod_main
module Gc = Eliom.Mod_gc

(* [parse s] reads the options [s], the content of an <eliom> element. *)
let parse s =
  match Xml.parse_string ("<eliom>" ^ s ^ "</eliom>") with
  | Xml.Element (_, _, options) -> Main.parse_global_config options
  | Xml.PCData _ -> Alcotest.fail "not an element"

let check_error msg expected s =
  match parse s with
  | () -> Alcotest.failf "%s: accepted" msg
  | exception Ocsigen.Extensions.Error_in_config_file e ->
      Alcotest.(check string) msg expected e

let frequency = Alcotest.(option (float 0.))

let test_gc_frequencies () =
  parse {|<sessiongcfrequency value="10"/>|};
  Alcotest.check frequency "service" (Some 10.)
    (Gc.get_servicesessiongcfrequency ());
  Alcotest.check frequency "data" (Some 10.) (Gc.get_datasessiongcfrequency ());
  parse
    {|<servicesessiongcfrequency value="infinity"/>
      <datasessiongcfrequency value="2.5"/>
      <persistentsessiongcfrequency value="3600"/>|};
  Alcotest.check frequency "service only" None
    (Gc.get_servicesessiongcfrequency ());
  Alcotest.check frequency "data only" (Some 2.5)
    (Gc.get_datasessiongcfrequency ());
  Alcotest.check frequency "persistent" (Some 3600.)
    (Gc.get_persistentsessiongcfrequency ());
  check_error "not a number" "Eliom: Wrong value for <sessiongcfrequency>"
    {|<sessiongcfrequency value="often"/>|}

let test_comet_timeout () =
  parse {|<comettimeout value="2.5"/>|};
  Alcotest.(check (float 0.)) "set" 2.5 (Main.get_comet_timeout ());
  List.iter
    (fun v ->
       check_error v "Eliom: Wrong value for <comettimeout>"
         (Printf.sprintf {|<comettimeout value="%s"/>|} v))
    ["soon"; "0"; "-1"; "infinity"; "nan"];
  Alcotest.(check (float 0.)) "unchanged" 2.5 (Main.get_comet_timeout ());
  Alcotest.check_raises "not positive"
    (Invalid_argument "Eliom: the Comet timeout must be a positive number")
    (fun () -> Main.set_comet_timeout 0.);
  Main.set_comet_timeout 20.

(* The limits, by name *)
let limits =
  Main.
    [ "service sessions per group", default_max_service_sessions_per_group
    ; "data sessions per group", default_max_volatile_data_sessions_per_group
    ; "service sessions per subnet", default_max_service_sessions_per_subnet
    ; "data sessions per subnet", default_max_volatile_data_sessions_per_subnet
    ; ( "persistent sessions per group"
      , default_max_persistent_data_sessions_per_group )
    ; ( "service tab sessions per group"
      , default_max_service_tab_sessions_per_group )
    ; ( "data tab sessions per group"
      , default_max_volatile_data_tab_sessions_per_group )
    ; ( "persistent tab sessions per group"
      , default_max_persistent_data_tab_sessions_per_group )
    ; "coservices per session", default_max_anonymous_services_per_session
    ; "coservices per subnet", default_max_anonymous_services_per_subnet
    ; "groups per site", default_max_volatile_groups_per_site ]

(* The options setting limits, each with the limits it sets *)
let limit_options =
  [ ( "maxvolatilesessionspergroup"
    , ["service sessions per group"; "data sessions per group"] )
  ; "maxservicesessionspergroup", ["service sessions per group"]
  ; "maxdatasessionspergroup", ["data sessions per group"]
  ; ( "maxvolatilesessionspersubnet"
    , ["service sessions per subnet"; "data sessions per subnet"] )
  ; "maxservicesessionspersubnet", ["service sessions per subnet"]
  ; "maxdatasessionspersubnet", ["data sessions per subnet"]
  ; "maxpersistentsessionspergroup", ["persistent sessions per group"]
  ; ( "maxvolatiletabsessionspergroup"
    , ["service tab sessions per group"; "data tab sessions per group"] )
  ; "maxservicetabsessionspergroup", ["service tab sessions per group"]
  ; "maxdatatabsessionspergroup", ["data tab sessions per group"]
  ; "maxpersistenttabsessionspergroup", ["persistent tab sessions per group"]
  ; "maxanonymouscoservicespersession", ["coservices per session"]
  ; "maxanonymouscoservicespersubnet", ["coservices per subnet"]
  ; "maxvolatilegroupspersite", ["groups per site"] ]

let test_limits () =
  (* Each option sets its limits, and no other one. *)
  List.iteri
    (fun i (tag, set) ->
       let value = 100 + i in
       let before = List.map (fun (_, r) -> !r) limits in
       parse (Printf.sprintf {|<%s value="%d"/>|} tag value);
       List.iter2
         (fun (name, r) old ->
            let expected = if List.mem name set then value else old in
            Alcotest.(check int) (tag ^ ": " ^ name) expected !r)
         limits before)
    limit_options;
  check_error "not an integer"
    "Eliom: Wrong attribute value for maxdatasessionspergroup tag"
    {|<maxdatasessionspergroup value="many"/>|}

let test_cookies_and_subnets () =
  parse {|<securecookies value="true"/><ipv4subnetmask value="16"/>|};
  Alcotest.(check bool) "secure cookies" true !Main.default_secure_cookies;
  Alcotest.(check int) "IPv4 mask" 16 !Eliom.Common.ipv4mask;
  parse {|<securecookies value="false"/><ipv6subnetmask value="48"/>|};
  Alcotest.(check bool) "insecure cookies" false !Main.default_secure_cookies;
  Alcotest.(check int) "IPv6 mask" 48 !Eliom.Common.ipv6mask;
  check_error "not a boolean"
    "Eliom: Wrong attribute value for securecookies tag"
    {|<securecookies value="yes"/>|}

let test_client_program () =
  parse
    {|<applicationscript defer="true" async="false"/><wasm enabled="true"/>|};
  let {Eliom.Common.defer; async} = !Main.default_application_script in
  Alcotest.(check (pair bool bool)) "script" (true, false) (defer, async);
  Alcotest.(check bool) "wasm" true !Main.default_enable_wasm;
  parse {|<cacheglobaldata path="gd/data" cache="60"/>|};
  Alcotest.(check (option (pair (list string) int)))
    "global data caching"
    (Some (["gd"; "data"], 60))
    !Main.default_cache_global_data;
  parse {|<htmlcontenttype value="text/html"/>|};
  Alcotest.(check (option string))
    "HTML content type" (Some "text/html")
    !Main.default_html_content_type;
  check_error "wrong script attribute"
    "Eliom: attribute src not allowed in element applicationscript"
    {|<applicationscript src="a.js"/>|};
  check_error "wrong boolean"
    "Eliom: Wrong attribute value for tag defer in element applicationscript"
    {|<applicationscript defer="maybe"/>|};
  check_error "wrong cache duration"
    "Eliom: Wrong attribute value for tag cache in element cacheglobaldata"
    {|<cacheglobaldata cache="long"/>|};
  check_error "wrong wasm attribute" "Eliom: Wrong attribute value for wasm tag"
    {|<wasm enabled="maybe"/>|}

let test_ignored_params () =
  Main.default_ignored_get_params := [];
  Main.default_ignored_post_params := [];
  parse {|<ignoredgetparams regexp="utm_.*"/><ignoredpostparams regexp="x"/>|};
  match !Main.default_ignored_get_params, !Main.default_ignored_post_params with
  | [(s, re)], [(s', re')] ->
      Alcotest.(check string) "source" "utm_.*" s;
      Alcotest.(check bool) "matches" true (Re.execp re "utm_source");
      Alcotest.(check bool) "whole name" false (Re.execp re "a_utm_source");
      Alcotest.(check string) "POST source" "x" s';
      Alcotest.(check bool) "POST whole name" false (Re.execp re' "xx")
  | _ -> Alcotest.fail "ignored parameters not recorded"

let test_omit_persistent_storage () =
  parse
    {|<omitpersistentstorage><header user-agent=".*bot.*"/></omitpersistentstorage>|};
  (match !Main.default_omitpersistentstorage with
  | Some [Eliom.Common.HeaderRule (name, re)] ->
      Alcotest.(check string)
        "header" "user-agent"
        (Ocsigen_http.Header.Name.to_string name);
      Alcotest.(check bool) "regexp" true (Re.execp re "Googlebot/2.1")
  | _ -> Alcotest.fail "rules not recorded");
  check_error "wrong rule"
    "Eliom: <omitpersistentstorage> only accepts <header HEADER-NAME=\"REGEXP\"/> elements"
    {|<omitpersistentstorage><cookie name="x"/></omitpersistentstorage>|}

let test_timeout_errors () =
  check_error "missing value" "Eliom: Missing value for datatimeout tag"
    {|<datatimeout level="session"/>|};
  check_error "wrong value"
    "Eliom: Wrong attribute value for servicetimeout tag"
    {|<servicetimeout value="soon"/>|};
  check_error "wrong level"
    "Eliom: Wrong attribute value for level in volatiletimeout tag"
    {|<volatiletimeout value="10" level="site"/>|};
  check_error "wrong attribute"
    "Eliom: Wrong attribute name for persistenttimeout tag"
    {|<persistenttimeout value="10" scope="session"/>|}

let test_unknown () =
  check_error "unknown option"
    "Unexpected content <sessions> inside eliom config" {|<sessions/>|};
  check_error "text" "Unexpected content inside eliom config" "text"

let suite =
  ( "config"
  , [ Alcotest.test_case "GC frequencies" `Quick test_gc_frequencies
    ; Alcotest.test_case "Comet timeout" `Quick test_comet_timeout
    ; Alcotest.test_case "limits" `Quick test_limits
    ; Alcotest.test_case "cookies and subnets" `Quick test_cookies_and_subnets
    ; Alcotest.test_case "client program" `Quick test_client_program
    ; Alcotest.test_case "ignored parameters" `Quick test_ignored_params
    ; Alcotest.test_case "omit persistent storage" `Quick
        test_omit_persistent_storage
    ; Alcotest.test_case "timeout errors" `Quick test_timeout_errors
    ; Alcotest.test_case "unknown options" `Quick test_unknown ] )
