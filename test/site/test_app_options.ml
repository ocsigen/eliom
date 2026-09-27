module Common = Eliom.Common
module State = Eliom.State

let session = Common.default_session_scope

(* The settings of the current site read by the tests *)
type settings =
  { xhr_links : bool
  ; data_timeout : float option
  ; service_timeout : float option
  ; persistent_timeout : float option
  ; max_service_sessions : int
  ; max_data_sessions : int
  ; max_persistent_sessions : int option
  ; max_service_tab_sessions : int
  ; max_data_tab_sessions : int
  ; max_persistent_tab_sessions : int option
  ; max_anonymous_services : int
  ; secure_cookies : bool
  ; ignored_get_params : string list }

let settings () =
  let sitedata = Common.get_current_sitedata () in
  { xhr_links = Eliom.Config.get_default_links_xhr ()
  ; data_timeout =
      State.get_global_volatile_data_state_timeout ~cookie_scope:session ()
  ; service_timeout =
      State.get_global_service_state_timeout ~cookie_scope:session ()
  ; persistent_timeout =
      State.get_global_persistent_data_state_timeout ~cookie_scope:session ()
  ; max_service_sessions = sitedata.max_service_sessions_per_group.cf_value
  ; max_data_sessions = sitedata.max_volatile_data_sessions_per_group.cf_value
  ; max_persistent_sessions =
      sitedata.max_persistent_data_sessions_per_group.cf_value
  ; max_service_tab_sessions =
      sitedata.max_service_tab_sessions_per_group.cf_value
  ; max_data_tab_sessions =
      sitedata.max_volatile_data_tab_sessions_per_group.cf_value
  ; max_persistent_tab_sessions =
      sitedata.max_persistent_data_tab_sessions_per_group.cf_value
  ; max_anonymous_services =
      sitedata.max_anonymous_services_per_session.cf_value
  ; secure_cookies = sitedata.secure_cookies
  ; ignored_get_params = List.map fst sitedata.ignored_get_params }

let run ~app =
  Eliom.App.run ~app ~xhr_links:false ~data_timeout:(`Session, None, Some 10.)
    ~service_timeout:(`Session, None, Some 20.)
    ~persistent_timeout:(`Session, None, Some 30.)
    ~max_service_sessions_per_group:(3, false)
    ~max_volatile_data_sessions_per_group:(4, false)
    ~max_persistent_data_sessions_per_group:5
    ~max_service_tab_sessions_per_group:(6, false)
    ~max_volatile_data_tab_sessions_per_group:(7, false)
    ~max_persistent_data_tab_sessions_per_group:8
    ~max_anonymous_services_per_session:(9, false) ~secure_cookies:true
    ~ignored_get_params:("utm", Re.Posix.compile_pat "utm_.*")
    ()

let test_options () =
  let s = Site.init ~run ~app:"options" settings in
  Alcotest.(check bool) "XHR links" false s.xhr_links;
  Alcotest.(check (option (float 0.))) "data timeout" (Some 10.) s.data_timeout;
  Alcotest.(check (option (float 0.)))
    "service timeout" (Some 20.) s.service_timeout;
  Alcotest.(check (option (float 0.)))
    "persistent timeout" (Some 30.) s.persistent_timeout;
  Alcotest.(check int) "service sessions" 3 s.max_service_sessions;
  Alcotest.(check int) "data sessions" 4 s.max_data_sessions;
  Alcotest.(check (option int))
    "persistent sessions" (Some 5) s.max_persistent_sessions;
  Alcotest.(check int) "service tab sessions" 6 s.max_service_tab_sessions;
  Alcotest.(check int) "data tab sessions" 7 s.max_data_tab_sessions;
  Alcotest.(check (option int))
    "persistent tab sessions" (Some 8) s.max_persistent_tab_sessions;
  Alcotest.(check int) "anonymous services" 9 s.max_anonymous_services;
  Alcotest.(check bool) "secure cookies" true s.secure_cookies;
  Alcotest.(check (list string))
    "ignored GET parameters" ["utm"] s.ignored_get_params

let test_other_site () =
  (* The options of a site do not change the other sites. *)
  ignore (Site.init ~run ~app:"options, first site" settings : settings);
  let s = Site.init ~app:"options, other site" settings in
  Alcotest.(check bool) "XHR links" true s.xhr_links;
  (* The default timeout, one hour *)
  Alcotest.(check (option (float 0.)))
    "data timeout" (Some 3600.) s.data_timeout;
  Alcotest.(check bool) "secure cookies" false s.secure_cookies;
  Alcotest.(check (list string))
    "ignored GET parameters" [] s.ignored_get_params

let test_configuration_file () =
  (* A value given with [true], as by the configuration file, is only
     changed by the program when asked to. *)
  let run ~app =
    Eliom.App.run ~app ~max_service_sessions_per_group:(3, true) ()
  in
  let kept, overridden =
    Site.init ~run ~app:"options, configuration file" (fun () ->
      State.set_default_max_service_sessions_per_group 5;
      let kept = (settings ()).max_service_sessions in
      State.set_default_max_service_sessions_per_group ~override_configfile:true
        5;
      kept, (settings ()).max_service_sessions)
  in
  Alcotest.(check int) "kept" 3 kept;
  Alcotest.(check int) "overridden" 5 overridden

let suite =
  ( "options of App.run"
  , [ Alcotest.test_case "options" `Quick test_options
    ; Alcotest.test_case "other site" `Quick test_other_site
    ; Alcotest.test_case "configuration file" `Quick test_configuration_file ] )
