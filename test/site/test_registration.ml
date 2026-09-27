module Service = Eliom.Service
module Parameter = Eliom.Parameter
module Html_text = Eliom.Registration.Html_text

let service path =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get Parameter.unit) ()

let coservice name =
  Service.create ~name ~path:Service.No_path ~meth:(Service.Get Parameter.unit)
    ()

let register service = Html_text.register ~service (fun _ _ -> Lwt.return "")

(* [warnings f] is the result of [f ()] and the warnings logged meanwhile. *)
let warnings f =
  let logged = ref [] in
  let report _src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
      Format.kasprintf
        (fun s ->
           if level = Logs.Warning then logged := s :: !logged;
           over ();
           k ())
        fmt)
  in
  let reporter = Logs.reporter () in
  Logs.set_reporter {Logs.report};
  Fun.protect
    ~finally:(fun () -> Logs.set_reporter reporter)
    (fun () ->
       let r = f () in
       r, List.rev !logged)

let test_all_registered () =
  let (), logged =
    warnings (fun () ->
      Site.init ~site_dir:["s"] ~app:"registered" (fun () ->
        register (service ["a"]);
        register (service ["b"; "c"]);
        register (coservice "na")))
  in
  Alcotest.(check (list string)) "no warning" [] logged

let test_unregistered () =
  match
    Site.init ~site_dir:["s"] ~app:"unregistered" (fun () ->
      register (service ["a"]);
      ignore (service ["b"; "c"]);
      ignore (coservice "na"))
  with
  | () -> Alcotest.fail "unregistered services accepted"
  | exception Eliom.Common.Eliom_there_are_unregistered_services (site, l, na)
    ->
      Alcotest.(check (list string)) "site" ["s"] site;
      Alcotest.(check (list (list string))) "services" [["b"; "c"]] l;
      Alcotest.(check bool)
        "non-attached coservices" true
        (na = [Eliom.Common.SNa_get_ "na"])

let test_unregistered_non_attached () =
  (* Libraries create non-attached coservices that applications may not use:
     they are only reported. *)
  let (), logged =
    warnings (fun () ->
      Site.init ~site_dir:["s"] ~app:"unregistered-na" (fun () ->
        register (coservice "used");
        ignore (coservice "unused")))
  in
  Alcotest.(check (list string))
    "warning"
    [ "In site /s - The non-attached GET service \"unused\" has not been registered."
    ]
    logged

let test_buses () =
  (* The service of a bus of client processes is registered for each of them,
     during requests. *)
  let (), logged =
    warnings (fun () ->
      Site.init ~app:"buses" (fun () ->
        ignore (Eliom.Bus.create [%json: int]);
        ignore
          (Eliom.Bus.create ~scope:Eliom.Common.default_process_scope
             [%json: int])))
  in
  Alcotest.(check (list string)) "no warning" [] logged

let test_registered_twice () =
  match
    Site.init ~app:"twice" (fun () ->
      let s = service ["a"] in
      register s; register s)
  with
  | () -> Alcotest.fail "registered twice"
  | exception Eliom.Common.Eliom_duplicate_registration path ->
      Alcotest.(check string) "path" "a" path

let test_duplicates () =
  (* [check msg expected f] checks that the registrations of [f] fail with
     [Eliom_duplicate_registration expected], or are accepted if [expected]
     is [None]. *)
  let check msg expected f =
    match Site.init ~app:("duplicates, " ^ msg) f with
    | () -> Alcotest.(check (option string)) msg expected None
    | exception Eliom.Common.Eliom_duplicate_registration s ->
        Alcotest.(check (option string)) msg expected (Some s)
  in
  let get path params =
    Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()
  in
  check "same path and parameters" (Some "a") (fun () ->
    register (service ["a"]);
    register (service ["a"]));
  check "same path, other parameters" None (fun () ->
    register (get ["a"] (Parameter.int "i"));
    register (get ["a"] (Parameter.string "s")));
  check "non-attached coservices with the same name"
    (Some "GET non-attached service na") (fun () ->
    register (coservice "na");
    register (coservice "na"));
  check "anonymous non-attached coservices" None (fun () ->
    let anonymous () =
      Service.create ~path:Service.No_path ~meth:(Service.Get Parameter.unit) ()
    in
    register (anonymous ());
    register (anonymous ()));
  check "anonymous attached coservices" None (fun () ->
    let fallback = service ["a"] in
    let anonymous () =
      Service.create_attached_get ~fallback ~get_params:Parameter.unit ()
    in
    register fallback;
    register (anonymous ());
    register (anonymous ()))

let test_page_erasing () =
  (* A path cannot be both a page and a directory. *)
  let check msg first second =
    match
      Site.init ~app:("erasing, " ^ msg) (fun () ->
        register (service first);
        register (service second))
    with
    | () -> Alcotest.failf "%s: accepted" msg
    | exception Eliom.Common.Eliom_page_erasing s ->
        Alcotest.(check string) msg "a" s
  in
  check "page, then page below" ["a"] ["a"; "b"];
  check "page below, then page" ["a"; "b"] ["a"];
  check "page, then directory" ["a"] ["a"; ""];
  check "directory, then page" ["a"; ""] ["a"]

let test_session_registration () =
  (* Services of a session are registered during a request. *)
  match
    Site.init ~app:"session registration" (fun () ->
      Html_text.register ~scope:Eliom.Common.default_session_scope
        ~service:(service ["a"]) (fun () () -> Lwt.return ""))
  with
  | () -> Alcotest.fail "registered"
  | exception Eliom.Common.Request_information_not_available f ->
      Alcotest.(check string) "function" "Registration.register" f

let test_registration_after_the_initialisation () =
  (* Outside a request, services are registered during the initialisation
     of their site. A non-attached coservice, which may be left
     unregistered, is created during the initialisation and registered
     after it. *)
  let na, (_ : string list) =
    warnings (fun () -> Site.init ~app:"after" (fun () -> coservice "after"))
  in
  match register na with
  | () -> Alcotest.fail "registered"
  | exception Eliom.Common.Site_information_not_available f ->
      Alcotest.(check string) "function" "register" f

let test_current_site_restored () =
  (* Once a site is initialised, even when its check fails, the services
     created afterwards go to the default site of the program again, which
     has no directory, since the tests do not run the default application. *)
  let current_site_dir () = (Eliom.Common.get_current_sitedata ()).site_dir in
  Site.init ~site_dir:["ok"] ~app:"restored-ok" ignore;
  Alcotest.(check (option (list string)))
    "after a site" None (current_site_dir ());
  (match
     Site.init ~site_dir:["failed"] ~app:"restored-failed" (fun () ->
       ignore (service ["a"]))
   with
  | () -> Alcotest.fail "unregistered service accepted"
  | exception Eliom.Common.Eliom_there_are_unregistered_services _ -> ());
  Alcotest.(check (option (list string)))
    "after a failed site" None (current_site_dir ())

let suite =
  ( "registration"
  , [ Alcotest.test_case "all registered" `Quick test_all_registered
    ; Alcotest.test_case "unregistered" `Quick test_unregistered
    ; Alcotest.test_case "unregistered non-attached coservice" `Quick
        test_unregistered_non_attached
    ; Alcotest.test_case "buses" `Quick test_buses
    ; Alcotest.test_case "registered twice" `Quick test_registered_twice
    ; Alcotest.test_case "duplicates" `Quick test_duplicates
    ; Alcotest.test_case "page erasing" `Quick test_page_erasing
    ; Alcotest.test_case "session registration" `Quick test_session_registration
    ; Alcotest.test_case "registration after the initialisation" `Quick
        test_registration_after_the_initialisation
    ; Alcotest.test_case "current site restored" `Quick
        test_current_site_restored ] )
