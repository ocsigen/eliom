module Service = Eliom.Service
module Parameter = Eliom.Parameter
module Html_text = Eliom.Registration.Html_text

let service path =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get Parameter.unit) ()

let coservice name =
  Service.create ~name ~path:Service.No_path ~meth:(Service.Get Parameter.unit)
    ()

let register service = Html_text.register ~service (fun () () -> Lwt.return "")

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

let suite =
  ( "registration"
  , [ Alcotest.test_case "all registered" `Quick test_all_registered
    ; Alcotest.test_case "unregistered" `Quick test_unregistered
    ; Alcotest.test_case "unregistered non-attached coservice" `Quick
        test_unregistered_non_attached
    ; Alcotest.test_case "buses" `Quick test_buses
    ; Alcotest.test_case "registered twice" `Quick test_registered_twice ] )
