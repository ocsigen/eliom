module Service = Eliom.Service
module Parameter = Eliom.Parameter
module Html_text = Eliom.Registration.Html_text

let service path =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get Parameter.unit) ()

let register service = Html_text.register ~service (fun () () -> Lwt.return "")

let test_all_registered () =
  Site.init ~site_dir:["s"] ~app:"registered" (fun () ->
    register (service ["a"]);
    register (service ["b"; "c"]))

let test_unregistered () =
  match
    Site.init ~site_dir:["s"] ~app:"unregistered" (fun () ->
      register (service ["a"]);
      ignore (service ["b"; "c"]))
  with
  | () -> Alcotest.fail "unregistered services accepted"
  | exception Eliom.Common.Eliom_there_are_unregistered_services (site, l, na)
    ->
      Alcotest.(check (list string)) "site" ["s"] site;
      Alcotest.(check (list (list string))) "services" [["b"; "c"]] l;
      Alcotest.(check bool) "non-attached coservices" true (na = [])

let test_unregistered_non_attached () =
  (* Non-attached coservices are not checked. *)
  Site.init ~app:"unregistered-na" (fun () ->
    ignore
      (Service.create ~name:"na" ~path:Service.No_path
         ~meth:(Service.Get Parameter.unit) ()))

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
    ; Alcotest.test_case "registered twice" `Quick test_registered_twice ] )
