module Common = Eliom.Common

(* The messages of the errors of the initialisation of a site, shown by
   Ocsigen Server *)

let check msg expected e =
  Alcotest.(check string) msg expected (Eliom.Mod_main.handle_init_exn e)

let help =
  "\nPlease correct your modules and make sure you have linked in all the modules..."

let test_unregistered () =
  let unregistered site l na =
    Common.Eliom_there_are_unregistered_services (site, l, na)
  in
  check "one service"
    ("Eliom: in site /s - One service or coservice has not been registered on URL /a/b. "
   ^ help)
    (unregistered ["s"] [["a"; "b"]] []);
  check "several services"
    ("Eliom: in site /s/t - Some services or coservices have not been registered on URLs: /a, /b. "
   ^ help)
    (unregistered ["s"; "t"] [["a"]; ["b"]] []);
  check "root site"
    ("Eliom: in site / - One service or coservice has not been registered on URL /a. "
   ^ help)
    (unregistered [] [["a"]] []);
  check "non-attached coservice"
    ("Eliom: in site /s - The non-attached POST service \"na\" has not been registered."
   ^ help)
    (unregistered ["s"] [] [Common.SNa_post_ "na"]);
  check "services and non-attached coservices"
    ("Eliom: in site /s - One service or coservice has not been registered on URL /a. "
   ^ "Some non-attached services or coservices have not been registered: na (GET), <void coservice>."
   ^ help)
    (unregistered ["s"] [["a"]] [Common.SNa_get_ "na"; Common.SNa_void_keep])

let test_other_errors () =
  check "duplicate registration"
    "Eliom: Duplicate registration of service \"a/b\". Please correct the module."
    (Common.Eliom_duplicate_registration "a/b");
  check "page erasing"
    "Eliom: You cannot create a page or directory here. a already exists. Please correct your modules."
    (Common.Eliom_page_erasing "a");
  check "site information"
    "Eliom: Bad use of function \"register\". Must be used only during site initialisation phase (or, sometimes, also during request)."
    (Common.Site_information_not_available "register");
  check "request information"
    "Eliom: Bad use of function \"Registration.register\". It needs the current request, so it cannot be used during the site initialisation phase (for instance, register session services during a request)."
    (Common.Request_information_not_available "Registration.register");
  check "error while loading" "message"
    (Common.Eliom_error_while_loading_site "message")

let test_other_exceptions () =
  (* The other exceptions are left to Ocsigen Server. *)
  match Eliom.Mod_main.handle_init_exn Not_found with
  | s -> Alcotest.failf "message %S" s
  | exception Not_found -> ()

let suite =
  ( "initialisation errors"
  , [ Alcotest.test_case "unregistered services" `Quick test_unregistered
    ; Alcotest.test_case "other errors" `Quick test_other_errors
    ; Alcotest.test_case "other exceptions" `Quick test_other_exceptions ] )
