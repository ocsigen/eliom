(* The default application, as in a statically linked executable run by
   [Ocsigen.Server.start [host [Eliom.App.run ()]]]: the services are created
   and registered by the toplevel code of the modules, at the start of the
   program, which is in the initialisation phase of Ocsigen Server, before any
   site exists. Their registration is delayed until [App.run] gives them a
   site. There is one default application in a program, hence this separate
   executable. *)

module Service = Eliom.Service
module Parameter = Eliom.Parameter

let service path =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get Parameter.unit) ()

(* As the ppx does for the toplevel code of an Eliom module *)
let () = Eliom.Syntax.set_global true

let () =
  Eliom.Registration.Html_text.register ~service:(service ["registered"])
    (fun () () -> Lwt.return "")

let _unregistered = service ["unregistered"]
let () = Eliom.Syntax.set_global false

(* The site of the default application already exists: the defaults that the
   toplevel code sets with [Config] apply to it all the same. *)
let default_site = Eliom.Common.get_current_sitedata ()

let () =
  Eliom.Config.set_secure_cookies true;
  Eliom.Config.set_html_content_type "text/x-test"

let run = lazy (Eliom.App.run () [] Site.config_info ["s"])

let test_unregistered () =
  match Lazy.force run with
  | (_ : Ocsigen.Extensions.extension) ->
      Alcotest.fail "unregistered service accepted"
  | exception Eliom.Common.Eliom_there_are_unregistered_services (site, l, na)
    ->
      Alcotest.(check (list string)) "site" ["s"] site;
      Alcotest.(check (list (list string))) "services" [["unregistered"]] l;
      Alcotest.(check int) "non-attached coservices" 0 (List.length na)

let test_config_defaults () =
  (try ignore (Lazy.force run : Ocsigen.Extensions.extension)
   with Eliom.Common.Eliom_there_are_unregistered_services _ -> ());
  Alcotest.(check bool) "secure cookies" true default_site.secure_cookies;
  Alcotest.(check (option string))
    "HTML content type" (Some "text/x-test") default_site.html_content_type

let () =
  Alcotest.run "eliom-default-app"
    [ ( "default application"
      , [ Alcotest.test_case "unregistered" `Quick test_unregistered
        ; Alcotest.test_case "defaults of Config" `Quick test_config_defaults ]
      ) ]
