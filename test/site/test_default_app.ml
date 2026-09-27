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

let test_unregistered () =
  match Eliom.App.run () [] Site.config_info ["s"] with
  | (_ : Ocsigen.Extensions.extension) ->
      Alcotest.fail "unregistered service accepted"
  | exception Eliom.Common.Eliom_there_are_unregistered_services (site, l, na)
    ->
      Alcotest.(check (list string)) "site" ["s"] site;
      Alcotest.(check (list (list string))) "services" [["unregistered"]] l;
      Alcotest.(check int) "non-attached coservices" 0 (List.length na)

let () =
  Alcotest.run "eliom-default-app"
    [ ( "default application"
      , [Alcotest.test_case "unregistered" `Quick test_unregistered] ) ]
