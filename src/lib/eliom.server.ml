let run ?site () =
  Ocsigen_server.Site.register ?site (Eliom_registration.instruction ())
