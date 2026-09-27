let () =
  Alcotest.run "eliom-site"
    [Test_services.suite; Test_registration.suite; Test_references.suite]
