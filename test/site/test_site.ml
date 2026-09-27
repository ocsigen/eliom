let () =
  (* A new database for each run *)
  let db = Filename.temp_file "eliom-test-site" ".sqlite" in
  Ocsipersist_settings.set_db_file db;
  Fun.protect
    ~finally:(fun () -> Sys.remove db)
    (fun () ->
       Alcotest.run ~and_exit:false "eliom-site"
         [ Test_services.suite
         ; Test_registration.suite
         ; Test_references.suite
         ; Test_persistent_references.suite
         ; Test_app_options.suite ])
