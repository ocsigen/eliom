(* The persistent data of the tests go to a database of their own, set
   before any test opens it *)
let () =
  let file = Filename.temp_file "eliom_test_unit" ".sqlite" in
  Ocsipersist_settings.set_db_file file;
  at_exit (fun () -> Sys.remove file)

let () =
  Alcotest.run "eliom-unit"
    [ Test_parameter.suite
    ; Test_uri.suite
    ; Test_sessiongroups.suite
    ; Test_cookies.suite
    ; Test_wrap.suite
    ; Test_content.suite
    ; Test_config.suite
    ; Test_init_errors.suite
    ; Test_reference.suite ]
