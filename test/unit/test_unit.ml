let () =
  Alcotest.run "eliom-unit"
    [ Test_parameter.suite
    ; Test_uri.suite
    ; Test_sessiongroups.suite
    ; Test_cookies.suite
    ; Test_wrap.suite
    ; Test_content.suite
    ; Test_config.suite ]
