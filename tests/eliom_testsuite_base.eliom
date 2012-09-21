

{shared{
  open Eliom_content
  open Eliom_lib
}}

module My_appl =
  Eliom_registration.App (
    struct
      let application_name = "eliom_testsuite"
    end)

let main = Eliom_service.service [] Eliom_parameter.unit ()

let tests description services =
  Html5.F.(
    div [
      h4 [pcdata description];
      ul
        (List.map
           (fun (description, service) ->
              li [a ~service [pcdata description] ()])
           services);
    ]
  )

let testsuite ~name testsuite_tests =
  Html5.F.(
    div
      (h3 [pcdata name] ::
       List.map (uncurry tests) testsuite_tests)
  )

let test_logger =
  Html5.Id.create_global_elt
    (Html5.D.(div ~a:[a_class ["test_logger"]]
                [h4 [pcdata "Client logger"]]))

let test ~path ~title:ttl ~description f =
  ttl, My_appl.register_service
           ~path
           ~get_params:Eliom_parameter.unit
           (fun () () ->
              lwt content = f () in
              Lwt.return
                Html5.F.(html
                           (Eliom_tools.Menu.F.head
                              ~title:(String.concat "/" path)
                              ~css:[["style.css"]] ())
                           (body
                              (p [a ~xhr:false ~service:main [pcdata "Home and break app"] ()] ::
                               p [a ~service:Eliom_service.void_coservice' [pcdata "Reload in running app"] ()] ::
                               h1 ~a:[a_class ["test_title"]] [pcdata ttl] ::
                               div ~a:[a_class ["test_description"]] description ::
                               hr () ::
                               content @
                               [ test_logger ]))))

let thebutton ?(msg="THE BUTTON") onclick : [> Html5_types.button ] Html5.elt =
  Html5.F.(
    button
      ~a:[a_class ["thebutton"]; a_onclick onclick]
      ~button_type:`Submit
      [ pcdata msg ])

let monospace fmt =
  Printf.ksprintf
    (fun str ->
       Html5.F.(span ~a:[a_class ["monospace"]] [pcdata str]))
    fmt

{client{

  let buffer = ref []
  let append_log_message msg =
    trace "append_log_message %s" msg;
    Html5.Manip.appendChild
      %test_logger
      Html5.D.(div ~a:[a_class ["logging_line"]] [pcdata msg])
  let () =
    Lwt.ignore_result
      (lwt () = Eliom_client.wait_load_end () in
       List.iter append_log_message (List.rev !buffer);
       buffer := [];
       Lwt.return ())
  let log : 'a . ('a, unit, string, unit) format4 -> 'a =
    fun fmt ->
      Printf.ksprintf
        (fun msg ->
           if Eliom_client.in_onload ()
           then buffer := msg :: !buffer
           else append_log_message msg)
        fmt

}}

{shared{
  let report_flush_assertions' name output ~ran ~failed =
    Printf.ksprintf output
      "Eliom_testsuite %S: Ran %d assertions (%s)" name
      (List.length ran) (String.concat ", " ran);
    if failed = [] then
      Printf.ksprintf output
        "Eliom_testsuite %S: All tests succeeded" name
    else
      Printf.ksprintf output
        "Eliom_testsuite %S: %d tests failed: %s" name
        (List.length failed) (String.concat ", " failed);
}}

{server{
  let failed_assertions : string list Eliom_reference.Volatile.eref =
    Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope []
  let ran_assertions : string list Eliom_reference.Volatile.eref =
    Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope []
  let report_flush_assertions name =
    report_flush_assertions' name (debug "%s")
      ~ran:(Eliom_reference.Volatile.get ran_assertions)
      ~failed:(Eliom_reference.Volatile.get failed_assertions);
    Eliom_reference.Volatile.set ran_assertions [];
    Eliom_reference.Volatile.set failed_assertions []
  let assert_equal ?(eq=(=)) ~name value should_be =
    Eliom_reference.Volatile.modify ran_assertions
      (fun names -> name :: names);
    if not (eq value should_be) then
      Eliom_reference.Volatile.modify failed_assertions
        (fun names -> name :: names)
}}

{client{
  let ran_assertions = ref []
  let failed_assertions = ref []
  let report_flush_assertions name =
    report_flush_assertions' name (log "%s")
      ~ran:!ran_assertions
      ~failed:!failed_assertions;
    ran_assertions := [];
    failed_assertions := []
  let assert_equal ?(eq=(=)) ~name value should_be =
    ran_assertions := name :: !ran_assertions;
    if not (eq value should_be) then
      failed_assertions := name :: !failed_assertions
}}

