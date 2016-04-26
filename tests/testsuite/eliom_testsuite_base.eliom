

{shared{
  open Eliom_content
  open Eliom_lib
}}

{client{
  let () =
    if Js.to_string Dom_html.window##location##hash = "#__trace" then
      Eliom_config.set_tracing true;
    if Js.to_string Dom_html.window##location##hash = "#__timings" then
      Eliom_config.debug_timings := true
}}

(* This is server only because there are no delimiters. *)
module My_appl = struct

  include
    Eliom_registration.App
      (struct
        let application_name = "testsuite_client"
        let global_data_path = None
      end)

  (* partial compatibility layer for old Eliom_registration *)

  let register_service ~path ~get_params f =
    create
      ~id:(Eliom_service.Path path)
      ~meth:(Eliom_service.Get get_params) f

  let register_coservice ?scope ~fallback ~get_params f =
    create
      ?scope
      ~id:(Eliom_service.Fallback fallback)
      ~meth:(Eliom_service.Get get_params) f

  let register_coservice' ?scope ~get_params f =
    create
      ?scope
      ~id:Eliom_service.Global
      ~meth:(Eliom_service.Get get_params) f

  let register_post_coservice' ~post_params f =
    create
      ~id:Eliom_service.Global
      ~meth:(Eliom_service.Post (Eliom_parameter.unit, post_params)) f

end

module Service = struct

  (* partial compatibility layer for old Eliom_service *)

  include Eliom_service

  module Http = struct

    let service ~path ~get_params () =
      create
        ~id:(Path path)
        ~meth:(Get get_params)
        ()

    let post_coservice
        ?name ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https
        ~fallback ~post_params () =
      create
        ?name ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?https
        ~id:(Fallback fallback)
        ~meth:(Post (Eliom_parameter.unit, post_params))
        ()

    let post_coservice'
        ?csrf_safe ?csrf_scope ?csrf_secure
        ?max_use ?timeout ?name ~post_params () =
      create
        ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout ?name
        ~id:Global
        ~meth:(Post (Eliom_parameter.unit, post_params))
        ()

    let coservice
        ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout
        ~fallback ~get_params () =
      create
        ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout
        ~id:(Fallback fallback)
        ~meth:(Get get_params)
        ()

    let coservice'
        ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout
        ~get_params () =
      create
        ?csrf_safe ?csrf_scope ?csrf_secure ?max_use ?timeout
        ~id:Global
        ~meth:(Get get_params)
        ()

    let post_service ~fallback ~post_params () =
      let path =
        let info =
          match info fallback with
          | Attached info ->
            info
          | _ ->
            failwith
              "create_post_with_fallback with non-attached fallback"
        in
        sub_path info
      and get_params = get_params_type fallback in
      let id = Path path
      and meth = Post (get_params, post_params) in
      create ~id ~meth ()

  end

  module App = Http

end

module Registration = struct

  type 'a redirected_service = 'a Eliom_registration.redirected_service =
      Service :
        (unit, unit, Eliom_service.get , _, _, _, _,
         [ `WithoutSuffix ], unit, unit, 'a) Eliom_service.t ->
      'a redirected_service

  module Html5 = struct

    include Eliom_registration.Html5

    let register_service
        ?code ?charset ?headers ?priority ?error_handler ~path
        ~get_params f =
      create
        ?code ?charset ?headers
        ~id:(Eliom_service.Path path)
        ~meth:(Eliom_service.Get get_params)
        ?error_handler
        f

    let register_coservice
        ?scope ?timeout ?error_handler ~fallback ~get_params f =
      create
        ?scope ?timeout
        ?error_handler
        ~id:(Eliom_service.Fallback fallback)
        ~meth:(Eliom_service.Get get_params)
        f

    let register_coservice'
        ?scope ?timeout ?error_handler ~get_params f =
      create
        ?scope ?timeout
        ?error_handler
        ~id:Eliom_service.Global
        ~meth:(Eliom_service.Get get_params)
        f

    let register_post_coservice
        ?scope ?timeout ?error_handler ~fallback ~post_params f =
      create
        ?scope ?timeout
        ?error_handler
        ~id:(Eliom_service.Fallback fallback)
        ~meth:(Eliom_service.Post (Eliom_parameter.unit, post_params))
        f

    let register_post_coservice'
        ?scope ?timeout ?error_handler ~post_params f =
      create
        ?scope ?timeout
        ?error_handler
        ~id:Eliom_service.Global
        ~meth:(Eliom_service.Post (Eliom_parameter.unit, post_params))
        f

    let register_post_service ?error_handler ~fallback ~post_params f =
      let path =
        let info =
          match Eliom_service.info fallback with
          | Eliom_service.Attached info ->
            info
          | _ ->
            failwith
              "create_post_with_fallback with non-attached fallback"
        in
        Eliom_service.sub_path info
      and get_params = Eliom_service.get_params_type fallback in
      let id = Eliom_service.Path path
      and meth = Eliom_service.Post (get_params, post_params) in
      Eliom_registration.Html5.create ~id ~meth f

  end

  module Ocaml = struct

    include Eliom_registration.Ocaml

    let register_service ~path ~get_params f =
      create
        ~id:(Eliom_service.Path path)
        ~meth:(Eliom_service.Get get_params)
        f

    let register_coservice' ?scope ~get_params f =
      create ?scope
        ~id:Eliom_service.Global
        ~meth:(Eliom_service.Get get_params)
        f

    let register_post_coservice' ?scope ~post_params f =
      create ?scope
        ~id:Eliom_service.Global
        ~meth:(Eliom_service.Post (Eliom_parameter.unit, post_params))
        f

  end

  module App          = Eliom_registration.App
  module Action       = Eliom_registration.Action
  module Flow5        = Eliom_registration.Flow5
  module Redirection  = Eliom_registration.Redirection
  module Any          = Eliom_registration.Any
  module Html_text    = Eliom_registration.Html_text
  module Unit         = Eliom_registration.Unit
  module File         = Eliom_registration.File
  module String       = Eliom_registration.String
  module Eliom_tmpl   = Eliom_registration.Eliom_tmpl

  let set_exn_handler = Eliom_registration.set_exn_handler

  type appl_service_options = Eliom_registration.appl_service_options = {
    do_not_launch : bool
  }

  let appl_self_redirect = Eliom_registration.appl_self_redirect

end

let main =
  let open Eliom_service in
  create
    ~id:(Path [])
    ~meth:(Get Eliom_parameter.unit)
    ()

(* an alias to allow using main even when it is shadowed by some open
   HTML module *)
let main_service = main

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
  ttl, My_appl.create
           ~id:(Eliom_service.Path path)
           ~meth:(Eliom_service.Get Eliom_parameter.unit)
           (fun () () ->
              lwt content = f () in
              let toggle_tracing = {{
                fun _ ->
                  Eliom_config.set_tracing (not (Eliom_config.get_tracing ()));
                  Dom_html.window##alert ( Js.string (
                    Printf.sprintf "%s tracing"
                    (if Eliom_config.get_tracing ()
                       then "Enabled" else "Disabled")))
              }}
              in
              Lwt.return
                Html5.F.(html
                           (Eliom_tools.F.head
                              ~title:(String.concat "/" path)
                              ~css:[["style.css"]] ())
                           (body
                              (div [
                                a ~xhr:false ~service:main_service
                                  [pcdata "Home and break app"] () ;
                                pcdata " - " ;
                                a ~service:Eliom_service.reload_action [pcdata "Reload in running app"] () ;
                                pcdata " - " ;
                                Raw.a ~a:[a_onclick toggle_tracing]
                                  [pcdata "Toggle tracing (or append #__trace to the URL)"] ;
                               ] ::
                               h1 ~a:[a_class ["test_title"]] [pcdata ttl] ::
                               div ~a:[a_class ["test_description"]] description ::
                               hr () ::
                               content @
                               [ test_logger ]))))

let thebutton ?(msg="THE BUTTON") onclick : [> Html5_types.button ] Html5.elt =
  Html5.F.(
    Form.button_no_value ~button_type:`Submit
      ~a:[a_class ["thebutton"]; a_onclick onclick]
      [ pcdata msg ])

let monospace fmt =
  Printf.ksprintf
    (fun str ->
       Html5.F.(span ~a:[a_class ["monospace"]] [pcdata str]))
    fmt

{client{

  let buffer = ref []
  let append_log_message msg =
    Html5.Manip.appendChild
      %test_logger
      Html5.D.(div ~a:[a_class ["logging_line"]] [pcdata msg])

  let () =
    let rec flush () =
      Eliom_client.onload
        (fun () ->
          List.iter append_log_message (List.rev !buffer);
          buffer := [];
          Lwt.ignore_result
            (lwt () = Lwt_js.sleep 0.01 in
             Lwt.return (flush ())))
    in flush ()

  let log : 'a . ('a, unit, string, unit) format4 -> 'a =
    fun fmt ->
      Printf.ksprintf
        (fun msg ->
           if Eliom_client_core.in_onload ()
           then buffer := msg :: !buffer
           else append_log_message msg)
        fmt

}}

{shared{
  let report_flush_assertions' name output ~ran ~failed =
    Printf.ksprintf output "Eliom_testsuite %S:" name;
    Printf.ksprintf output " * Ran %d assertions (%s)"
      (List.length ran) (String.concat ", " ran);
    if failed = [] then
      Printf.ksprintf output
        " * All tests succeeded"
    else
      Printf.ksprintf output
        " * %d tests failed: %s"
        (List.length failed) (String.concat ", " failed);
}}

{server{
  let failed_assertions : string list Eliom_reference.Volatile.eref =
    Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope []
  let ran_assertions : string list Eliom_reference.Volatile.eref =
    Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope []
  let report_flush_assertions name =
    report_flush_assertions' name (fun s -> Lwt_log.ign_debug s)
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
