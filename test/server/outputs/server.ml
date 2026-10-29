(* The test server of test.ml: actions that reload the page, services
   sending OCaml values, and pages of an application. *)

open Eliom
module P = Parameter
module V = Reference.Volatile

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f

(* Actions: each sets the value of the session to its name. The pages show
   this value. *)

let value = V.eref ~scope:Common.default_session_scope "none"

let action service name =
  Registration.Action.register ~service (fun () () ->
    V.set value name; Lwt.return_unit)

let page = get ["page"] P.(opt (string "x"))

let () =
  string page (fun x () ->
    text
      (Printf.sprintf "page x=%s value=%s"
         (Option.value ~default:"none" x)
         (V.get value)))

let fallback = get ["fallback"] P.unit
let () = string fallback (fun () () -> text ("fallback value=" ^ V.get value))

let na_action =
  Service.create ~path:Service.No_path ~meth:(Service.Get P.unit) ()

let () = action na_action "non-attached"

let attached_action =
  Service.create_attached_get ~fallback ~get_params:P.unit ()

let () = action attached_action "attached"
let () = action (get ["path_action"] P.unit) "path"

(* Services sending OCaml values *)

let () =
  Registration.Ocaml.register
    ~service:
      (Service.create_ocaml ~path:(Service.Path ["ocaml"])
         ~meth:(Service.Get P.(int "i"))
         ())
    (fun i () -> if i < 0 then failwith "negative" else Lwt.return (i + 1))

(* Pages of an application. They tell whether the request is the first one
   of the client process. *)

module Test_app = Registration.App (struct
    let application_name = "test_app"
    let global_data_path = None
  end)

let app_page ?(scripts = []) () =
  Lwt.return
    Content.Html.F.(
      html
        (head (title (txt "app")) scripts)
        (body
           [ p
               [ txt
                   (if Test_app.is_initial_request ()
                    then "initial request"
                    else "request of the client process") ] ]))

let () =
  Test_app.register ~service:(get ["app"] P.unit) (fun () () -> app_page ())

let () =
  Test_app.register ~service:(get ["app_with_script"] P.unit) (fun () () ->
    app_page ~scripts:[Test_app.application_script ()] ())

let () =
  Test_app.register ~options:{Registration.do_not_launch = true}
    ~service:(get ["app_not_launched"] P.unit) (fun () () -> app_page ())

(* Pages with the HTTP status set by their handler *)

(* [status_text s] sets the status [s] and shows the status set *)
let status_text s =
  Registration.set_status s;
  text
    (match Registration.get_status () with
    | Some s -> Cohttp.Code.string_of_status (s :> Cohttp.Code.status_code)
    | None -> "none")

let () = string (get ["status"] P.unit) (fun () () -> status_text `Not_found)

let () =
  string (get ["no_status"] P.unit) (fun () () ->
    text
      (match Registration.get_status () with
      | Some _ -> "some"
      | None -> "none"))

(* The status set by the handler wins over the code of the registration. *)
let () =
  Registration.String.register ~code:410
    ~service:(get ["status_over_code"] P.unit) (fun () () ->
    status_text `Not_found)

let () =
  Test_app.register ~service:(get ["app_status"] P.unit) (fun () () ->
    Registration.set_status `Not_found;
    app_page ())

(* Actions setting a status: sent as is without reload, dropped when the
   page is reloaded. *)
let status_action =
  Service.create ~path:Service.No_path ~meth:(Service.Get P.unit) ()

let () =
  Registration.Action.register ~service:status_action (fun () () ->
    Registration.set_status `Bad_request;
    V.set value "status";
    Lwt.return_unit)

let () =
  Registration.Action.register ~options:`NoReload
    ~service:(get ["status_no_reload"] P.unit) (fun () () ->
    Registration.set_status `Bad_request;
    Lwt.return_unit)

(* The status set by a handler that raises an exception is dropped: the
   exception handler of the site answers with its own. *)
exception Boom

let () =
  string (get ["status_raise"] P.unit) (fun () () ->
    Registration.set_status `Not_found;
    raise Boom)

let error_page text =
  Content.Html.F.(html (head (title (txt text)) []) (body [p [txt text]]))

let () =
  Registration.set_exn_handler (function
    | Boom -> Registration.Html.send (error_page "error page")
    | e -> Lwt.fail e)

(* Links to the actions *)

let () =
  string
    (get ["link"] P.(string "to"))
    (fun target () ->
       text
         (match target with
         | "non-attached" ->
             Eliom_uri.make_string_uri ~absolute_path:true ~service:na_action ()
         | "attached" ->
             Eliom_uri.make_string_uri ~absolute_path:true
               ~service:attached_action ()
         | "status" ->
             Eliom_uri.make_string_uri ~absolute_path:true
               ~service:status_action ()
         | _ -> invalid_arg target))

let () = Eliom_test_server.Server_harness.start [App.run ()]
