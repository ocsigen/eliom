(* The application whose page the client tests load. *)

open Eliom

module Client_test = Registration.App (struct
    let application_name = "client_test"
    let global_data_path = None
  end)

let () =
  Client_test.register
    ~service:
      (Service.create ~path:(Service.Path ["page"])
         ~meth:(Service.Get Parameter.unit) ()) (fun () () ->
    Lwt.return Content.Html.F.(html (head (title (txt "page")) []) (body [])))

let () = Eliom_test_server.Server_harness.start [App.run ()]
