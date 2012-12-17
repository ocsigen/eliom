{shared{
  open Eliom_lib
  open Eliom_content
}}

module %%%MODULE_NAME%%%_app =
  Eliom_registration.App (
    struct
      let application_name = "%%%PROJECT_NAME%%%"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  %%%MODULE_NAME%%%_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"%%%PROJECT_NAME%%%"
           ~css:[["css";"%%%PROJECT_NAME%%%.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
