{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

module %%%MODULE_NAME%%%_app =
  Eliom_registration.App (
    struct
      let application_name = "%%%PROJECT_NAME%%%"
    end)

let main_service =
  Eliom_service.create
    ~id:(Eliom_service.Id.Path [])
    ~meth:(Eliom_service.Meth.Get Eliom_parameter.unit)
    ~ret:Eliom_service.Ret.Non_ocaml
    ()

let () =
  %%%MODULE_NAME%%%_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"%%%PROJECT_NAME%%%"
           ~css:[["css";"%%%PROJECT_NAME%%%.css"]]
           Html5.F.(body [
             h1 [pcdata "Welcome from Eliom's distillery!"];
           ])))
