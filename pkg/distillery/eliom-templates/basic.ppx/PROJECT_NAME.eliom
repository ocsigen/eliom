[%%shared
open Eliom.Lib
open Eliom.Content
open Html.D
]

module %%%MODULE_NAME%%%_app =
  Eliom.Registration.App (
  struct
    let application_name = "%%%PROJECT_NAME%%%"
    let global_data_path = None
  end)

let main_service =
  Eliom.Service.create
    ~path:(Eliom.Service.Path [])
    ~meth:(Eliom.Service.Get Eliom.Parameter.unit)
    ()

let () =
  %%%MODULE_NAME%%%_app.register
    ~service:main_service
    (fun () () ->
       Lwt.return
         (Eliom.Tools.F.html
            ~title:"%%%PROJECT_NAME%%%"
            ~css:[["css";"%%%PROJECT_NAME%%%.css"]]
            Html.F.(body [
              h1 [txt "Welcome from Eliom's distillery!"];
            ])))
