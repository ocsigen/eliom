[%%shared
  open Eliom_lib;
  open Eliom_content;
  open Html.D
];

let module %%%MODULE_NAME%%%_app = Eliom_registration.App {
  let application_name = "%%%PROJECT_NAME%%%";
  let global_data_path = None;
};

let main_service =
  Eliom_service.create
    path::(Eliom_service.Path [])
    meth::(Eliom_service.Get Eliom_parameter.unit)
    ();

let () =
  %%%MODULE_NAME%%%_app.register
    service::main_service
    (
      fun () () => Lwt.return (
        Eliom_tools.F.html
          title::"%%%PROJECT_NAME%%%"
          css::[["css", "%%%PROJECT_NAME%%%.css"]]
          Html.F.(body [h1 [pcdata "Welcome from Eliom's distillery!"]])
      )
    );
