
(*****************************************************************************)
(** References of scope site *)

open Eliom_content
open Eliom_lib

let reference_scope_site =
  let action =
    Eliom_registration.Action.create
      ~path:Eliom_service.No_path
      ~meth:
        (Eliom_service.Post
           (Eliom_parameter.unit,
            Eliom_parameter.string "v"))
      (fun () v ->
         lwt () = Eliom_reference.set Eliom_testsuite_global.eref (Some v) in
         Eliom_reference.set Eliom_testsuite_global.eref' (Some v))
  in
  Eliom_registration.Html.create
    ~path:(Eliom_service.Path ["reference_scope_site"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () ->
       let show = function None -> Html.D.entity "#x2012" | Some str -> Html.D.pcdata str in
       lwt v = Lwt.map show (Eliom_reference.get Eliom_testsuite_global.eref) in
       lwt v' = Lwt.map show (Eliom_reference.get Eliom_testsuite_global.eref') in
       Lwt.return Html.D.(
         html
           (head (title (pcdata "")) [])
           (body [
             p [
               pcdata "This is site ";
               em [pcdata (Eliom_common.((get_sp ()).sp_sitedata.config_info).Ocsigen_extensions.default_hostname)];
             ];
             p [pcdata "Open other site (substitute localhost by 127.0.0.1 in the URL or vice verse)."];
             p [
               pcdata "Current value "; i [v];
               pcdata ", persistent "; i [v'];
             ];
             pcdata "Enter a new string for both references";
             Html.D.Form.post_form
               ~service:action
               (fun name ->
                  [Html.D.Form.input ~input_type:`Text ~name
                     Html.D.Form.string ])
               ()
           ])
       ))
