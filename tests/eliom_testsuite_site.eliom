
(*****************************************************************************)
(** References of scope site *)

open Eliom_content
open Eliom_lib

let reference_scope_site =
  let action =
    Eliom_registration.Action.register_post_coservice'
      ~post_params:(Eliom_parameter.string "v")
      (fun () v ->
         lwt () = Eliom_reference.set Eliom_testsuite_global.eref (Some v) in
         Eliom_reference.set Eliom_testsuite_global.eref' (Some v))
  in
  Eliom_registration.Html5.register_service
    ~path:["reference_scope_site"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let show = function None -> Html5.D.entity "#x2012" | Some str -> Html5.D.pcdata str in
       lwt v = Lwt.map show (Eliom_reference.get Eliom_testsuite_global.eref) in
       lwt v' = Lwt.map show (Eliom_reference.get Eliom_testsuite_global.eref') in
       Lwt.return Html5.D.(
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
             Html5.D.post_form
               ~service:action
               (fun name ->
                  [Html5.D.string_input ~input_type:`Text ~name ()])
               ()
           ])
       ))
