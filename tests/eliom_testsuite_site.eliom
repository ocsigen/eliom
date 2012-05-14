
(*****************************************************************************)
(** References of scope site *)

open Eliom_content.HTML5.D

let reference_scope_site =
  let action =
    Eliom_output.Action.register_post_coservice'
      ~post_params:(Eliom_parameters.string "v")
      (fun () v ->
         lwt () = Eliom_references.set Eliom_testsuite_global.eref (Some v) in
         Eliom_references.set Eliom_testsuite_global.eref' (Some v))
  in
  Eliom_output.Html5.register_service
    ~path:["reference_scope_site"]
    ~get_params:Eliom_parameters.unit
    (fun () () ->
       let show = function None -> entity "#x2012" | Some str -> pcdata str in
       lwt v = Lwt.map show (Eliom_references.get Eliom_testsuite_global.eref) in
       lwt v' = Lwt.map show (Eliom_references.get Eliom_testsuite_global.eref') in
       Lwt.return (
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
             post_form
               ~service:action
               (fun name ->
                  [string_input ~input_type:`Text ~name ()])
               ()
           ])
       ))


