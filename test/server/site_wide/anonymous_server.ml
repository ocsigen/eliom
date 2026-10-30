(* At most two anonymous coservices of the global table created during
   requests. *)

open Eliom
module P = Parameter

let text, get, register = Services.(text, get, register)

let () =
  State.set_default_max_anonymous_services_per_site ~override_configfile:true 2

let fallback = get ["fallback"] P.unit
let () = register fallback (fun () () -> text "fallback")
let uri service = Eliom_uri.make_string_uri ~absolute_path:true ~service ()

(* An anonymous coservice created during the initialisation of the site *)

let initial = Service.create_attached_get ~fallback ~get_params:P.unit ()

let () =
  Registration.String.register ~service:initial (fun () () -> text "initial")

let () = register (get ["initial"] P.unit) (fun () () -> text (uri initial))

(* An anonymous coservice of the global table, created by the request *)

let () =
  register
    (get ["coservice"] P.(string "v"))
    (fun v () ->
       let service =
         Service.create_attached_get ~fallback ~get_params:P.unit ()
       in
       Registration.String.register ~service (fun () () ->
         text ("coservice " ^ v));
       text (uri service))

let () = Services.start ()
