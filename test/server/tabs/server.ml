(* The test server of test.ml: services that set, read and discard the states
   of the tab (client process) and of the browser session of the request, and
   answer in plain text. *)

open Eliom
open Lwt.Syntax
module P = Parameter
module V = Reference.Volatile

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f
let tab_scope = Common.default_process_scope
let session_scope = Common.default_session_scope
let group_scope = Common.default_group_scope
let tab_value = V.eref ~scope:tab_scope ""
let session_value = V.eref ~scope:session_scope ""

let () =
  string
    (get ["tab"; "set"] P.(string "v"))
    (fun v () -> V.set tab_value v; text "set")

let () =
  string (get ["tab"; "get"] P.unit) (fun () () -> text (V.get tab_value))

let () =
  string
    (get ["session"; "set"] P.(string "v"))
    (fun v () -> V.set session_value v; text "set")

let () =
  string
    (get ["session"; "get"] P.unit)
    (fun () () -> text (V.get session_value))

(* A coservice registered for the tab of the request *)

let fallback = get ["fallback"] P.unit
let () = string fallback (fun () () -> text "fallback")

let () =
  string
    (get ["tab"; "coservice"] P.unit)
    (fun () () ->
       let service =
         Service.create_attached_get ~fallback ~get_params:P.unit ()
       in
       Registration.String.register ~scope:tab_scope ~service (fun () () ->
         text "coservice of the tab");
       text (Eliom_uri.make_string_uri ~absolute_path:true ~service ()))

let () =
  string
    (get ["group"; "join"] P.(string "name"))
    (fun name () ->
       State.set_volatile_data_session_group ~scope:session_scope name;
       State.set_service_session_group ~scope:session_scope name;
       text "joined")

let () =
  string
    (get ["group"; "leave_services"] P.unit)
    (fun () () ->
       State.unset_service_session_group ~scope:session_scope ();
       text "left")

let scope_of_string = function
  | "tab" -> (tab_scope :> Common.user_scope)
  | "session" -> (session_scope :> Common.user_scope)
  | "group" -> (group_scope :> Common.user_scope)
  | s -> failwith ("unknown scope " ^ s)

let () =
  string
    (get ["discard"] P.(string "scope"))
    (fun scope () ->
       let* () = State.discard ~scope:(scope_of_string scope) () in
       text "discarded")

let () = Eliom_test_server.Server_harness.start [App.run ()]
