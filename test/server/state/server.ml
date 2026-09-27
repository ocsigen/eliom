(* The test server of test.ml: services that set, read and discard the states
   of the browser sending the request, and answer in plain text. *)

open Eliom
module P = Parameter
module V = Reference.Volatile
open Lwt.Syntax

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f
let session_scope = Common.default_session_scope
let group_scope = Common.default_group_scope

(* Session data *)

let session_value = V.eref ~scope:session_scope ""

let () =
  string
    (get ["set"] P.(string "v"))
    (fun v () -> V.set session_value v; text "set")

let () = string (get ["get"] P.unit) (fun () () -> text (V.get session_value))

let string_of_status = function
  | State.Alive_state -> "alive"
  | State.Empty_state -> "empty"
  | State.Expired_state -> "expired"

let () =
  string (get ["status"] P.unit) (fun () () ->
    text
      (Printf.sprintf "data=%s services=%s"
         (string_of_status
            (State.volatile_data_state_status ~scope:session_scope ()))
         (string_of_status (State.service_state_status ~scope:session_scope ()))))

(* Session services: a coservice registered for the session of the request,
   whose fallback answers for the other browsers *)

let fallback = get ["fallback"] P.unit
let () = string fallback (fun () () -> text "fallback")

let () =
  string (get ["coservice"] P.unit) (fun () () ->
    let service = Service.create_attached_get ~fallback ~get_params:P.unit () in
    let owner = V.get session_value in
    Registration.String.register ~scope:session_scope ~service (fun () () ->
      text ("coservice of " ^ owner));
    text (Eliom_uri.make_string_uri ~absolute_path:true ~service ()))

(* Closing *)

let scope_of_string = function
  | "session" -> (session_scope :> Common.user_scope)
  | "group" -> (group_scope :> Common.user_scope)
  | s -> failwith ("unknown scope " ^ s)

let () =
  string
    (get ["discard"] P.(string "scope"))
    (fun scope () ->
       let* () = State.discard ~scope:(scope_of_string scope) () in
       text "discarded")

let () =
  string
    (get ["discard_data"] P.(string "scope"))
    (fun scope () ->
       let* () = State.discard_data ~scope:(scope_of_string scope) () in
       text "data discarded")

let () =
  string
    (get ["discard_services"] P.(string "scope"))
    (fun scope () ->
       State.discard_services ~scope:(scope_of_string scope) ();
       text "services discarded")

(* Persistent session data *)

let persistent_value =
  Reference.eref ~scope:session_scope
    ~persistent:("test_session_value", [%json: string])
    ""

let () =
  string
    (get ["persistent"; "set"] P.(string "v"))
    (fun v () ->
       let* () = Reference.set persistent_value v in
       text "set")

let () =
  string
    (get ["persistent"; "get"] P.unit)
    (fun () () ->
       let* v = Reference.get persistent_value in
       text v)

(* Session groups *)

let group_value = V.eref ~scope:group_scope ""

let () =
  string
    (get ["group"; "join"] P.(string "name" ** opt (int "max")))
    (fun (name, set_max) () ->
       State.set_volatile_data_session_group ?set_max ~scope:session_scope name;
       State.set_service_session_group ?set_max ~scope:session_scope name;
       let* () =
         State.set_persistent_data_session_group
           ?set_max:(Option.map Option.some set_max)
           ~scope:session_scope name
       in
       text "joined")

let () =
  string
    (get ["group"; "set"] P.(string "v"))
    (fun v () -> V.set group_value v; text "set")

let () =
  string (get ["group"; "get"] P.unit) (fun () () -> text (V.get group_value))

let persistent_group_value =
  Reference.eref ~scope:group_scope
    ~persistent:("test_group_value", [%json: string])
    ""

let () =
  string
    (get ["group"; "persistent"; "set"] P.(string "v"))
    (fun v () ->
       let* () = Reference.set persistent_group_value v in
       text "set")

let () =
  string
    (get ["group"; "persistent"; "get"] P.unit)
    (fun () () ->
       let* v = Reference.get persistent_group_value in
       text v)

let () =
  string
    (get ["group"; "size"] P.unit)
    (fun () () ->
       text
         (match
            State.get_volatile_data_session_group_size ~scope:session_scope ()
          with
         | Some n -> string_of_int n
         | None -> "none"))

(* The sessions of a group, from outside the group, as
   Os.Session.disconnect_all of Ocsigen Start closes them *)

let () =
  string
    (get ["group"; "sessions"] P.(string "name"))
    (fun name () ->
       let state =
         State.Ext.volatile_data_group_state ~scope:group_scope name
       in
       text
         (string_of_int
            (State.Ext.fold_volatile_sub_states ~state (fun n _ -> n + 1) 0)))

let () =
  string
    (get ["group"; "close_sessions"] P.(string "name"))
    (fun name () ->
       let close state =
         State.Ext.iter_sub_states ~state (fun state ->
           State.Ext.discard_state ~state ())
       in
       let* () =
         close (State.Ext.volatile_data_group_state ~scope:group_scope name)
       in
       let* () =
         close (State.Ext.service_group_state ~scope:group_scope name)
       in
       let* () =
         close (State.Ext.persistent_data_group_state ~scope:group_scope name)
       in
       text "closed")

let () = Eliom_test_server.Server_harness.start [App.run ()]
