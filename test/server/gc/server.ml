(* The test server of test.ml: states expire after 1 s, and are collected
   only when /collect is called, which runs each collector once. *)

open Eliom
open Lwt.Syntax
module P = Parameter
module V = Reference.Volatile

let () =
  State.set_default_global_volatile_data_state_timeout ~cookie_level:`Session
    ~override_configfile:true (Some 1.);
  State.set_default_global_service_state_timeout ~cookie_level:`Session
    ~override_configfile:true (Some 1.);
  State.set_default_global_persistent_data_state_timeout ~cookie_level:`Session
    ~override_configfile:true (Some 1.);
  Config.set_session_gc_frequency None;
  Config.set_persistent_session_gc_frequency None

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f
let session_scope = Common.default_session_scope
let value = V.eref ~scope:session_scope ""

let persistent_value =
  Reference.eref ~scope:session_scope
    ~persistent:("test_gc_value", [%json: string])
    ""

let () =
  string (get ["set"] P.(string "v")) (fun v () -> V.set value v; text "set")

let () = string (get ["get"] P.unit) (fun () () -> text (V.get value))

let () =
  string
    (get ["persistent"; "set"] P.(string "v"))
    (fun v () ->
       let* () = Reference.set persistent_value v in
       text "set")

let fallback = get ["fallback"] P.unit
let () = string fallback (fun () () -> text "fallback")

let () =
  string (get ["coservice"] P.unit) (fun () () ->
    let service = Service.create_attached_get ~fallback ~get_params:P.unit () in
    Registration.String.register ~scope:session_scope ~service (fun () () ->
      text "coservice");
    text (Eliom_uri.make_string_uri ~absolute_path:true ~service ()))

let group_value = V.eref ~scope:Common.default_group_scope ""

let () =
  string
    (get ["group"; "join"] P.(string "name" ** string "v"))
    (fun (name, v) () ->
       State.set_volatile_data_session_group ~scope:session_scope name;
       V.set group_value v;
       text "joined")

let () =
  string (get ["collect"] P.unit) (fun () () ->
    let sitedata = Request_info.get_sitedata () in
    let* () = Mod_gc.collect_service_sessions sitedata in
    let* () = Mod_gc.collect_data_sessions sitedata in
    let* () = Mod_gc.collect_persistent_sessions sitedata in
    text "collected")

(* The numbers of sessions, and the session groups *)
let () =
  string (get ["count"] P.unit) (fun () () ->
    let* persistent = State.number_of_persistent_data_cookies () in
    text
      (Printf.sprintf "data=%d services=%d persistent=%d groups=[%s]"
         (State.number_of_volatile_data_cookies ())
         (State.number_of_service_cookies ())
         persistent
         (String.concat ","
            (List.sort compare (State.Ext.get_session_group_list ())))))

let () = Eliom_test_server.Server_harness.start [App.run ()]
