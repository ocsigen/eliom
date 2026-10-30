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

(* Sets the timeout of all the persistent cookies, or unsets it without [t].
   The cookies are read first: writing a cookie during an iteration on the
   SQLite table gives it to the iteration again. *)
let () =
  string
    (get ["persistent"; "timeout"] P.(opt (float "t")))
    (fun t () ->
       let* cookies =
         State.Ext.fold_persistent_data_cookies
           (fun cookie l -> Lwt.return (cookie :: l))
           []
       in
       let* () =
         Lwt_list.iter_s
           (fun cookie ->
              match t with
              | Some t ->
                  State.Ext.set_persistent_data_cookie_timeout ~cookie (Some t)
              | None -> State.Ext.unset_persistent_data_cookie_timeout ~cookie)
           cookies
       in
       text "timeout")

(* Sets the global timeout of persistent sessions, and computes again the
   expiration dates of the cookies. *)
let () =
  string
    (get ["persistent"; "global"] P.(float "t"))
    (fun t () ->
       State.set_global_persistent_data_state_timeout
         ~cookie_scope:session_scope ~recompute_expdates:true (Some t);
       text "global")

(* Whether the expiration dates of the persistent cookies are all more than
   50 s away ("far"), not all ("near"), or there is no cookie ("none"). *)
let () =
  string
    (get ["persistent"; "expiry"] P.unit)
    (fun () () ->
       let now = Unix.gettimeofday () in
       let* expiries =
         Mod_cookies.Persistent_cookies.Cookies.fold
           (fun _ {Mod_cookies.expiry; _} l -> Lwt.return (expiry :: l))
           []
       in
       let far = function Some t -> t > now +. 50. | None -> true in
       text
         (match expiries with
         | [] -> "none"
         | l -> if List.for_all far l then "far" else "near"))

(* Sets the timeout of the cookie of the session after closing it. *)
let () =
  string
    (get ["persistent"; "close"; "timeout"] P.unit)
    (fun () () ->
       let* state =
         State.Ext.current_persistent_data_state ~scope:session_scope ()
       in
       let* cookie = State.Ext.get_persistent_cookie_info state in
       let* () = State.discard ~scope:session_scope () in
       let* () =
         State.Ext.set_persistent_data_cookie_timeout ~cookie (Some 100.)
       in
       text "closed")

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
