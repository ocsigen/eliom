(* The test server of test.ml: services that set the timeouts of the states
   of the browser sending the request, and answer in plain text. *)

open Eliom
open Lwt.Syntax
module P = Parameter
module V = Reference.Volatile

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f
let session_scope = Common.default_session_scope
let cookie_scope = (session_scope :> Common.cookie_scope)

(* States *)

let value = V.eref ~scope:session_scope ""

let persistent_value =
  Reference.eref ~scope:session_scope
    ~persistent:("test_timeouts_value", [%json: string])
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

let () =
  string
    (get ["persistent"; "get"] P.unit)
    (fun () () ->
       let* v = Reference.get persistent_value in
       text v)

let fallback = get ["fallback"] P.unit
let () = string fallback (fun () () -> text "fallback")

(* [coservice ?timeout ?max_use ()] is the URL of a new coservice of the
   session of the request. *)
let coservice ?timeout ?max_use () =
  let service =
    Service.create_attached_get ?timeout ?max_use ~fallback ~get_params:P.unit
      ()
  in
  Registration.String.register ~scope:session_scope ~service (fun () () ->
    text "coservice");
  text (Eliom_uri.make_string_uri ~absolute_path:true ~service ())

let () = string (get ["coservice"] P.unit) (fun () () -> coservice ())

let () =
  string
    (get ["timed_coservice"] P.(float "t"))
    (fun timeout () -> coservice ~timeout ())

let () =
  string
    (get ["counted_coservice"] P.(int "n"))
    (fun max_use () -> coservice ~max_use ())

(* Timeouts of the states of the session, [None] without [t] *)

let () =
  string
    (get ["timeout"] P.(string "kind" ** opt (float "t")))
    (fun (kind, t) () ->
       let* () =
         match kind with
         | "data" ->
             State.set_volatile_data_state_timeout ~cookie_scope t;
             Lwt.return_unit
         | "service" ->
             State.set_service_state_timeout ~cookie_scope t;
             Lwt.return_unit
         | "persistent" ->
             State.set_persistent_data_state_timeout ~cookie_scope t
         | k -> failwith ("unknown kind " ^ k)
       in
       text "timeout set")

(* Expiration dates of the cookies, [t] seconds from now *)
let () =
  string
    (get ["cookie_expiration"] P.(opt (float "t")))
    (fun t () ->
       State.set_volatile_data_cookie_exp_date ~cookie_scope
         (Option.map (fun t -> Unix.time () +. t) t);
       text "expiration set")

let () = Eliom_test_server.Server_harness.start [App.run ()]
