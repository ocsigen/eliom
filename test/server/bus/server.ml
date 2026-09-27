(* The test server of test.ml: buses of the site and of client processes,
   and events of Eliom_react, whose information is sent in JSON. *)

open Eliom
module P = Parameter
module Client = Eliom_test_client

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f

(* [record stream] is the list of the values received on [stream] so far,
   most recent first. *)
let record stream =
  let received = ref [] in
  Lwt.async (fun () ->
    Lwt_stream.iter (fun v -> received := v :: !received) stream);
  received

let received_service name received =
  string
    (get [name; "received"] P.unit)
    (fun () () -> text (String.concat "," (List.rev !received)))

(* [bus ?size name ~scope] is a bus of strings, with the services
   /name/info, /name/write?v= and /name/received. *)
let bus ?size name ~scope =
  let b = Bus.create ~scope ?size [%json: string] in
  let received = record (Bus.stream b) in
  string
    (get [name; "info"] P.unit)
    (fun () () -> text (Client.Bus_info.to_string (Client.Bus_info.of_bus b)));
  string
    (get [name; "write"] P.(string "v"))
    (fun v () -> Lwt.bind (Bus.write b v) (fun () -> text "written"));
  received_service name received

let () =
  bus "site_bus" ~scope:`Site;
  bus "process_bus" ~scope:Common.comet_client_process_scope;
  bus "small_site_bus" ~scope:`Site ~size:2;
  bus "small_process_bus" ~scope:Common.comet_client_process_scope ~size:2

(* Events from the server to the client *)

let down name ?scope () =
  let event, send = React.E.create () in
  let d = Eliom_react.Down.of_react ?scope event in
  string
    (get [name; "info"] P.unit)
    (fun () () ->
       text (Client.Comet_info.to_string (Client.React_info.of_down d)));
  string (get [name; "send"] P.(string "v")) (fun v () -> send v; text "sent")

(* A scope of client processes of another hierarchy, with a service to
   discard its states *)
let other_scope = `Client_process (Common.create_scope_hierarchy "other")

let () =
  string
    (get ["other"; "discard"] P.unit)
    (fun () () ->
       Lwt.bind (State.discard ~scope:other_scope ()) (fun () ->
         text "discarded"))

let () =
  down "down" ();
  down "site_down" ~scope:`Site ();
  down "other_down" ~scope:other_scope ()

(* Signals from the server to the client, with services /name/info and
   /name/set?v= *)

let signal name ?scope () =
  let s, set = React.S.create "initial" in
  let d = Eliom_react.S.Down.of_react ?scope s in
  string
    (get [name; "info"] P.unit)
    (fun () () ->
       let info, value = Client.React_info.of_signal_down d in
       text
         (Deriving_Json.to_string [%json: string * string]
            (Client.Comet_info.to_string info, value)));
  string (get [name; "set"] P.(string "v")) (fun v () -> set v; text "set")

let () =
  signal "signal" ();
  signal "site_signal" ~scope:`Site ();
  signal "other_signal" ~scope:other_scope ()

(* Events from the client to the server *)

let up = Eliom_react.Up.create (P.ocaml "v" [%json: string])
let up_received = ref []

(* Kept alive: React events do not keep their dependent events. *)
let () =
  Lwt_react.E.keep
    (React.E.map
       (fun v -> up_received := v :: !up_received)
       (Eliom_react.Up.to_react up))

let () =
  string
    (get ["up"; "info"] P.unit)
    (fun () () ->
       text (Client.Service_info.to_string (Client.React_info.of_up up)));
  received_service "up" up_received

(* A full major collection, to check that nothing needed is collected *)
let () =
  string (get ["gc"] P.unit) (fun () () -> Gc.full_major (); text "collected")

let () = Eliom_test_server.Server_harness.start [App.run ()]
