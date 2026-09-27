(* The test server of test.ml: Comet channels whose messages are sent by
   services. The information about a channel is sent in JSON
   (Eliom_test_client.Comet_info). *)

open Eliom
open Lwt.Syntax
module P = Parameter
module V = Reference.Volatile
module Info = Eliom_test_client.Comet_info

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let string service f = Registration.String.register ~service f
let tab_scope = Common.default_process_scope

(* Requests waiting for data are answered after 1 s. *)
let () = Config.set_comet_timeout 1.

(* A channel of the client process of the request, whose stream is pushed by
   /stateful/push and ended by /stateful/end *)

let push_of_tab : (string option -> unit) option V.eref =
  V.eref ~scope:tab_scope None

let push_to_tab v =
  match V.get push_of_tab with
  | Some push -> push v; text "pushed"
  | None -> text "no channel"

let () =
  string
    (get ["stateful"; "create"] P.(opt (int "size")))
    (fun size () ->
       let stream, push = Lwt_stream.create () in
       let channel = Comet.Channel.create ?size stream in
       V.set push_of_tab (Some push);
       text (Info.to_string (Info.of_channel channel)))

let () =
  string
    (get ["stateful"; "push"] P.(string "v"))
    (fun v () -> push_to_tab (Some v))

let () = string (get ["stateful"; "end"] P.unit) (fun () () -> push_to_tab None)

let () =
  string (get ["discard"] P.unit) (fun () () ->
    let* () = State.discard ~scope:Common.comet_client_process_scope () in
    text "discarded")

(* Channels of the site *)

let site_channel ~name create =
  let stream, push = Lwt_stream.create () in
  let channel = create stream in
  string
    (get [name; "info"] P.unit)
    (fun () () -> text (Info.to_string (Info.of_channel channel)));
  string
    (get [name; "push"] P.(string "v"))
    (fun v () -> push (Some v); text "pushed")

let () =
  site_channel ~name:"stateless"
    (Comet.Channel.create ~scope:`Site ~name:"site");
  site_channel ~name:"newest" (Comet.Channel.create_newest ~name:"newest")

(* The channel "site", declared as a channel of another server, here the
   same one *)
let () =
  let channel =
    Comet.Channel.external_channel ~prefix:"http://eliom-test" ~name:"site" ()
  in
  string
    (get ["external"; "info"] P.unit)
    (fun () () -> text (Info.to_string (Info.of_channel channel)))

let () = Eliom_test_server.Server_harness.start [App.run ()]
