open Eliom
module P = Parameter
module V = Reference.Volatile

let text s = Lwt.return (s, "text/plain")

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let register service f = Registration.String.register ~service f
let session_value = V.eref ~scope:Common.default_session_scope ""

let start () =
  register
    (get ["set"] P.(string "v"))
    (fun v () -> V.set session_value v; text "set");
  register (get ["get"] P.unit) (fun () () -> text (V.get session_value));
  register (get ["count"] P.unit) (fun () () ->
    text (string_of_int (State.number_of_volatile_data_cookies ())));
  Eliom_test_server.Server_harness.start [App.run ()]
