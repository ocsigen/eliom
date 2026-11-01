open Eliom
open Eliom_test_services
module P = Parameter
module V = Reference.Volatile

let session_value = V.eref ~scope:Common.default_session_scope ""

let start () =
  string
    (get ["set"] P.(string "v"))
    (fun v () -> V.set session_value v; text "set");
  string (get ["get"] P.unit) (fun () () -> text (V.get session_value));
  string (get ["count"] P.unit) (fun () () ->
    text (string_of_int (State.number_of_volatile_data_cookies ())));
  Eliom_test_server.Server_harness.start [App.run ()]
