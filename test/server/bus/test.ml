(* Tests of buses and of the events of Eliom_react, through the Comet client:
   values written by the server or by clients reach every participant. *)

open Eliom_test_server
open Eliom_test_client
open Lwt.Syntax
module C = Comet_client

let text tab url =
  let+ r = Tab.get tab url in
  if r.status <> 200 then Alcotest.failf "%s: status %d" url r.status;
  r.body

let check_text msg expected tab url =
  let+ body = text tab url in
  Alcotest.(check string) msg expected body

let values messages =
  List.filter_map (function _, C.Data v -> Some v | _ -> None) messages

let check msg expected messages =
  Alcotest.(check (list string)) msg expected (values messages)

let stateless_values messages =
  List.filter_map (function _, C.Data (v, _) -> Some v | _ -> None) messages

let new_tab server =
  let b = Browser.create server in
  b, Tab.create b

(* [write tab bus v] writes [v] on [bus] from the client of [tab], as the
   client-side program does: a list of values, in JSON. *)
let write tab (bus : Bus_info.t) v =
  let+ r =
    Service_info.post tab bus.write
      (Deriving_Json.to_string [%json: string list] [v])
  in
  if r.status <> 200 && r.status <> 204
  then Alcotest.failf "write: status %d" r.status

let case server name f =
  Alcotest.test_case name `Quick (fun () -> Lwt_main.run (f server))

let buses server =
  let case = case server in
  ( "buses"
  , [ case "bus of the site" (fun server ->
        let a, ta = new_tab server and b, tb = new_tab server in
        let* info = text ta "/site_bus/info" in
        let info = Bus_info.of_string info in
        let* _ = text tb "/site_bus/info" in
        let* _ = text ta "/site_bus/write?v=from-server" in
        let last = Eliom.Comet_base.Last (Some 1) in
        let* ma = C.request_stateless a info.channel last in
        let* mb = C.request_stateless b info.channel last in
        Alcotest.(check (list string)) "a" ["from-server"] (stateless_values ma);
        Alcotest.(check (list string)) "b" ["from-server"] (stateless_values mb);
        let* () = write ta info "from-a" in
        let* mb = C.request_stateless b info.channel last in
        Alcotest.(check (list string))
          "written by a" ["from-a"] (stateless_values mb);
        check_text "received by the server" "from-server,from-a" ta
          "/site_bus/received")
    ; case "bus of client processes" (fun server ->
        (* Each client process gets its own channel, fed by the bus. *)
        let _, ta = new_tab server and _, tb = new_tab server in
        let* ia = text ta "/process_bus/info" in
        let ia = Bus_info.of_string ia in
        let* ib = text tb "/process_bus/info" in
        let ib = Bus_info.of_string ib in
        let* () = C.register ta ia.channel in
        let* () = C.register tb ib.channel in
        let* _ = text ta "/process_bus/write?v=from-server" in
        let* ma = C.request ta ia.channel 1 in
        check "a" ["from-server"] ma;
        let* mb = C.request tb ib.channel 1 in
        check "b" ["from-server"] mb;
        let* () = write ta ia "from-a" in
        let* mb = C.request tb ib.channel 2 in
        check "written by a" ["from-a"] mb;
        check_text "received by the server" "from-server,from-a" ta
          "/process_bus/received") ] )

let events server =
  let case = case server in
  ( "events"
  , [ case "down" (fun server ->
        let _, tab = new_tab server in
        let* info = text tab "/down/info" in
        let info = Comet_info.of_string info in
        let* () = C.register tab info in
        let* _ = text tab "/down/send?v=x" in
        let+ m = C.request tab info 1 in
        check "occurrence" ["x"] m)
    ; case "down, for the site" (fun server ->
        let a, ta = new_tab server and b, _ = new_tab server in
        let* info = text ta "/site_down/info" in
        let info = Comet_info.of_string info in
        let* _ = text ta "/gc" in
        let* _ = text ta "/site_down/send?v=y" in
        let last = Eliom.Comet_base.Last (Some 1) in
        let* ma = C.request_stateless a info last in
        let+ mb = C.request_stateless b info last in
        Alcotest.(check (list string)) "a" ["y"] (stateless_values ma);
        Alcotest.(check (list string)) "b" ["y"] (stateless_values mb))
    ; case "up" (fun server ->
        let _, tab = new_tab server in
        let* info = text tab "/up/info" in
        let info = Service_info.of_string info in
        let* _ = text tab "/gc" in
        let* r =
          Service_info.post tab info
            (Deriving_Json.to_string [%json: string] "u")
        in
        if r.status <> 200 && r.status <> 204
        then Alcotest.failf "up: status %d" r.status;
        check_text "received by the server" "u" tab "/up/received") ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-bus" [buses server; events server])
