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

(* [writes tab name values] writes [values] on the bus [name] from the server *)
let writes tab name values =
  Lwt_list.iter_s
    (fun v -> Lwt.map ignore (text tab ("/" ^ name ^ "/write?v=" ^ v)))
    values

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
          "/process_bus/received")
    ; case "size, for the site" (fun server ->
        let a, ta = new_tab server in
        let* info = text ta "/small_site_bus/info" in
        let info = Bus_info.of_string info in
        let* () = writes ta "small_site_bus" ["1"; "2"; "3"] in
        let* m =
          C.request_stateless a info.channel (Eliom.Comet_base.After 1)
        in
        (match m with
        | [(_, C.Full)] -> ()
        | _ -> Alcotest.fail "full expected");
        let+ m =
          C.request_stateless a info.channel (Eliom.Comet_base.After 2)
        in
        Alcotest.(check (list string)) "kept" ["2"; "3"] (stateless_values m))
    ; case "size, for client processes" (fun server ->
        let _, tab = new_tab server in
        let* info = text tab "/small_process_bus/info" in
        let info = Bus_info.of_string info in
        let* () = C.register tab info.channel in
        let* () = writes tab "small_process_bus" ["1"; "2"; "3"] in
        let+ m = C.request tab info.channel 1 in
        match List.map snd m with
        | [C.Full] -> ()
        | _ -> Alcotest.fail "full expected") ] )

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

(* [signal tab name] is the channel of the signal [name] and its value, as
   sent to [tab] *)
let signal tab name =
  let+ s = text tab ("/" ^ name ^ "/info") in
  let info, value = Deriving_Json.from_string [%json: string * string] s in
  Comet_info.of_string info, value

let set tab name v = Lwt.map ignore (text tab ("/" ^ name ^ "/set?v=" ^ v))

let signals server =
  let case = case server in
  ( "signals"
  , [ case "down" (fun server ->
        (* The value sent with the page is sent again by the first request.
           Values of the signal may then be skipped, but not the last one. *)
        let _, tab = new_tab server in
        let* () = set tab "signal" "a" in
        let* info, value = signal tab "signal" in
        Alcotest.(check string) "value" "a" value;
        let* () = C.register tab info in
        let* m = C.request tab info 1 in
        check "first request" ["a"] m;
        let* () = set tab "signal" "b" in
        let* () = set tab "signal" "c" in
        let+ m = C.request tab info 2 in
        match List.rev (values m) with
        | "c" :: _ -> ()
        | _ -> Alcotest.fail "last value expected")
    ; case "down, for the site" (fun server ->
        let a, ta = new_tab server and b, _ = new_tab server in
        let* () = set ta "site_signal" "a" in
        let* info, value = signal ta "site_signal" in
        Alcotest.(check string) "value" "a" value;
        let* _ = text ta "/gc" in
        let* () = set ta "site_signal" "b" in
        let* () = set ta "site_signal" "c" in
        let last = Eliom.Comet_base.Last None in
        let* ma = C.request_stateless a info last in
        let+ mb = C.request_stateless b info last in
        Alcotest.(check (list string)) "a" ["c"] (stateless_values ma);
        Alcotest.(check (list string)) "b" ["c"] (stateless_values mb)) ] )

(* Channels of a scope of another hierarchy are closed with the client
   process of this hierarchy. *)
let scopes server =
  let closed name info_of =
    case server name (fun server ->
      let _, tab = new_tab server in
      let* info = info_of tab in
      let* () = C.register tab info in
      let* _ = text tab "/other/discard" in
      Lwt.catch
        (fun () ->
           let+ _ = C.request ~idle:true tab info 1 in
           Alcotest.fail "answered")
        (function C.State_closed -> Lwt.return_unit | e -> Lwt.fail e))
  in
  ( "scopes"
  , [ closed "down" (fun tab ->
        Lwt.map Comet_info.of_string (text tab "/other_down/info"))
    ; closed "signal" (fun tab -> Lwt.map fst (signal tab "other_signal")) ] )

let () =
  Server_harness.with_server "./server.exe" (fun server ->
    Alcotest.run ~and_exit:false "eliom-server-bus"
      [buses server; events server; signals server; scopes server])
