(* The server of the browser tests:
   [server.exe PORT STATIC_DIR DATA_DIR (js | wasm)] serves the application e2e
   on localhost, and its client programs from STATIC_DIR. DATA_DIR holds the
   logs and the persistent data. With [wasm], the page loads the WebAssembly
   client program in the browsers that support it. *)

let () =
  match Sys.argv with
  | [|_; port; static_dir; data_dir; ("js" | "wasm") as backend|] ->
      Ocsipersist_settings.set_db_file (Filename.concat data_dir "ocsidb");
      Ocsigen.Server.start
        ~ports:[`IPv4 Unix.inet_addr_loopback, int_of_string port]
        ~command_pipe:(Filename.concat data_dir "command")
        ~logdir:data_dir ~debugmode:true
        [ Ocsigen.Server.host
            [ Staticmod.run ~dir:static_dir ()
            ; Eliom.App.run ~enable_wasm:(backend = "wasm") () ] ]
  | _ ->
      prerr_endline "usage: server.exe PORT STATIC_DIR DATA_DIR (js | wasm)";
      exit 2
