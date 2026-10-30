(* The server of the browser tests: [server.exe PORT STATIC_DIR DATA_DIR] serves
   the application e2e on localhost, and its client program from STATIC_DIR.
   DATA_DIR holds the logs and the persistent data. *)

let () =
  match Sys.argv with
  | [|_; port; static_dir; data_dir|] ->
      Ocsipersist_settings.set_db_file (Filename.concat data_dir "ocsidb");
      Ocsigen.Server.start
        ~ports:[`IPv4 Unix.inet_addr_loopback, int_of_string port]
        ~command_pipe:(Filename.concat data_dir "command")
        ~logdir:data_dir ~debugmode:true
        [ Ocsigen.Server.host
            [Staticmod.run ~dir:static_dir (); Eliom.App.run ()] ]
  | _ ->
      prerr_endline "usage: server.exe PORT STATIC_DIR DATA_DIR";
      exit 2
