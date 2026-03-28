(** This is the main file if you are using static linking without config file.
 *)

module%shared %%%MODULE_NAME%%% = %%%MODULE_NAME%%%

let%server _ =
  Ocsigen_server.start
    ~ports:[`All, 8080]
    ~veryverbose:()
    ~debugmode:true
    ~logdir:"local/var/log/%%%PROJECT_NAME%%%"
    ~datadir:"local/var/data/%%%PROJECT_NAME%%%"
    ~uploaddir:(Some "/tmp")
    ~usedefaulthostname:true
    ~command_pipe:"local/var/run/%%%PROJECT_NAME%%%-cmd"
    ~default_charset:(Some "utf-8")
    [ Ocsigen_server.host
      [Staticmod.run ~dir:"local/var/www/%%%PROJECT_NAME%%%" (); Eliom.run ()] ]
