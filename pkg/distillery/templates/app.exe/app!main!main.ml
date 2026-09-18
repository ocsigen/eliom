(** This is the main file if you are using static linking without config file.
*)

let logdir = "local/var/log/%%%PROJECT_NAME%%%"
let datadir = "local/var/data/%%%PROJECT_NAME%%%"
let rundir = "local/var/run"
let staticdir = "local/var/www/%%%PROJECT_NAME%%%"

let () =
  Ocsigen_server.start
    ~ports:[`All, 8080]
    ~veryverbose:() ~debugmode:true ~logdir ~datadir ~uploaddir:(Some "/tmp")
    ~usedefaulthostname:true
    ~command_pipe:(Filename.concat rundir "%%%PROJECT_NAME%%%-cmd")
    ~default_charset:(Some "utf-8")
    [Ocsigen_server.host [Staticmod.run ~dir:staticdir (); Eliom.run ()]]
