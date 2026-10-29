(** This is the main file if you are using static linking without config file.
*)

let logdir = "local/var/log/%%%PROJECT_NAME%%%"
let dbfile = "local/var/data/%%%PROJECT_NAME%%%/ocsidb"
let rundir = "local/var/run"
let staticdir = "local/var/www/%%%PROJECT_NAME%%%"

let () =
  Ocsipersist_settings.set_db_file dbfile;
  Ocsigen.Server.start
    ~ports:[`All, 8080]
    ~veryverbose:() ~debugmode:true ~logdir ~uploaddir:(Some "/tmp")
    ~usedefaulthostname:true
    ~command_pipe:(Filename.concat rundir "%%%PROJECT_NAME%%%-cmd")
    ~default_charset:(Some "utf-8")
    [Ocsigen.Server.host [Staticmod.run ~dir:staticdir (); Eliom.App.run ()]]
