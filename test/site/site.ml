let hostname = "example.org"
let http_port = 8080
let https_port = 8443

let config_info =
  { (Ocsigen.Extensions.default_config_info ()) with
    default_hostname = hostname
  ; default_httpport = http_port
  ; default_httpsport = https_port }

let init ?(config_info = config_info) ?(site_dir = []) ~app f =
  let result = ref None in
  Eliom.Service.register_eliom_module app (fun () ->
    Eliom.Syntax.set_global true;
    Fun.protect
      ~finally:(fun () -> Eliom.Syntax.set_global false)
      (fun () -> result := Some (f ())));
  Ocsigen.Extensions.start_initialisation ();
  Fun.protect ~finally:Ocsigen.Extensions.end_initialisation (fun () ->
    ignore
      (Eliom.App.run ~app () [] config_info site_dir
       : Ocsigen.Extensions.extension));
  match !result with
  | Some r -> r
  | None -> failwith ("Site.init: " ^ app ^ " was not initialised")
