let hostname = "example.org"
let http_port = 8080
let https_port = 8443

let config_info =
  { (Ocsigen.Extensions.default_config_info ()) with
    default_hostname = hostname
  ; default_httpport = http_port
  ; default_httpsport = https_port }

(* The site data of an application are kept by Eliom for the next site of the
   same name. *)
let apps = Hashtbl.create 16

let init
      ?(config_info = config_info)
      ?(site_dir = [])
      ?(run = fun ~app -> Eliom.App.run ~app ())
      ~app
      f
  =
  if Hashtbl.mem apps app
  then invalid_arg ("Site.init: application " ^ app ^ " already used");
  Hashtbl.add apps app ();
  let result = ref None in
  Eliom.Service.register_eliom_module app (fun () ->
    Eliom.Syntax.set_global true;
    Fun.protect
      ~finally:(fun () -> Eliom.Syntax.set_global false)
      (fun () -> result := Some (f ())));
  Ocsigen.Extensions.start_initialisation ();
  Fun.protect ~finally:Ocsigen.Extensions.end_initialisation (fun () ->
    ignore (run ~app [] config_info site_dir : Ocsigen.Extensions.extension));
  match !result with
  | Some r -> r
  | None -> failwith ("Site.init: " ^ app ^ " was not initialised")

let register service =
  Eliom.Registration.Html_text.register ~service (fun _ _ -> Lwt.return "")
