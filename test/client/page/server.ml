(* The application whose page the client tests load. The page sets cookies of
   the client process, and gives services and the URLs of Urls computed by the
   server. *)

open Eliom
module P = Parameter

module Client_test = Registration.App (struct
    let application_name = "client_test"
    let global_data_path = None
  end)

let get path params =
  Service.create ~path:(Service.Path path) ~meth:(Service.Get params) ()

let main = get ["a"; "b"] P.(int "n" ** string "s")
let root = get [] P.unit
let page = get ["page"] P.unit
let deep = get ["x"; "y"; "z"] P.unit
let suffix = get ["suffix"] P.(suffix (int "n" ** string "s"))

let non_attached =
  Service.create ~name:"na" ~path:Service.No_path ~meth:(Service.Get P.unit) ()

let attached =
  Service.create_attached_get ~name:"co" ~fallback:root
    ~get_params:P.(int "n")
    ()

let external_ =
  Service.extern ~prefix:"http://example.org" ~path:["e"]
    ~meth:(Service.Get P.(string "s"))
    ()

let services = main, root, page, deep, suffix, non_attached, attached, external_

let register service =
  Registration.String.register ~service (fun _ _ ->
    Lwt.return ("", "text/plain"))

let () =
  register main;
  register root;
  register deep;
  register suffix;
  register non_attached;
  register attached

let set_cookie ?path ?exp ?secure name =
  State.set_cookie ~cookie_level:`Client_process ?path ?exp ?secure ~name
    ~value:(name ^ "_value") ()

let () =
  Client_test.register ~service:page (fun () () ->
    let now = Unix.gettimeofday () in
    set_cookie "plain";
    set_cookie ~secure:true "secure";
    set_cookie ~path:["other"] "other_path";
    set_cookie ~exp:(now -. 10.) "expired";
    set_cookie ~exp:(now +. 3600.) "expires_later";
    let urls =
      List.map (fun (_, url) -> Js_literal.string url) (Urls.cases services)
    in
    Lwt.return
      Content.Html.F.(
        html
          (head
             (title (txt "page"))
             [ script
                 (cdata_script
                    (Printf.sprintf
                       "var services = '%s';\nvar server_urls = [%s];"
                       (Lib.jsmarshal (Wrap.wrap services))
                       (String.concat ", " urls))) ])
          (body [])))

let () = Eliom_test_server.Server_harness.start [App.run ()]
