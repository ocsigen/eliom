(* The client reads the data of a page of the server, as a browser does: the
   application, the cookies of the client process. It computes the URLs of
   Urls as the server does. *)

open Js_of_ocaml

let host = (Eliom.Process.get_info ()).cpi_hostname

let () =
  Printf.printf "application: %s\nhost: %s\n"
    (Eliom.Client.get_application_name ())
    host

(* The cookies of the client process that a request sends *)
let print_cookies ~https path =
  Eliom.Mod_cookies.get_cookies_to_send (Some host) https path
  |> List.sort compare
  |> List.map (fun (name, value) -> name ^ "=" ^ value)
  |> String.concat " "
  |> Printf.printf "cookies of /%s%s: %s\n" (String.concat "/" path)
       (if https then " (https)" else "")

let () =
  print_cookies ~https:false ["page"];
  print_cookies ~https:true ["page"];
  print_cookies ~https:false ["other"; "x"]

(* The cookies of the table, expired or not *)
let print_table () =
  Eliom.Mod_cookies.get_table (Some host)
  |> Ocsigen_cookie_map.Map_path.bindings
  |> List.concat_map (fun (path, cookies) ->
    Ocsigen_cookie_map.Map_inner.bindings cookies
    |> List.map (fun (name, _) -> String.concat "/" path ^ ":" ^ name))
  |> String.concat " "
  |> Printf.printf "table: %s\n"

let () = print_table ()

(* Cookies changed by a response *)
let update name cookie =
  Eliom.Mod_cookies.update_cookie_table (Some host)
    (Ocsigen_cookie_map.add ~path:[] name cookie Ocsigen_cookie_map.empty)

let () =
  let now = Js.to_float (new%js Js.date_now)##getTime /. 1000. in
  update "plain" OUnset;
  update "expires_later" (OSet (Some (now -. 1.), "", false));
  update "new" (OSet (None, "new_value", false));
  print_table ();
  print_cookies ~https:false ["page"];
  (* An expired cookie of the table is not sent, and is removed *)
  Eliom.Mod_cookies.set_table (Some host)
    (Ocsigen_cookie_map.Poly.add ~path:[] "old"
       (Some (now -. 1.), "old_value", false)
       (Eliom.Mod_cookies.get_table (Some host)));
  print_cookies ~https:false ["page"];
  print_table ()

(* The URLs depend on the request, whose information the client sets at load.
   The load event of the stub never fires: set it as at load. *)
let () =
  let data = Eliom.Request_info.get_request_data () in
  Lwt.ignore_result
    (Eliom.Request_info.set_session_info
       ~uri:
         (String.concat "/" (Eliom.Request_info.get_csp_original_full_path ()))
       data.ejs_sess_info Lwt.return)

let () =
  let server_urls : Js.js_string Js.t Js.js_array Js.t =
    Js.Unsafe.get Js.Unsafe.global (Js.string "server_urls")
  in
  let server_urls = Array.map Js.to_string (Js.to_array server_urls) in
  List.iteri
    (fun n (name, url) ->
       match server_urls.(n) with
       | on_server when on_server = url -> Printf.printf "%s: %s\n" name url
       | on_server ->
           Printf.printf "%s: %s, NOT %s on the server\n" name url on_server)
    (Urls.cases
       (Eliom.Unwrap.unwrap_js
          (Js.Unsafe.get Js.Unsafe.global (Js.string "services"))))
