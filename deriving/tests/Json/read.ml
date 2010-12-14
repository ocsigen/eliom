(*pp ${JS_OF_OCAML} *)

open Lwt

module Html = Dom_html
let js = Js.string
let document = Html.window##document

let add_msg msg =
  let ul = Js.Opt.get (document##getElementById(js"ul")) (fun () -> assert false) in
  let li = Html.createLi document in
  let msg = document##createTextNode(msg) in
  Dom.appendChild li msg;
  Dom.appendChild ul li;
  Lwt.return ()

let show_data () =
  add_msg (js ("Valeur originale: " ^ Decl.print_all Decl.all)) >>= fun () ->

  add_msg (js "Test: serveur -> client") >>= fun () ->
  let json = Json.to_string<Decl.all> Decl.all in
  add_msg (js ("Json généré par deriving: " ^ json)) >>= fun () ->
  let data : Decl.all = Json.unsafe_input (js json) in
  add_msg (js "JSON.parse: OK !") >>= fun () ->
  add_msg (js ("Comparaison des valeurs orignale et desérialisée: "
	       ^ if data = Decl.all then "OK" else "KO")) >>= fun () ->
  add_msg (js ("Valeur désérialisée: " ^ Decl.print_all data)) >>= fun () ->
  add_msg (js ("Test encodage:" ^ Decl.extract_string data)) >>= fun () ->

  add_msg (js "Test: client -> serveur") >>= fun () ->
  let json' = Json.output Decl.all in
  add_msg (js ("Json généré par stringify: " ^ Js.to_string json')) >>= fun () ->
  let data' : Decl.all = Json.from_string<Decl.all> (Js.to_string json') in
  add_msg (js "Json.from_string<Decl.all>: OK !") >>= fun () ->
  add_msg (js ("Comparaison des valeurs orignale et desérialisée : "
	       ^ if data' = Decl.all then "OK" else "KO")) >>= fun () ->
  add_msg (js ("Valeur désérialisée : " ^ Decl.print_all data')) >>= fun () ->
  add_msg (js ("Test encodage:" ^ Decl.extract_string data')) >>= fun () ->
  Lwt.return ()

let start () =
  show_data ()

let _ = Html.window##onload <- Html.handler (fun _ -> ignore (start ()); Js._false)
