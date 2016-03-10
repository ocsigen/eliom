(* Load Eliom client-side program after storing global data in
   localStorage. Compile as follos:

   ocamlfind ocamlc \
     -package js_of_ocaml,js_of_ocaml.ppx,lwt.ppx \
     -linkpkg -o eliom_loader.byte \
     eliom_loader.ml

   js_of_ocaml eliom_loader.byte
*)

let url =
  if Js.Unsafe.global##.___eliom_server_ = Js.undefined then
    "127.0.0.1:8080/__global_data__"
  else
    Js.to_string (Js.Unsafe.global##.___eliom_server_) ^ "/__global_data__"

let storage () =
  Js.Optdef.case (Dom_html.window##.localStorage)
    (fun () -> failwith "Browser storage not supported")
    (fun v -> v)

let rec retry_button wake =
  let p = Dom_html.createP Dom_html.document in
  Dom.appendChild p
    (Dom_html.document##createTextNode(Js.string "click to retry"));
  p##.onclick := Dom_html.handler
      (fun _ ->
         Lwt.async (fun () -> get_data wake);
         Js._false);
  p##.id := Js.string "retry-button";
  p

and add_retry_button wake : unit =
  try
    ignore (Dom_html.getElementById "retry-button")
  with Not_found ->
    Dom.appendChild
      (Dom_html.getElementById "app-container")
      (retry_button wake)

and get_data wake =
  let%lwt {XmlHttpRequest.content; code} = XmlHttpRequest.get url in
  if code = 200 then (
    (storage ())##setItem (Js.string "__global_data") (Js.string content);
    Lwt.wakeup wake ()
  ) else
    add_retry_button wake;
  Lwt.return ()

let redirect () =
  if Js.Unsafe.global##.___eliom_html_url_ = Js.undefined then
    ()
  else
    (Js.Unsafe.coerce Dom_html.window)##.location :=
      (Js.Unsafe.global ##.___eliom_html_url_)

let _ =
  Lwt.async @@ fun () ->
  let%lwt _ = Lwt_js_events.onload () in
  let wait, wake = Lwt.wait () in
  let%lwt _ = get_data wake in
  let%lwt _ = wait in
  Lwt.return (redirect ())
