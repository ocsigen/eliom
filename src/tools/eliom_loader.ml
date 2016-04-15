(* Load Eliom client-side program after storing global data in
   localStorage. Compile as follos:

   ocamlfind ocamlc \
     -package js_of_ocaml,js_of_ocaml.ppx,lwt.ppx \
     -linkpkg -o eliom_loader.byte \
     eliom_loader.ml

   js_of_ocaml eliom_loader.byte
*)

let update_failed = ref false
let data_upload_failed = ref false

let url =
  Js.Optdef.case (Js.Unsafe.global##.___eliom_server_)
    (fun ()     -> "127.0.0.1:8080/__global_data__")
    (fun server -> Js.to_string server ^ "/__global_data__")

let storage () =
  Js.Optdef.case (Dom_html.window##.localStorage)
    (fun () -> failwith "Browser storage not supported")
    (fun v -> v)

let rec add_retry_button wake msg =
  let container = Dom_html.getElementById "app-container" in
  let p = Dom_html.createP Dom_html.document in
  let btn = Dom_html.createButton Dom_html.document in
  (* Set error class *)
  (Dom_html.getElementById "app-container")##.className :=
    Js.string "app-error";
  (* Error message paragraph *)
  Dom.appendChild p
    (Dom_html.document##createTextNode
      (Js.string (msg ^ ". Please try again later.")));
  p##.id := Js.string "retry-message";
  (* Retry button *)
  Dom.appendChild btn
    (Dom_html.document##createTextNode(Js.string "Retry"));
  btn##.onclick := Dom_html.handler
      (fun _ ->
         Dom.removeChild container p;
         if !update_failed then begin
           update_failed := false;
           ignore (Js.Unsafe.global##.chcp##fetchUpdate)
         end;
         if !data_upload_failed then begin
           data_upload_failed := false;
           Lwt.async (fun () -> get_data wake)
         end;
         Js._false);
  btn##.id := Js.string "retry-button";
  Dom.appendChild p btn;
  Dom.appendChild container p

and get_data wake =
  let%lwt {XmlHttpRequest.content; code} = XmlHttpRequest.get url in
  if code = 200 then begin
    (storage ())##setItem (Js.string "__global_data") (Js.string content);
    Lwt.wakeup wake ()
  end else begin
    data_upload_failed := true;
    if not !update_failed then
      add_retry_button wake "No connection available"
  end;
  Lwt.return ()

let redirect () =
  Js.Optdef.iter (Js.Unsafe.global##.___eliom_html_url_)
    (fun url -> Dom_html.window##.location##replace (url))

let _ =
  let wait2, wake = Lwt.wait () in
  let callback =
    Dom.handler
      (fun ev ->
         update_failed := false;
         Lwt.wakeup wake ();
         Js.bool true)
  in
  ignore @@ Dom.addEventListener Dom_html.document
    (Dom_html.Event.make "chcp_nothingToUpdate")
    callback Js._false;
  let error_callback =
    Dom.handler
      (fun ev ->
         update_failed := true;
         if not !data_upload_failed then
           add_retry_button wake
             (Js.to_string (ev##.detail##.error##.description));
         Js.bool true)
  in
  List.iter
    (fun ev ->
       ignore @@
       Dom.addEventListener Dom_html.document (Dom_html.Event.make ev)
         error_callback Js._false)
    ["chcp_updateLoadFailed";
     "chcp_updateInstallFailed";
     "chcp_assetsInstallationError"];
  Lwt.async @@ fun () ->
  let%lwt _ = Lwt_js_events.onload () in
  let wait, wake = Lwt.wait () in
  let%lwt _ = get_data wake in
  let%lwt _ = wait in
  let%lwt _ = wait2 in
  Lwt.return (redirect ())
