{shared{
  open XHTML5.M
  open Common
}}
{client{
  open Client
}}
open Server

let graffiti_info = Hashtbl.create 0

let include_canvas (name:string) (canvas_box:[ Xhtml5types.div ] XHTML5.M.elt) =

  (* create a new bus and image_string function only if it did not exists *)
  let bus,image_string =
    try
      Hashtbl.find graffiti_info name
    with
      | Not_found ->
	let bus,imageservice = launch_server_canvas () in
	Hashtbl.add graffiti_info name (bus,imageservice);
	(bus,imageservice)
  in

  let imageservice =
    Eliom_output.Text.register_coservice'
      ~timeout:10.
      (* the service is available fo 10 seconds only, but it is long
	 enouth for the browser to do its request. *)
      ~get_params:Eliom_parameters.unit
      (fun () () -> Lwt.return (image_string (), "image/png"))
  in

  Eliom_services.onload
    {{
      let canceller = launch_client_canvas %bus %imageservice %canvas_box in
      Eliom_client.on_unload (fun () -> stop_drawing canceller; Lwt.return ())
    }}

let () = My_appl.register ~service:multigraffiti_service
  ( fun name () ->
    (* the page element in wich we will include the canvas *)
    let canvas_box = div [] in
    include_canvas name canvas_box;
    Lwt.return [
      h1 [pcdata name];
      choose_drawing_form ();
      canvas_box;] )
