{shared{
  open XHTML5.M
  open Common
}}
{client{
  open Client
}}
open Server

let include_canvas (name:string) (canvas_box:[ Xhtml5types.div ] XHTML5.M.elt) =

  let bus,image_string = launch_server_canvas name in

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

let () = Connected.register ~service:multigraffiti_service
  !% ( fun name () username ->
    (* the page element in wich we will include the canvas *)
    let canvas_box = div [] in
    include_canvas name canvas_box;
    Lwt.return ([
      h1 [pcdata name];
      choose_drawing_form ();
      My_appl.a feed_service [pcdata "atom feed"] name;
      div ( if name = username
	then [save_image_box name]
	else [pcdata "no saving"] );
      canvas_box;]))

