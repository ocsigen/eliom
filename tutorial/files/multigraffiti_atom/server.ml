open XHTML5.M
open Common
open Lwt

module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "graffiti"
      let params =
	{ Eliom_output.default_appl_params with
	  
          Eliom_output.ap_headers_before =
            [
              XHTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(XHTML5.M.uri_of_string"./css/graffiti.css")
                ();
              XHTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(XHTML5.M.uri_of_string"./css/common.css")
                ();
              XHTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(XHTML5.M.uri_of_string"./css/hsvpalette.css")
                ();
              XHTML5.M.link ~rel:[ `Stylesheet ]
                ~href:(XHTML5.M.uri_of_string"./css/slider.css")
                ();
              XHTML5.M.script
                ~a:[XHTML5.M.a_src (XHTML5.M.uri_of_string "./goog/base.js")
                   ] (XHTML5.M.pcdata "");
              XHTML5.M.script
                ~a:[XHTML5.M.a_src (XHTML5.M.uri_of_string "./graffiti_req.js")
                   ] (XHTML5.M.pcdata "");
            ];
	}
    end)

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i = (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255. in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

let launch_server_canvas () =
  let bus = Eliom_bus.create Json.t<messages> in
  
  let draw_server, image_string =
    let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height in
    let ctx = Cairo.create surface in
    ((fun ((color : string), size, (x1, y1), (x2, y2)) ->

      (* Set thickness of brush *)
      Cairo.set_line_width ctx (float size) ;
      Cairo.set_line_join ctx Cairo.LINE_JOIN_ROUND ;
      Cairo.set_line_cap ctx Cairo.LINE_CAP_ROUND ;
      let red, green, blue =  rgb_from_string color in
      Cairo.set_source_rgb ctx ~red ~green ~blue ;

      Cairo.move_to ctx (float x1) (float y1) ;
      Cairo.line_to ctx (float x2) (float y2) ;
      Cairo.close_path ctx ;
      
      (* Apply the ink *)
      Cairo.stroke ctx ;
     ),
     (fun () ->
       let b = Buffer.create 10000 in
       (* Output a PNG in a string *)
       Cairo_png.surface_write_to_stream surface (Buffer.add_string b);
       Buffer.contents b
     ))
  in
  let _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus) in
  bus,image_string

let main_service = Eliom_services.service ~path:[""]
  ~get_params:(Eliom_parameters.unit) ()
let multigraffiti_service = Eliom_services.coservice ~fallback:main_service
  ~get_params:(Eliom_parameters.string "name") ()

let choose_drawing_form () =
  My_appl.get_form ~service:multigraffiti_service
    (fun (name) ->
      [p [pcdata "drawing name: ";
          My_appl.string_input ~input_type:`Text ~name ();
          br ();
          My_appl.string_input ~input_type:`Submit ~value:"Go" ()
         ]])

let connection_service =
  Eliom_services.post_coservice'
    ~post_params:(let open Eliom_parameters in (string "name" ** string "password"))
    ()
let disconnection_service = Eliom_services.post_coservice' ~post_params:Eliom_parameters.unit ()
let create_account_service = 
  Eliom_services.post_coservice ~fallback:main_service ~post_params:(let open Eliom_parameters in (string "name" ** string "password")) ()

let username = Eliom_references.eref ~scope:`Session None
let users = ref ["titi","tata";"test","test"]
let check_pwd name pwd = try List.assoc name !users = pwd with Not_found -> false

let () = Eliom_output.Action.register
  ~service:create_account_service
  (fun () (name, pwd) ->
    users := (name, pwd)::!users;
    Lwt.return ())

let () = Eliom_output.Action.register
  ~service:connection_service
  (fun () (name, password) ->
    if check_pwd name password
    then Eliom_references.set username (Some name)
    else Lwt.return ())

let () =
  Eliom_output.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.close_session ())

let disconnect_box () =
  My_appl.post_form disconnection_service
    (fun _ -> [p [My_appl.string_input
                  ~input_type:`Submit ~value:"Log out" ()]]) ()

let login_name_form service button_text =
  My_appl.post_form ~service
    (fun (name1, name2) ->
      [p [pcdata "login: ";
          My_appl.string_input ~input_type:`Text ~name:name1 ();
          br ();
          pcdata "password: ";
          My_appl.string_input ~input_type:`Password ~name:name2 ();
          br ();
          My_appl.string_input ~input_type:`Submit ~value:button_text ()
         ]]) ()

let default_content () =
  Lwt.return [h1 [pcdata "Welcome to Multigraffiti"];
	      h2 [pcdata "log in"];
	      login_name_form connection_service "Connect";
	      h2 [pcdata "create account"];
	      login_name_form create_account_service "Create account";]

module Connected_translate =
struct
  type page = string -> My_appl.page Lwt.t
  let translate page =
    Eliom_references.get username >>=
      function
	| None -> default_content ()
	| Some username -> page username >|= ( fun v -> (disconnect_box ())::v )
end

module Connected =
  Eliom_output.Customize ( My_appl ) ( Connected_translate )

let ( !% ) f = fun a b -> return (fun c -> f a b c)

let () = Connected.register ~service:main_service 
  !% (fun () () username ->
    Lwt.return [h1 [pcdata ("Welcome to Multigraffiti " ^ username)];
		choose_drawing_form ()])


let graffiti_info = Hashtbl.create 0

let launch_server_canvas name =
  (* create a new bus and image_string function only if it did not exists *)
  try
    Hashtbl.find graffiti_info name
  with
    | Not_found ->
      let bus,imageservice = launch_server_canvas () in
      Hashtbl.add graffiti_info name (bus,imageservice);
      (bus,imageservice)

let static_dir = "/tmp/static/"

let image_dir name =
  let dir = static_dir ^ "/graffiti_saved/" ^ (Ocsigen_lib.encode name) in
  Lwt.catch (fun () -> Lwt_unix.mkdir dir 0o777) (fun _ -> Lwt.return ()) >|= (fun () -> dir)

let make_filename name number =
  image_dir name >|= ( fun dir -> (dir ^ "/" ^ (string_of_int number) ^ ".png") )

let save image name number =
  make_filename name number >>=
    ( fun file_name ->
      Lwt_io.open_file ~mode:Lwt_io.output file_name >>=
	( fun out_chan -> Lwt_io.write out_chan image ) )

let image_info_table = Ocsipersist.open_table "image_info_table"

let save_image username =
  let now = CalendarLib.Calendar.now () in
  Lwt.catch
    (fun () -> Ocsipersist.find image_info_table username)
    (function Not_found -> Lwt.return (0,now,[]) | e -> Lwt.fail e )
  >>= ( fun (number,_,list) ->
    (Ocsipersist.add image_info_table username (number+1,now,(number,now)::list))
    >>= ( fun () ->
      let (_,image_string) = Hashtbl.find graffiti_info username in
      save (image_string ()) username number ))

let save_image_box name =
  let save_image_service =
    Eliom_output.Action.register_post_coservice'
      ~post_params:Eliom_parameters.unit
      (fun () () -> save_image name)
  in
  My_appl.post_form save_image_service
    (fun _ ->
      [p [My_appl.string_input
             ~input_type:`Submit ~value:"save" ()]]) ()

let feed_service = Eliom_services.service ~path:["feed"]
  ~get_params:(Eliom_parameters.string "name") ()

let local_filename name number =
  ["graffiti_saved"; Ocsigen_lib.encode name ; (string_of_int number) ^ ".png"]

let rec entries name list = function
  | 0 -> []
  | len ->
    match list with
      | [] -> []
      | (n,saved)::q ->
	let title = Atom_feed.plain ("graffiti " ^ name ^ " " ^ (string_of_int n)) in
	let uri = 
	  Eliom_uri.make_string_uri ~absolute:true ~service:(Eliom_services.static_dir ())
	    (local_filename name n)
	in
	let entry =
	  Atom_feed.entry ~title ~id:uri ~updated:saved
            [Atom_feed.xhtmlC [ XHTML.M.img ~src:uri ~alt:"image" ()]] in
	entry::(entries name q (len - 1))

let feed name () =
  let id = Eliom_uri.make_string_uri ~absolute:true ~service:feed_service name in
  let title = Atom_feed.plain ("nice drawings of " ^ name) in
  Lwt.catch
    (fun () -> Ocsipersist.find image_info_table name >|=
	(fun (number,updated,list) -> Atom_feed.feed ~id ~updated ~title (entries name list 10)))
    ( function Not_found ->
      let now = CalendarLib.Calendar.now () in
      Lwt.return (Atom_feed.feed ~id ~updated:now ~title [])
      | e -> Lwt.fail e )

let () = Eliom_atom.Reg.register ~service:feed_service feed
