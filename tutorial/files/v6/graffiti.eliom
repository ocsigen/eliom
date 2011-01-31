{shared{
  open XHTML5.M
  let width = 700
  let height = 400
}}

module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "graffiti"
      let params = Eliom_output.default_appl_params
    end)

{client{
  open Event_arrows
  let draw ctx (color, size, (x1, y1), (x2, y2)) =
    ctx##strokeStyle <- (Js.string color);
    ctx##lineWidth <- float size;
    ctx##beginPath();
    ctx##moveTo(float x1, float y1);
    ctx##lineTo(float x2, float y2);
    ctx##stroke()
}}

{shared{
  type messages = (string * int * (int * int) * (int * int)) deriving (Json)
}}

let bus = Eliom_bus.create ~name:"graff" Json.t<messages>

let main_service =
  My_appl.register_service ~path:[""] ~get_params:Eliom_parameters.unit
    (fun () () ->
       Eliom_services.onload
         {{
           let canvas = Dom_html.createCanvas Dom_html.document in
           let ctx = canvas##getContext (Dom_html._2d_) in
           canvas##width <- width; canvas##height <- height;
           ctx##lineCap <- Js.string "round";

           Dom.appendChild Dom_html.document##body canvas;

           let x = ref 0 and y = ref 0 in
           let set_coord ev =
             let x0, y0 = Dom_html.elementClientPosition canvas in
             x := ev##clientX - x0; y := ev##clientY - y0 in
           let compute_line ev = 
             let oldx = !x and oldy = !y in
             set_coord ev;
             ("#ff9933", 5, (oldx, oldy), (!x, !y))
           in
           let (bus:messages Eliom_client_bus.t) = %bus in
           let line ev =
             let v = compute_line ev in
             let _ = Eliom_client_bus.write bus v in
             draw ctx v
           in
           let _ = Lwt_stream.iter (draw ctx) (Eliom_client_bus.stream bus) in
           ignore (run (mousedowns canvas
                          (arr (fun ev -> set_coord ev; line ev)
                           >>> first [mousemoves Dom_html.document (arr line);
                                      mouseup Dom_html.document >>> (arr line)])) ());
         }};
      Lwt.return [h1 [pcdata "Graffiti"]])
