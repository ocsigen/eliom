
{shared{
  open Eliom_content
  open Eliom_lib
}}

module My_appl =
  Eliom_registration.App (
    struct
      let application_name = "eliom_testsuite"
    end)

(******************************************************************************)
(*                                Custom data                                 *)

{shared{

  type my_data = { x : int; y : int } deriving (Json)

  let my_data =
    Eliom_content.Html5.Custom_data.create_json ~name:"my_int" ~default:{x=0;y=0;} Json.t<my_data>

}}

{client{

  let show_my_data (ev : Dom_html.mouseEvent Js.t) =
    let elt = Js.Optdef.get (ev##target) (fun () -> failwith "show_my_data") in
    let i = Html5.Custom_data.get_dom elt my_data in
    alert "custom_data : {x=%d;y=%d}" i.x i.y

  let change_data container =
    let element = Html5.To_dom.of_element container in
    let i = Html5.Custom_data.get_dom element my_data in
    let i' = { x = succ i.x; y = pred i.y } in
    Html5.Custom_data.set_dom element my_data i'
}}

let custom_data =
  My_appl.register_service
    ~path:["custom_data"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let open Html5.F in
       let container =
         Html5.D.div ~a:[Html5.Custom_data.attrib my_data { x = 100; y = 100 }]
           [pcdata "A: click me (my_data is originally {x=100;y=100})"]
       in
       let change_button =
         button ~a:[a_onclick {{ change_data %container }}] ~button_type:`Submit
           [pcdata "In-/decrement my_data"]
       in
       Lwt.return
         (html
            (head (title (pcdata "Custom data")) [])
            (body [
              h1 [pcdata "custom data"];
              div ~a:[a_onclick {{ show_my_data _ev }}] [
                ul [
                  li [pcdata "The following div \"click me ...\" contains a custom data for my_data."];
                  li [pcdata "The value of the div may be changed by the button below."];
                  li [pcdata "Click any of these lines or the div, to alert it's my_data custom data."];
                ];
                container;
              ];
              change_button;
            ])))

(******************************************************************************)

(******************************************************************************)
(*                          Client event handler syntax                       *)

{shared{

  let wrap str = "(wrap "^str^")"

  let shared_value = "shared_value"

  let shared_onclick context = {{
    let shared_value = "shared_inner_value" in
    ignore shared_value;
    let str =
      "shared_onclick from " ^ %context ^ ": \n"
        ^ " %shared_value=\"" ^ %shared_value ^ "\"\n"
        ^ " %(wrap shared_value)=\"" ^ %(wrap shared_value) ^ "\""
    in
    Dom_html.window##alert(Js.string str)
  }}

  let unused_handler : Dom_html.mouseEvent Html5.client_server_event_handler = {{
    (assert false)
  }}

}}

{client{

  let client_value = "client_value"

  let client_tests = [
    ( let client_onclick = {{
        let client_value = "client_inner_value" in
        ignore client_value;
        let str =
          "client_onclick: \n"
            ^ " %shared_value=\"" ^ %shared_value ^ "\"\n"
            ^ " %client_value=\"" ^ %client_value ^ "\"\n"
            ^ " %(wrap shared_value)=\"" ^ %(wrap shared_value) ^ "\"\n"
            ^ " %(wrap client_value)=\"" ^ %(wrap client_value) ^ "\"\n"
        in
        Dom_html.window##alert(Js.string str)
      }} in
      Html5.F.(
        li ~a:[a_onclick client_onclick]
          [ pcdata "client_onclick: %shared_value=\"shared_value\" %client_value=\"client_value\""]
      )
    );
    Html5.F.(
      li ~a:[a_onclick (shared_onclick "client")]
        [pcdata "shared_onclick from client: %shared_onclick=\"shared_value\""]
    );
  ]
}}

{server{

  let server_value = "server_value"

  let server_tests = [
    (
      let server_onclick = {{
        let shared_value = "shared_inner_server_value" in
        let server_value = "server_inner_value" in
        ignore (shared_value, server_value);
        let str =
          "server_onclick: \n"
            ^ " %shared_value=\"" ^ %shared_value ^ "\"\n"
            ^ " %server_value=\"" ^ %server_value ^ "\"\n"
            ^ " %(wrap shared_value)=\"" ^ %(wrap shared_value) ^ "\"\n"
            ^ " %(wrap server_value)=\"" ^ %(wrap server_value) ^ "\"\n"
        in
        Dom_html.window##alert(Js.string str)
      }} in
      Html5.F.(
        li ~a:[a_onclick server_onclick]
          [pcdata "server_onclick: %shared_value=\"shared_value\" %server_value=\"server_value\""]
      )
    );
    Html5.F.(
      li ~a:[a_onclick (shared_onclick "server")]
        [pcdata "shared_onclick from server: %shared_value=\"shared_value\""]
    );
  ]

}}

{shared{

  let shared_tests context = [
    Html5.F.(
      li ~a:[a_onclick (shared_onclick (context^" via shared"))] [
        pcdata
          ("shared_onclick from " ^ context ^ " via shared: " ^ "%shared_value=\"shared_value\"")
      ]
    )
  ]
}}

let client_handler_syntax =
  My_appl.register_service
    ~path:["client_handler_syntax"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let open Html5.F in
       let tests_id = Html5.Id.new_elt_id ~global:false () in
       Eliom_service.onload {{
         List.iter (Html5.Manip.Named.appendChild %tests_id) Html5.F.([
           div [pcdata "Client elements"];
           ul (client_tests);
         ]);
         List.iter (Html5.Manip.Named.appendChild %tests_id) Html5.F.([
           div [pcdata "Shared elements used from client"];
           ul (shared_tests "client");
         ]);
       }};
       Lwt.return
         (html
           (Eliom_tools.Html5.head ~title:"client_handler_syntax" ())
           (body [
             h1 [pcdata "Client event handler syntax"];
             Html5.Id.create_named_elt ~id:tests_id
               (div [
                 div [pcdata "Server elements"];
                 ul server_tests;
                 div [pcdata "Shared elements from server"];
                 ul (shared_tests "server")
               ])
           ])))

(******************************************************************************)

(******************************************************************************)
(*                        Client event handler syntax 2                       *)

{shared{

  type 'a with_id = {
    id : int;
    data : 'a;
  }

  type hidden_widget = {
    content : Html5_types.div_content Html5.elt list;
    widget_id : Html5_types.flow5 Html5.Id.id;
    overlay_id : Html5_types.div Html5.Id.id;
    container_id : Html5_types.div Html5.Id.id;
  }

}}

{client{

  type hidden_widget_client_data = {
    mutable show_callback : (unit -> unit) option;
    mutable content_getter : (unit -> Html5_types.div_content Html5.elt list Lwt.t) option;
  }
  let hidden_widget_client_data_default w = {
    show_callback = None;
    content_getter = None;
  }

  let hidden_widget_client_data = Hashtbl.create 13

  let get_hidden_widget_client_data w =
    try
      Hashtbl.find hidden_widget_client_data w.id
    with Not_found ->
      let data = hidden_widget_client_data_default w in
      Hashtbl.add hidden_widget_client_data w.id data;
      data
}}

{shared{

  let counter = ref 0
  let hidden_widget content = {
    id = (incr counter; !counter);
    data = {
      content = (content :> Html5_types.div_content Html5.elt list);
      widget_id = Html5.Id.new_elt_id ();
      overlay_id = Html5.Id.new_elt_id ();
      container_id = Html5.Id.new_elt_id ();
    }
  }

  let hidden_widget_html =
    fun w ->
      let open Html5.F in
      let container =
        Html5.Id.create_named_elt ~id:w.data.container_id
          (div ~a:[a_class ["container"]; a_style "display: none"] w.data.content)
      in
      let onclick_overlay = {{
        Html5.(Manip.SetCss.display (Id.get_element %(w.data.overlay_id)) "none");
        Html5.(Manip.SetCss.display (Id.get_element %(w.data.container_id)) "block");
        Option.iter (fun f -> f ()) (get_hidden_widget_client_data %w).show_callback;
        Option.iter
          (fun f ->
             Lwt.ignore_result
               (lwt content = f () in
                Html5.Manip.Named.replaceAllChild %(w.data.container_id) content;
                Lwt.return ()))
          (get_hidden_widget_client_data %w).content_getter;
        ()
      }} in
      let overlay =
        Html5.Id.create_named_elt ~id:w.data.overlay_id
          (div ~a:[
            a_class ["overlay"];
            a_onclick onclick_overlay;
          ] [ pcdata "click to show"; ])
      in
      Html5.Id.create_named_elt ~id:w.data.widget_id
        (div ~a:[a_class ["hidden_widget"]] [ overlay; container; ])

}}

{client{

  let hidden_widget_hide w =
    Html5.(Manip.SetCss.display (Id.get_element w.data.overlay_id) "block");
    Html5.(Manip.SetCss.display (Id.get_element w.data.container_id) "none");
    Html5.Manip.Named.replaceAllChild w.data.container_id w.data.content;
    ()

  let hidden_widget_set_show_callback w f =
    (get_hidden_widget_client_data w).show_callback <- Some f

  let hidden_widget_set_content_getter w f =
    (get_hidden_widget_client_data w).content_getter <- Some f

}}

let get_slow_content =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let sleep_time = 1.0 +. Random.float 4.0 in
       lwt () = Lwt_unix.sleep sleep_time in
       Lwt.return Html5.F.([
         (h2 [pcdata "Slow content"] :> Html5_types.div_content Html5.elt);
         p [pcdata (Printf.sprintf "Had to sleep %f seconds for this" sleep_time)];
       ]))

let client_handler_syntax_2 =
  My_appl.register_service
    ~path:["client_handler_syntax_2"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let w = hidden_widget [Html5.F.pcdata "waiting ..."] in
       let hide_button =
         Html5.D.(
           button ~a:[a_onclick {{ hidden_widget_hide %w }} ] ~button_type:`Submit [
             pcdata "Hide again";
           ])
       in
       Eliom_service.onload {{
         hidden_widget_set_show_callback %w
           (fun () ->
              ignore Dom_html.document##body##appendChild
                ((Html5.To_dom.of_element %hide_button :> Dom.node Js.t)));
         hidden_widget_set_content_getter %w
           (fun () ->
              Eliom_client.call_caml_service ~service: %get_slow_content () ())
       }};
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:"client_handler_syntax_2"
              ~css:[["style.css"]]
              ())
           (body [
             h1 [pcdata "Real world shared event handler"];
             hidden_widget_html w;
           ])))

(******************************************************************************)

