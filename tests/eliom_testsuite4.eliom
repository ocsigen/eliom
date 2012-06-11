
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



