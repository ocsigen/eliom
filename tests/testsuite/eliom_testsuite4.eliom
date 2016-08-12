
{shared{
  open Eliom_content
  open Eliom_lib
}}

{client{
  let () =
(*     Eliom_lib.set_tracing true; *)
    ()
}}

(******************************************************************************)

module Eliom_registration = Eliom_testsuite_base.Registration

let the_number = 100

let ocaml_service =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.unit
    (let rec counter = ref 0 in
     fun () () ->
       ignore {unit{
         Eliom_testsuite_base.log "From ocaml service 1";
       }};
       incr counter;
       if !counter mod 3 = 0 then
         Lwt.fail (Failure "Fails each 3rd time")
       else
         (ignore {unit{
            Eliom_testsuite_base.log "From ocaml service 2";
          }};
          Lwt.return the_number))

let test_client_value_on_caml_service =
  Eliom_testsuite_base.test
    ~title:"Client values in Ocaml-services"
    ~path:["holes"; "caml_service"]
    ~description:Html.F.([
      pcdata "On loading: \"From main service\"";
      br ();
      pcdata "On clicking button";
      ul [
        li [pcdata "\"From ocaml service 1\""];
        li [pcdata "\"From ocaml service 2\""];
        li [Printf.ksprintf pcdata "\"number: %d\"" the_number];
      ];
      pcdata "Each time clicking the button";
      ul [
        li [pcdata "\"From ocaml service 1\""];
        li [pcdata "Exception on server: Failure(\"Fails each 3rd time\")"];
      ]
    ])
    (fun () ->
       ignore {unit{
         Eliom_testsuite_base.log "From main service";
       }};
       let onclick = {{
         fun _ ->
           Lwt.ignore_result
             (try_lwt
                lwt number =
                  Eliom_client.call_ocaml_service %ocaml_service () () in
                Eliom_testsuite_base.log "number: %d" number;
                Lwt.return ()
              with Eliom_client_value.Exception_on_server msg ->
                Eliom_testsuite_base.log "Exception on server: %s" msg;
                Lwt.return ())
       }} in
       Lwt.return Html.F.([
         Form.button_no_value ~a:[a_onclick onclick ] ~button_type:`Submit [
           pcdata "Click to get ocaml service";
         ]
       ]))

(******************************************************************************)
(*                          Binding of escaped nodes                          *)

let free_global =
  Html.(Id.create_global_elt (D.div F.([b [pcdata "Global (free)"]])))
let bound_global =
  Html.(Id.create_global_elt (D.div F.([b [pcdata "Global (bound)"]])))
let free_request =
  Html.(D.div F.([b [pcdata "Request (free)"]]))
let bound_request =
  Html.(D.div F.([b [pcdata "Request (bound)"]]))

let other_service =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       ignore {unit{
           Lwt_log.ign_debug "on other service";
         Html.Manip.appendChild
           %free_request
           Html.F.(div [pcdata "from ocaml service"]);
         Html.Manip.appendChild
           %free_global
           Html.F.(div [pcdata "from ocaml service"]);
         Html.Manip.appendChild
           %bound_request
           Html.F.(div [pcdata "from ocaml service"]);
         Html.Manip.appendChild
           %bound_global
           Html.F.(div [pcdata "from ocaml service"]);
         ()
       }};
       Lwt.return ())

{client{
   Lwt_log.ign_debug "toplevel";
  Eliom_client.onload
    (fun () ->
       Lwt_log.ign_debug "onload";
       Html.Manip.appendChild
         %free_request
         Html.F.(div [pcdata "from client init"]);
       Html.Manip.appendChild
         %free_global
         Html.F.(div [pcdata "from client init"]);
       Html.Manip.appendChild
         %bound_request
         Html.F.(div [pcdata "from client init"]);
       Html.Manip.appendChild
         %bound_global
         Html.F.(div [pcdata "from client init"]);
       ())
}}

let addenda = Html.D.div []

let node_bindings_local_global_id = Html.Id.new_elt_id ~global:true ()
let node_bindings_local_request_id = Html.Id.new_elt_id ~global:false ()

let node_bindings =
  Eliom_testsuite_base.test
    ~title:"Binding of nodes"
    ~path:["holes"; "node_binding"]
    ~description:Html.F.([
      p [pcdata "Observe when HTML5 elements with DOM semantics are reused."];
      p [pcdata "Bound nodes are sent in the page; free nodes are added by client value side effect after loading the page."];
      ul [
        li [pcdata "Initially, every node receives an \"from client\""];
        li [pcdata "All four nodes should receive an \"onclick\" line when \"Add onclick lines\" is clicked."];
        li [pcdata "The free ones should reset if you visit the empty service and go back in history"];
        li [pcdata "The bound and free global nodes should receive a \"from ocaml service\" when \"Run Ocaml service\" is clicked"];
      ];
    ])
    (fun () ->
      let local_bound_global =
        Html.Id.create_named_elt ~id:node_bindings_local_global_id
          Html.(D.div [F.(b [pcdata "Global (bound, local)"])])
      in
      let local_bound_request =
        Html.Id.create_named_elt ~id:node_bindings_local_request_id
          Html.(D.div [F.(b [pcdata "Request (bound, local)"])])
      in
      ignore {unit{
          Lwt_log.ign_debug "Adding free";
         Html.Manip.appendChild %addenda %free_request;
         Html.Manip.appendChild %addenda %free_global;
         ignore %bound_global;
         ignore %bound_request;
         ignore %local_bound_global;
         ignore %local_bound_request;
         ()
       }};
       let add_onclick = {{
         fun _ ->
           Lwt_log.ign_debug "onclick";
           List.iter
             (fun node ->
               Html.Manip.appendChild node
                 Html.F.(div [pcdata "onclick"]))
             [%free_request; %free_global; %bound_request; %bound_global; %local_bound_global; %local_bound_request];
       }} in
       let run_ocaml_service = {{
         fun _ ->
           Lwt_log.ign_debug "run_ocaml_service";
           Lwt.ignore_result
             (Eliom_client.call_ocaml_service ~service: %other_service () ())
       }} in
       Lwt.return Html.F.([
         Eliom_testsuite_base.thebutton ~msg:"Add onclick lines" add_onclick;
         Eliom_testsuite_base.thebutton ~msg:"Run ocaml service" run_ocaml_service;
         local_bound_global;
         local_bound_request;
         bound_request;
         bound_global;
         addenda]);
       )

(******************************************************************************)
(*                                Data sharing                                *)

let data_sharing_elt1 = Html.D.(div ~a:[a_class ["monospace"]] [pcdata "VVVVVVVVVVV"])
let data_sharing_elt2 = Html.D.(div ~a:[a_class ["monospace"]] [pcdata "WWWWWWWWWWW"])
let data_sharing_elt3 = Html.D.(div ~a:[a_class ["monospace"]] [pcdata "XXXXXXXXXXX"])
let data_sharing_addenda = Html.D.div []

let data_sharing =
  Eliom_testsuite_base.test
    ~title:"Data sharing"
    ~path:["holes"; "data_sharing"]
    ~description:Html.F.([
      p [pcdata "Checks wheather data in the eliom request data is shared"];
      p [pcdata "The string of request data is given below."];
      p [pcdata "There are three elements on the server, which contain the strings
                 \"VVVVVVVVVVV\", \"WWWWWWWWWWW\", and \"XXXXXXXXXXX\" respectively.
                 All three elements are added to the DOM under \"Added from client\"."];
      p [pcdata "The string \"VVVVVVVVVVV\" should not appear in the request data,
                 because the corresponding element is sent as part of the DOM.
                 The string \"WWWWWWWWWWW\" and \"XXXXXXXXXXX\" should appear in the request data
                 exactly once."]
    ])
    (fun () ->
       let data_sharing_data = Html.D.div [] in
       ignore {unit{
         Html.Manip.appendChild %data_sharing_addenda
           %data_sharing_elt1;
         Html.Manip.appendChild %data_sharing_addenda
           %data_sharing_elt2;
       }};
       ignore {unit{
         Html.Manip.appendChild %data_sharing_data
           (Html.F.pcdata
              (Js.to_string (Js.Unsafe.get Js.Unsafe.global (Js.string "__eliom_request_data"))))
       }};
       Lwt.return Html.F.([
         data_sharing_elt1;
         section [ h4 [ pcdata "Added from client" ]; data_sharing_addenda ];
         section [ h4 [ pcdata "Request data" ]; data_sharing_data ];
       ]))

{client{
  let () =
    ignore (%data_sharing_elt1, %data_sharing_elt2);
    Eliom_client.onload
      (fun () ->
        Html.Manip.appendChild %data_sharing_addenda
          %data_sharing_elt3)
}}

(******************************************************************************)
(*                                Custom data                                 *)

{shared{

  type my_data = { x : int; y : int } deriving (Json)

  let my_data =
    Eliom_content.Html.Custom_data.create_json ~name:"my_int" ~default:{x=0;y=0;} Json.t<my_data>

}}

{client{

  let show_my_data (ev : Dom_html.mouseEvent Js.t) =
    let elt = Js.Opt.get (ev##target) (fun () -> failwith "show_my_data") in
    let i = Html.Custom_data.get_dom elt my_data in
    Dom_html.window##alert (Js.string (Printf.sprintf "custom_data : {x=%d;y=%d}" i.x i.y))

  let change_data container =
    let element = Html.To_dom.of_element container in
    let i = Html.Custom_data.get_dom element my_data in
    let i' = { x = succ i.x; y = pred i.y } in
    Html.Custom_data.set_dom element my_data i'
}}

let test_custom_data =
  let description = "Custom data: modification and defaults" in
  let path = ["custom_data"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let open Html.F in
       let container =
         Html.D.div ~a:[Html.Custom_data.attrib my_data { x = 100; y = 100 }]
           [pcdata "A: click me (my_data is originally {x=100;y=100})"]
       in
       let change_button =
         Form.button_no_value
           ~a:[a_onclick {{ fun _ -> change_data %container }}]
           ~button_type:`Submit
           [pcdata "In-/decrement my_data"]
       in
       Lwt.return
         (html
            (head (title (pcdata (String.concat "/" path))) [])
            (body [
              h1 [pcdata description];
              div ~a:[a_onclick {{ show_my_data }}] [
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

           (*
(******************************************************************************)
(*                          Client values: espaping                           *)
let client_values_injection =
  let v_a = "a" in
  let v_b = {string{ "b" }} in
  let v_c = {string{
    Printf.sprintf "(c a:%s)" %v_a
  }} in
  let v_d = {string{
    Printf.sprintf "(d b:%s)" %v_b
  }} in
  let v_f = {string->string{
    fun arg -> Printf.sprintf "(f arg:%s)" arg
  }} in
  let onclick str cstr = {{
    fun ev ->
      alert "onclick\n  \
             a:%s b:%s c:%s d:%s\n  \
             str:%s cstr:%s farg:%s"
        %v_a %v_b %v_c %v_d %str %cstr ( %v_f "arg")
  }} in
  let description = "Nested escaping of (client-) values into holes" in
  let path = ["client_values"; "injection"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let cstr = {string{ "cstr" }} in
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h1 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (onclick "str" cstr)]
               [ pcdata "Click me" ];
           ])
       ))
(******************************************************************************)


(******************************************************************************)
(*                         Client values: mutability                          *)
let client_values_mutability =
  let server_ref = ref 0 in
  let client_ref = {int ref{ ref 0 }} in
  let fresh_ref_client = {unit -> int ref{ fun () -> ref 0}} in
  let id = "fresh_client_ref" in
  let fresh_client_ref s = {int ref{ ignore %id; debug "fresh_client_ref: %s" %s; ref 0 }} in
  let s = "server" in
  let onclick = {{
    fun _ ->
      let s = "client" in
      let fresh_ref_client = %fresh_ref_client () in
      let fresh_client_ref = %(fresh_client_ref s) in
      incr %server_ref; incr %client_ref;
      incr fresh_ref_client; incr fresh_client_ref;
      debug "injected server reference: %d (increments)\n\
             injected client reference: %d (increments)\n\
             fresh reference client: %d (resets)\n\
             fresh client reference: %d (increments)"
        ! %server_ref ! %client_ref !fresh_ref_client !fresh_client_ref;
      incr %server_ref; incr %client_ref;
      incr fresh_ref_client; incr fresh_client_ref;
      debug "server reference_r: %d (increments)\n\
             injected client reference_r: %d (increments)\n\
             fresh reference client: %d (increments)\n\
             fresh client reference: %d (increments)"
        ! %server_ref ! %client_ref !fresh_ref_client !fresh_client_ref
  }} in
  let description = "Mutability of server values and injected holes" in
  let path = ["client_values"; "mutability"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h1 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick onclick]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.reload_action [pcdata "self"] ();
           ])
       ))
(******************************************************************************)

(******************************************************************************)
(*                      Client values: changing context                       *)
let client_values_changing_context =
  let client_ref = {int ref{ ref 100 }} in
  let client_f = {string->string{ debug "client_f once"; fun str -> Printf.sprintf "(client_f %s)" str }} in
  let client_hold str = {string{ debug "client_hold once: %s" %str; Printf.sprintf "(client_hold %s)" %str }} in
  let client_value = {string{ debug "client value, once"; "client_value" }} in
  let other_client_value = {string{ Printf.sprintf "--%s--" %client_value }} in
  let handler ix = {{
    debug "handler, once per page: %d" %ix;
    fun _ ->
      incr %client_ref;
      debug "handler, on click: ix:%d client_ref:%d client_value:%s other_client_value:%s (client_f ix):%s"
        %ix ! %client_ref %client_value %other_client_value ( %client_f (string_of_int %ix))
  }} in
  let description = "Changing context of client values" in
  let path = ["client_values"; "changing_context"] in
  let ix = ref 0 in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path ~get_params:Eliom_parameter.unit
    (fun () () ->
       incr ix;
       debug "ix: %d" !ix;
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (handler !ix)]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.reload_action
               [ pcdata "Reload, keep application running" ] ();
           ])
       ))

(******************************************************************************)

(******************************************************************************)
(*                 Client values: client value initialization                 *)
{client{
}}
let client_values_initialization =
  let name = "Observe when client values created by server holes are initialized" in
  let description = "" in
  let client_value_1 = {string{
    debug "init: client_value_1 (after each reload)";
    "client_value_1" }} in
  let client_value_2 = {string{
    debug "init: client_value_2 (after each reload)";
    Printf.sprintf "(client_value_2 %s)" %client_value_1
  }} in
  let client_function = {string->string{
    debug "init: client_function (after each reload)";
    fun str ->
      Printf.sprintf "(client_function %s)" str
  }} in
  let client_value_onhold () = {string{
    debug "init: client_value_onhold (after each click)";
    "client_value_onhold"
  }} in
  let onclick ix = {{
    debug "init: onclick";
    fun _ ->
      debug "client_value_1: %s\n  \
             client_value_2: %s\n  \
             (client_function %d): %s\n \
             (client_value_onhold ()): %s"
        %client_value_1 %client_value_2 %ix ( %client_function (string_of_int %ix))
                %(client_value_onhold ())
  }} in
  let path = ["client_values"; "initialization"] in
  name,
  Eliom_testsuite_base.My_appl.register_service
    ~path ~get_params:Eliom_parameter.unit
    (let ix = ref 0 in
     fun () () ->
       incr ix;
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata name];
             div ~a:[a_class ["description"]] [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (onclick !ix)]
               [ pcdata (Printf.sprintf "Click me (ix:%d)" !ix) ];
             a ~service:Eliom_service.reload_action ~xhr:true
               [ pcdata "Reload, keep application running" ] ();
           ])
       ))
(******************************************************************************)


(******************************************************************************)
(*                          Client value custom data                          *

{shared{
  let my_unit_unit = Html.Custom_data.create_client_value ~name:"my_unit_unit" ()
}}

{client{

  let run_element : _ Eliom_content.Html.elt -> unit -> unit =
    let f = Html.Custom_data.get_dom (To_dom.of_element trg) my_unit_unit in
    f ()

  let f () =
    debug "ffffffff"

  let g () =
    debug "ggggggg"
}}

{server{

  open Eliom_content

  let a = {string{ "a" }}
  let b = {string->string{ fun x -> "(b" ^ " " ^ %a ^ " " ^ x ^ ")" }}

  let my_div label cv =
    Html.F.(
      let onclick = {{
        fun ev -> run_element ev##target ()
      }} in
      div ~a:[a_onclick onclick; Html.Custom_data.attrib my_unit_unit cv] [
        pcdata label
      ]
    )

  let client_values =
    My_appl.register_service
      ~path:["client_values"]
      ~get_params:Eliom_parameter.unit
      (fun () () ->
         (* let w = 1 in *)
         Eliom_service.onload' {{ fun () -> debug "hic %s" ( %b "x") }};
         Lwt.return Html.F.(
           html
             (Eliom_tools.F.head
                ~title:"client_values"
                ())
             (body [
             ])
         ))

}}

******************************************************************************)

{shared{

  let elt src =
    ignore {unit{ debug "creating elt from %s" %src }};
    Html.F.(div ~a:[a_onclick {{ fun _ -> debug "click!"}}] [pcdata ("click ("^src^")")])

}}

let test_simple =
  let description = "test" in
  let path = ["client_values"; "simple"] in
  let v1 = "v1" in
  let v2 = {string{ debug "init v2"; "v2" }} in
  let v3 = {string{ Printf.sprintf "(v3 v1:%s)" %v1 }} in
  let v4 = {string{ Printf.sprintf "(v4 v2:%s)" %v2 }} in
  let v5 = {string{ Printf.sprintf "(v5 v4:%s)" %v4 }} in
  let v6 = {unit->string{ debug "init v6"; fun () -> Printf.sprintf "arg: %s %s %s %s %s" %v1 %v2 %v3 %v4 %v5 }} in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path ~get_params:Eliom_parameter.unit
    (fun () () ->
       let () = Eliom_service.onload {{
         fun _ ->
           Dom.appendChild
             (Dom_html.document##body)
             (Eliom_content.Html.To_dom.of_div (elt "client"))
       }} in
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick {{ debug "init handler"; fun _ -> debug "%s" ( %v6 ()) }}]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.reload_action [pcdata "self"] ();
             elt "server";
           ])
       ))

(******************************************************************************)
(*                          Client event handler syntax                       *)

{shared{

  let wrap str = "(wrap "^str^")"

  let shared_value = "shared_value"

  let shared_onclick context = {{
    debug "init shared_onclick from %s" %context;
    fun _ ->
      let shared_value = "shared_inner_value" in
      ignore shared_value;
      let str =
        "shared_onclick from " ^ %context ^ ": \n"
          ^ " %shared_value=\"" ^ %shared_value ^ "\"\n"
          ^ " %(wrap shared_value)=\"" ^ %(wrap shared_value) ^ "\""
      in
      Dom_html.window##alert(Js.string str)
  }}

  let unused_handler = {unit->unit{
    fun () -> (assert false)
  }}

}}

{client{

  let client_value = "client_value"

  let client_tests = [
    ( let client_onclick = {{
        debug "init client_onclick";
        fun _ ->
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
      Html.F.(
        li ~a:[a_onclick client_onclick]
          [ pcdata "client_onclick: %shared_value=\"shared_value\" %client_value=\"client_value\""]
      )
    );
    Html.F.(
      li ~a:[a_onclick (shared_onclick "client")]
        [pcdata "shared_onclick from client: %shared_onclick=\"shared_value\""]
    );
  ]

  let unused_handler_client = {unit->unit{
    fun () -> (assert false)
  }}
}}

{server{

  let server_value = "server_value"

  let server_tests = [
    (
      let server_onclick = {{
        debug "init server_onclick";
        fun _ ->
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
      Html.F.(
        li ~a:[a_onclick server_onclick]
          [pcdata "server_onclick: %shared_value=\"shared_value\" %server_value=\"server_value\""]
      )
    );
    Html.F.(
      li ~a:[a_onclick (shared_onclick "server")]
        [pcdata "shared_onclick from server: %shared_value=\"shared_value\""]
    );
  ]

  let unused_handler_server = {unit->unit{
    fun () -> (assert false)
  }}
}}

{shared{

  let shared_tests context = [
    Html.F.(
      li ~a:[a_onclick (shared_onclick (context^" via shared"))] [
        pcdata
          ("shared_onclick from " ^ context ^ " via shared: " ^ "%shared_value=\"shared_value\"")
      ]
    )
  ]
}}

let client_handler_syntax =
  let description = "Client event handler syntax" in
  let path = ["client_handler_syntax"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let open Html.F in
       let tests_id = Html.Id.new_elt_id ~global:false () in
       Eliom_service.onload {{
         fun _ ->
           List.iter (Html.Manip.Named.appendChild %tests_id) Html.F.([
             div [pcdata "Client elements"];
             ul (client_tests);
           ]);
           List.iter (Html.Manip.Named.appendChild %tests_id) Html.F.([
             div [pcdata "Shared elements used from client"];
             ul (shared_tests "client");
           ])
       }};
       Lwt.return
         (html
           (Eliom_tools.F.head ~title:(String.concat "/" path) ())
           (body [
             h1 [pcdata description];
             Html.Id.create_named_elt ~id:tests_id
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

  type hidden_widget = {
    content : Html_types.div_content Html.elt list;
    widget_id : Html_types.flow5 Html.Id.id;
    overlay_id : Html_types.div Html.Id.id;
    container_id : Html_types.div Html.Id.id;
    mutable show_callback : (unit -> unit) option;
    mutable content_getter : (unit -> Html_types.div_content Html.elt list Lwt.t) option;
    mutable set_content_thread : unit Lwt.t option;
  }

}}

{shared{

  let hidden_widget content = {
    content = (content :> Html_types.div_content Html.elt list);
    widget_id = Html.Id.new_elt_id ();
    overlay_id = Html.Id.new_elt_id ();
    container_id = Html.Id.new_elt_id ();
    show_callback = None;
    content_getter = None;
    set_content_thread = None;
  }

  let hidden_widget_html w =
    let open Html.F in
    let container =
      Html.Id.create_named_elt ~id:w.container_id
        (div ~a:[a_class ["container"]; a_style "display: none"] w.content)
    in
    let onclick_overlay = {{
      fun _ ->
        Html.(Manip.SetCss.display (Id.get_element %w.overlay_id) "none");
        Html.(Manip.SetCss.display (Id.get_element %w.container_id) "block");
        Option.iter (fun f -> f ()) %w.show_callback;
        Option.iter
          (fun f ->
             let waiter, wakener = Lwt.task () in
             let t =
               lwt () = waiter in
               lwt content = f () in
               Html.Manip.Named.replaceAllChild %w.container_id content;
               Lwt.return ()
             in
             Lwt.wakeup wakener ();
             %w.set_content_thread <- Some t)
          %w.content_getter;
      ()
    }} in
    let overlay =
      Html.Id.create_named_elt ~id:w.overlay_id
        (div ~a:[
          a_class ["overlay"];
          a_onclick onclick_overlay;
        ] [ pcdata "click to show"; ])
    in
    Html.Id.create_named_elt ~id:w.widget_id
      (div ~a:[a_class ["hidden_widget"]] [ overlay; container; ])

}}

{client{

  let hidden_widget_hide w _ =
    Html.Manip.Named.replaceAllChild w.container_id w.content;
    Html.(Manip.SetCss.display (Id.get_element w.overlay_id) "block");
    Html.(Manip.SetCss.display (Id.get_element w.container_id) "none");
    Option.iter (fun t -> Lwt.cancel t; w.set_content_thread <- None) w.set_content_thread;
    ()

  let hidden_widget_set_show_callback w f =
    w.show_callback <- Some f

  let hidden_widget_set_content_getter w f =
    w.content_getter <- Some f

}}

let get_slow_content =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let sleep_time = 1.0 +. Random.float 4.0 in
       debug "Sleep %f" sleep_time;
       lwt () = Lwt_unix.sleep sleep_time in
       Lwt.return Html.F.([
         (h2 [pcdata "Slow content"] :> Html_types.div_content Html.elt);
         p [pcdata (Printf.sprintf "Had to sleep %f seconds for this" sleep_time)];
       ]))

let client_handler_syntax_2 =
  Eliom_testsuite_base.My_appl.register_service
    ~path:["client_handler_syntax_2"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let w = hidden_widget [Html.F.pcdata "waiting ..."] in
       let hide_button =
         Html.D.(
           button ~a:[a_onclick {{ hidden_widget_hide %w }} ] ~button_type:`Submit [
             pcdata "Hide again";
           ])
       in
       let add_another_waiter_button =
         let onclick = {{
           fun _ ->
             let w = hidden_widget [Html.F.pcdata "Incredible content!"] in
             ignore (Dom.appendChild (Dom_html.document##body)
                       (Html.To_dom.of_element (hidden_widget_html w)))
         }} in
         Html.D.(
           button ~a:[a_onclick onclick ] ~button_type:`Submit [
             pcdata "Add another waiter widget";
           ])
       in
       Eliom_service.onload {{
         fun _ ->
           hidden_widget_set_show_callback %w
             (fun () ->
                ignore
                  (Dom.insertBefore
                     (Dom_html.document##body)
                     (Html.To_dom.of_element %hide_button)
                     Js.null));
           hidden_widget_set_content_getter %w
             (fun () ->
                Eliom_client.call_caml_service ~service: %get_slow_content () ())
       }};
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:"client_handler_syntax_2"
              ~css:[["style.css"]] ())
           (body [
             h1 [pcdata "Real world shared event handler"];
             hidden_widget_html w;
             add_another_waiter_button;
           ])))

(******************************************************************************)

{shared{
  let shared_onclick source = {{
    debug "init shared_onclick from %s" %source;
    fun _ ->
      debug "shared_onclick from %s" %source
  }}
}}

{client{

}}

let client_values_shared =
  let description = "shared client values" in
  let path = ["client_values"; "shared"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path ~get_params:Eliom_parameter.unit
    (fun () () ->
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (shared_onclick "server")]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.reload_action [pcdata "self"] ();
           ])
       ))

(******************************************************************************)
(*                            Client values onload                            *)

{shared{
  let shared_elt src =
    ignore {unit{
      debug "init shared onload from ??"  (* %src *);
      Lwt.ignore_result
        (lwt () = Eliom_client.wait_load_end () in
         debug "shared onload from %s" %src;
         Lwt.return ())
    }};
    Html.F.(div [pcdata ("shared elt from "^src)])
}}

{client{
  let client_elt =
    ignore {unit{
      debug "init client onload" ;
      Lwt.ignore_result
        (lwt () = Eliom_client.wait_load_end () in
         debug "client onload";
         Lwt.return ())
    }};
    Html.F.(div [pcdata "client elt"])
}}

{server{
  let server_elt =
    ignore {unit{
      debug "init server onload";
      Lwt.ignore_result
        (lwt () = Eliom_client.wait_load_end () in
         debug "server onload";
         Lwt.return ())
    }};
    Html.F.(div [pcdata "server elt"])
}}


let client_values_onload =
  let description = "client values onload" in
  let path = ["client_values"; "onload"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path ~get_params:Eliom_parameter.unit
    (fun () () ->
       ignore {unit{
         Lwt.ignore_result
           (lwt () = Eliom_client.wait_load_end () in
            Dom.appendChild
              (Dom_html.document##body)
              (Html.To_dom.of_div (shared_elt "client"));
            Dom.appendChild
              (Dom_html.document##body)
              (Html.To_dom.of_div client_elt);
            Lwt.return ())
       }};
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (shared_onclick "server")]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.reload_action [pcdata "self"] ();
             server_elt;
             shared_elt "server";
           ])
       ))
(******************************************************************************)

(******************************************************************************)
(*                             Escaped in client                              *)

let time () =
  Int64.of_float (Unix.gettimeofday () *. 1000.0)

let global_value =
  time ()

let client_process, request =
  let client_process_counter = ref 0 in
  let request_counter = ref 0 in
  Eliom_reference.eref_from_fun ~scope:Eliom_common.client_process
    (fun () ->
       request_counter := 0;
       incr client_process_counter;
       !client_process_counter),
  Eliom_reference.Volatile.eref_from_fun ~scope:Eliom_common.request
    (fun () ->
       incr request_counter;
       !request_counter)

let s = Eliom_registration.Action.register_coservice'
          ~get_params:Eliom_parameter.unit
          (fun () () ->
             debug "escaped_in_client: the action";
             Lwt.return ())

{client{


  let show_server_injections _ =
    debug "global_value: %Ld\n\
           client_process: %d\n\
           request: %d\n\
           time: %Ld"
      %global_value
      %client_process
      %request
      %(time ())

  let () = debug "global_value: %Ld" %global_value

  let link () =
     Html.F.(div [(*a ~service: %s [pcdata "Action! (on server)"] ()*)])

}}

let escaped_in_client =
  let description = "Escaping server values on the client" in
  let path = ["escape"; "on_client"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path ~get_params:Eliom_parameter.unit
    (fun () () ->
       lwt cp = Eliom_reference.get client_process in
       debug "global_value: %Ld\n\
              client_process: %d\n\
              request: %d\n\
              time: %Ld"
         global_value
         cp
         (Eliom_reference.Volatile.get request)
         (time ());
       ignore {unit{
         Lwt.ignore_result
           (lwt () = Eliom_client.wait_load_end () in
            show_server_injections ();
            Dom.appendChild
              (Dom_html.document##body)
              (Html.To_dom.of_element (link ()));
            Lwt.return ())
       }};
       Lwt.return Html.F.(
         html
           (Eliom_tools.F.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick {{ show_server_injections }}]
               [ pcdata "Click me" ];
             div [ a ~service:Eliom_service.reload_action [pcdata "reload in app"] () ];
           ])
       ))
(******************************************************************************)

(******************************************************************************)
(*                             Client value injection                         *)

{client{
  let log = Eliom_testsuite_base.log
}}

let server_constant = Random.int 100

let request_reference =
  Eliom_reference.Volatile.eref_from_fun ~scope:Eliom_common.request
    (fun () -> Random.int 100)

let deep_client_value = Some {string{ "client value" }}

let an_elt = Html.D.div []

{client{

  let () =
    Lwt.ignore_result
      (lwt () = Eliom_client.wait_load_end () in
       debug "---> !!! 2";
       Html.Manip.appendChild
         %an_elt
         Html.F.(pcdata "!!! 2");
       Lwt.return ());
    log "client TOP"

  let f () =
    log "server_constant: %d" %server_constant;
    log "request_reference: %d" %request_reference;
    match %deep_client_value with
      | Some s ->
          log "deep client string: %s" s
      | None -> ()

  let () = f ()

}}

let deep_client_values =
  let counter = ref 0 in
  Eliom_testsuite_base.test
    ~title:"Client value injections"
    ~path:["client"; "injections"]
    ~description:Html.F.([
      pcdata
        ""
    ])
    (fun () ->
       debug "server_constant: %d" server_constant;
       debug "request_reference: %d" (Eliom_reference.Volatile.get request_reference);
       let request_counter = incr counter; Printf.sprintf "--%d--" !counter in
       let onclick = {{
         (* FIXME BB enable escaping of Eliom_references *)
         (* debug "----> request_reference': %d" %request_reference; *)
         begin match %deep_client_value with
           | Some client_value ->
               log "deep client string: %s" client_value
           | None -> assert false
         end;
         f ();
         fun _ ->
           log "request_counter: %s" %request_counter
       }} in
       Eliom_service.onload {{
         fun _ ->
           debug "---> !!! 1";
           Html.Manip.appendChild
             %an_elt
             Html.F.(pcdata "!!! 1")
       }};
       Lwt.return Html.F.([
         div [a ~service:Eliom_service.reload_action [pcdata "reload in app"] ()];
         div [
           button ~a:[a_onclick onclick ] ~button_type:`Submit [
             pcdata "Click";
           ]
         ];
         an_elt;
       ]))

(******************************************************************************)
           *)

(* XXX Two times the same code, once in shared (with variable names postix
   _shared and once in client (with variable names postfix _client). *)

{server{
  let injection_scoping_shared_v1 = "server1"
}}
{shared{
  let injection_scoping_shared_v1 = "shared1"
  let injection_scoping_shared () =
    Eliom_testsuite_base.assert_equal
      ~name:"injection_scoping_shared_v1"
      injection_scoping_shared_v1 "shared1";
    Lwt_log.ign_debug_f "%%injection_scoping_shared_v1=%s (server1)" %injection_scoping_shared_v1;
    Eliom_testsuite_base.assert_equal
      ~name:"%injection_scoping_shared_v1"
      %injection_scoping_shared_v1 "server1";
    ()
}}

{server{
  let injection_scoping_client_v1 = "server1"
}}
{client{
  let injection_scoping_client_v1 = "client1"
  let injection_scoping_client () =
    Eliom_testsuite_base.assert_equal
      ~name:"injection_scoping_client_v1"
      injection_scoping_client_v1 "client1";
    Eliom_testsuite_base.assert_equal
      ~name:"%injection_scoping_client_v1"
      %injection_scoping_client_v1 "server1";
    ()
}}

let test_injection_scoping =
  let title = "Scoping of injections in client/shared-section" in
  Eliom_testsuite_base.test
    ~title
    ~path:["holes"; "injection_scoping"]
    ~description:Html.F.([
      p [pcdata "Test, which value is referenced by ";
         Eliom_testsuite_base.monospace "v";
         pcdata " and an injection ";
         Eliom_testsuite_base.monospace "%%v";
         pcdata " in cases like:"];
      pre [pcdata "\
{server{\n
  let v = ...\n
}}\n
{client/shared{\n
  let v = ...\n
  let _ = v, %v (* <-- here *)
}}"];
      p [pcdata "There must be 4 tests mentioned in the client logger (two each \
                 from testing shared and client), and 2 in the server output."];
    ])
    (fun () ->
       injection_scoping_shared ();
       Eliom_testsuite_base.report_flush_assertions title;
       ignore {unit{
         injection_scoping_shared ();
         injection_scoping_client ();
         Eliom_testsuite_base.report_flush_assertions %title;
       }};
       Lwt.return Html.F.([]))

(******************************************************************************)

(* XXX Two times the same code, once in shared (with variable names postix
   _shared and once in server (with variable names prefixed by
   escaping_scoping_server_). *)

{client{
  let escaping_scoping_server_v1 = "client1"
}}
{server{
  let escaping_scoping_server_v1 = "server1"
  let escaping_scoping_server () = ignore {unit{
    Eliom_testsuite_base.assert_equal
      ~name:"escaping_scoping_server_v1"
      escaping_scoping_server_v1 "client1";
    Eliom_testsuite_base.assert_equal
      ~name:"%escaping_scoping_server_v1"
      %escaping_scoping_server_v1 "server1";
    let escaping_scoping_server_v1 = "inner1" in
    Eliom_testsuite_base.assert_equal
      ~name:"escaping_scoping_server_v1 (with inner)"
      escaping_scoping_server_v1 "inner1";
    Eliom_testsuite_base.assert_equal
      ~name:"%escaping_scoping_server_v1 (with inner)"
      %escaping_scoping_server_v1 "server1";
    ()
  }}
}}

{client{
  let escaping_scoping_shared_v1 = "client1"
}}
{shared{
  let escaping_scoping_shared_v1 = "shared1"
  let escaping_scoping_shared () = ignore {unit{
    Eliom_testsuite_base.assert_equal
      ~name:"escaping_scoping_shared_v1"
      escaping_scoping_shared_v1 "client1";
    Eliom_testsuite_base.assert_equal
      ~name:"%escaping_scoping_shared_v1"
      %escaping_scoping_shared_v1 "shared1";
    let escaping_scoping_shared_v1 = "inner1" in
    Eliom_testsuite_base.assert_equal
      ~name:"escaping_scoping_shared_v1 (with inner)"
      escaping_scoping_shared_v1 "inner1";
    Eliom_testsuite_base.assert_equal
      ~name:"%escaping_scoping_shared_v1 (with inner)"
      %escaping_scoping_shared_v1 "shared1";
    ()
  }}
}}

let test_escaping_scoping =
  let title = "Scoping of escaped variables in server/shared-section" in
  Eliom_testsuite_base.test ~title
    ~path:["holes"; "escaping_scoping"]
    ~description:Html.F.([
      p [pcdata "Test, which value is referenced by ";
         Eliom_testsuite_base.monospace "v";
         pcdata " and an escaped variable ";
         Eliom_testsuite_base.monospace "%%v";
         pcdata " in cases like:"];
      pre [pcdata "\
{client{\n
  let v = ...\n
}}\n
{server/shared{\n
  let v = ...\n
  let _ = {{
    v, %v (* <-- here *)
  }}
}}"];
      p [pcdata "There must be 12 tests mentioned in the client logger (4 from \
                 testing the server-section, and each 4 from testing the shared-section \
                 in client and server), and 0 in the server output."];
    ])
    (fun () ->
       escaping_scoping_shared ();
       escaping_scoping_server ();
       Eliom_testsuite_base.report_flush_assertions title;
       ignore {unit{
         escaping_scoping_shared ();
         Eliom_testsuite_base.report_flush_assertions %title;
       }};
       Lwt.return Html.F.([]))

(******************************************************************************)

let test_server_function =
  let f str =
    if str = "" then
      Lwt.fail (Failure "Empty string")
    else
      let strstr = str ^ str in
      Lwt_log.ign_debug_f "test_server_function: received %S sending %S" str strstr;
      Lwt.return (str ^ str)
  in
  let rpc_f = Eliom_client.server_function Json.t<string> f in
  Eliom_testsuite_base.test
    ~title:"RPC / server functions"
    ~path:["mixed"; "server_function"]
    ~description:Html.F.([
      pcdata
        "Server functions make functions from the server available on the client.";
      br ();
      pcdata
        "Click the button to send the content of the field to the server, where it \
         logged to the console, and sent back doubled (in the client logger)";
      pcdata
        "If you send the empty string, however, an exception is raised on the server.";
    ])
    (fun () ->
       let field = Html.D.input ~a:[Html.D.a_input_type `Text] () in
       let onclick = {{
         fun _ ->
           let field_dom = Html.To_dom.of_input %field in
           let str = Js.to_string field_dom##value in
           field_dom##value <- Js.string "";
           Lwt.async
             (fun () ->
               try_lwt
                 lwt strstr = %rpc_f str in
                 Eliom_testsuite_base.log "Sent %S received %S" str strstr;
                 Lwt.return ()
               with Eliom_client_value.Exception_on_server str ->
                 Eliom_testsuite_base.log "Exception on server: %s" str;
                 Lwt.return ())
       }} in
       Lwt.return Html.F.([
         Eliom_testsuite_base.thebutton ~msg:"send" onclick;
         br ();
         field;
       ]))

(******************************************************************************)
{server{
  let () = ignore {unit{ Eliom_testsuite_base.log "STEP 0" }}
  let client_value_initialization_a = "1"
  let client_value_initialization_b = "2"
}}
{client{
  let client_value_initialization_x1 = 2
  let () = Eliom_testsuite_base.log "STEP %s" %client_value_initialization_a
  let () = Eliom_testsuite_base.log "STEP %s" %client_value_initialization_b
}}
{server{
  let () = ignore {unit{ Eliom_testsuite_base.log "STEP 3" }}
  let client_value_initialization_f (x : int Eliom_client_value.t)
    : unit Eliom_client_value.t =
    {{ Eliom_testsuite_base.log "STEP %d" %x }}
  let client_value_initialization_y1 =
    client_value_initialization_f {{ client_value_initialization_x1 }}
}}
{client{
  let client_value_initialization_x2 = 5
  let client_value_initialization_f2 () =
    let () = ignore %client_value_initialization_a in
    let () = ignore %client_value_initialization_a in
    let () = ignore %(Lwt_log.ign_debug "STEP 0") in
    let () = ignore %(Lwt_log.ign_debug "STEP 1") in
    ()
}}
{server{
  let client_value_initialization_y2 =
    client_value_initialization_f
      {{ client_value_initialization_x2 }}
  let () = ignore {unit{ Eliom_testsuite_base.log "STEP 6" }}
}}
let client_value_initialization =
  Eliom_testsuite_base.test
    ~title:"Order of initializations of client values"
    ~path:["holes"; "client_value_initialization"]
    ~description:Html.F.([
      p [pcdata "The client logger should show the STEPs 0-6"];
      p [pcdata "The server output should show STEPs 0-1"];
    ])
    (fun () ->
       ignore {unit{ client_value_initialization_f2 () }};
       Lwt.return [])

(******************************************************************************)

let wrap_handler =
  let state = Eliom_reference.eref ~scope:Eliom_common.default_session_scope None in
  let service =
    Eliom_testsuite_base.My_appl.register_coservice'
      ~get_params:Eliom_parameter.unit
      (Eliom_tools.wrap_handler
         (fun () -> Eliom_reference.get state)
         (fun () () -> Lwt.return
           Html.F.(html (head (title (pcdata "not set")) [])
                      (body [pcdata "not set"])))
         (fun value () () -> Lwt.return
           Html.F.(html (head (title (pcdata "set")) [])
                      (body [Printf.ksprintf pcdata "set to %d." value]))))
  in
  let set_state =
    let counter = ref 0 in
    Eliom_registration.Unit.create
      ~path:Eliom_service.No_path
      ~meth:(Eliom_service.Get Eliom_parameter.unit)
      (fun () () ->
        lwt () = Eliom_reference.set state (incr counter; Some !counter) in
        Lwt.return ())
  in
  let unset_state =
    Eliom_registration.Unit.create
      ~path:Eliom_service.No_path
      ~meth:(Eliom_service.Get Eliom_parameter.unit)
      (fun () () ->
        lwt () = Eliom_reference.set state None in
        Lwt.return ())
  in
  Eliom_testsuite_base.test
    ~title:"Wrap handler"
    ~path:["mixed"; "wrap_handler"]
    ~description:Html.F.([
      pcdata "The links 'set state' and 'unset state' allow to modify a state.";
      pcdata "The link 'test state' show whether the state is set or not.";
    ])
    (fun () -> Lwt.return Html.F.([
      ul [
        li [a ~service [pcdata "test state"] ()];
        li [a ~service:set_state [pcdata "set state"] ()];
        li [a ~service:unset_state [pcdata "unset state"] ()];
      ]]))

(******************************************************************************)
{client{ Eliom_config.set_tracing true }}
let cross_change_page_client_values =
  let global_client_ref = {string ref{ ref "initial" }} in
  Eliom_testsuite_base.test
    ~title:"Cross change page client values"
    ~path:["holes";"cross_change_page"]
    ~description:Html.F.([
      ul [
        li [pcdata "The server keeps a global client value with a client reference. \
                    It is logged on each change_page (reload within application). \
                    The server adds an prime to that string on each reload"]
      ]
    ])
    (fun () ->
      ignore {unit{
        Eliom_testsuite_base.log "Global client reference is %S" ! %global_client_ref;
        %global_client_ref := ! %global_client_ref ^ "'"
      }};
      Lwt.return Html.F.([
      ]))

(******************************************************************************)
let ocaml_service_sleep =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt_unix.sleep 10.)

let test_ocaml_service_timeout =
  Eliom_testsuite_base.test
    ~title:"Timeout for ocaml services"
    ~path:["timeout_ocaml_service"]
    ~description:Html.F.([
      pcdata "Demo on how to put a timeout on an Ocaml service call.";
    ])
    (fun () ->
       ignore {unit{
         Lwt.async (fun () ->
           try_lwt
             lwt () =
               Lwt.pick
                 [Eliom_client.call_ocaml_service %ocaml_service_sleep () ();
                  lwt () = Lwt_js.sleep 2. in Lwt.fail (Failure "timeout")]
             in
             Eliom_testsuite_base.log "Data received without timeout";
             Lwt.return ()
           with Failure _ ->
             Eliom_testsuite_base.log "Timeout reached";
             Lwt.return ())
       }};
       Lwt.return Html.F.([]))
(*****************************************************************************)

let tests = [
  "Mixed", [
    test_custom_data;
    test_server_function;
    wrap_handler;
    test_ocaml_service_timeout;
  ];
  "Holes", [
    test_injection_scoping;
    test_escaping_scoping;
    test_client_value_on_caml_service;
    node_bindings;
    data_sharing;
    client_value_initialization;
    cross_change_page_client_values;
(*
   test_simple;
   client_values_injection;
   client_values_mutability;
   client_values_changing_context;
   client_values_initialization;
   client_values_shared;
   client_handler_syntax;
   client_values_onload;
   escaped_in_client;
   deep_client_values;
 *)
  ];
]
