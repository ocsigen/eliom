
{shared{
  open Eliom_content
  open Eliom_lib
}}

{client{
  let () =
(*     Eliom_lib.set_tracing true; *)
    ()
}}

{server{

  let value = "server_value"

  let server_client_value : string client_value =
    let value = "local_value" in
    {{
      let value = "inner_local" in
      Printf.sprintf "(server_client_value %s)" %value
    }}

}}

{shared{

  let value2 = "shared_value"

  let shared_client_value : string client_value =
    let value2 = "local_value" in
    {{
      let value2 = "inner_local" in
      Printf.sprintf "(shared_client_value %s)" %value2
    }}

}}

let test_escape_scoping =
  Eliom_testsuite_base.test
    ~title:"Scoping of escaped variables"
    ~path:["holes"; "scoping"; "escaped"]
    ~description:Html5.F.([
      pcdata "Test the scoping of escaped variables in client values";
      br ();
      pcdata "Total of 5 tests";
    ])
    (fun () ->
       Eliom_testsuite_base.assert_equal ~name:"value"
         value "server_value";
       Eliom_testsuite_base.assert_equal ~name:"value2"
         value2 "shared_value";
       ignore {unit{
         Eliom_testsuite_base.log "Running tests";
         Eliom_testsuite_base.assert_equal ~name:"%server_client_value"
           %server_client_value "(server_client_value local_value)";
         Eliom_testsuite_base.assert_equal ~name:"shared_client_value"
           shared_client_value "(shared_client_value local_value)";
         Eliom_testsuite_base.assert_equal ~name:"%shared_client_value"
           %shared_client_value "(shared_client_value local_value)";
         ()
       }};
       let ran_assertions = Eliom_testsuite_base.get_ran_assertions () in
       let failed_assertions = Eliom_testsuite_base.get_failed_assertions () in
       let aux msg li = List.map (fun str -> Html5.F.(li [pcdata (Printf.sprintf "Test %s %s" str msg)])) li in
       Lwt.return Html5.F.([
         h3 [pcdata "Tests run"];
         ul (aux "ok" ran_assertions);
       ] @
         if failed_assertions <> []
         then [
           h3 [pcdata "Tests failed"];
           ul (aux "failed" failed_assertions);
         ] else []
       ))

{shared{
  let value3 = "shared_value"
}}

{shared{
  let value3 = "shared_value_2"
  let shared_value3 = %value3
}}

{client{
  let value3 = "client_value"
  let client_value3 = %value3
}}

let test_injection_scoping =
  Eliom_testsuite_base.test
    ~title:"Scoping of injected variables"
    ~path:["holes"; "scoping"; "injection"]
    ~description:Html5.F.([
      pcdata "Test the scoping injected variables in the client section";
      br ();
      pcdata "Total of 5 tests";
    ])
    (fun () ->
       Eliom_testsuite_base.assert_equal ~name:"value3"
         value3 "shared_value_2";
       Eliom_testsuite_base.assert_equal ~name:"shared_value3"
         shared_value3 "shared_value";
       ignore {unit{
         Eliom_testsuite_base.assert_equal ~name:"value3"
           value3 "client_value";
         Eliom_testsuite_base.assert_equal ~name:"shared_value3"
           %shared_value3 "shared_value";
         Eliom_testsuite_base.assert_equal ~name:"client_value3"
           client_value3 "shared_value_2";
       }};
       let ran_assertions = Eliom_testsuite_base.get_ran_assertions () in
       let failed_assertions = Eliom_testsuite_base.get_failed_assertions () in
       let aux msg li = List.map (fun str -> Html5.F.(li [pcdata (Printf.sprintf "Test %s %s" str msg)])) li in
       Lwt.return Html5.F.([
         h3 [pcdata "Tests run"];
         ul (aux "ok" ran_assertions);
       ] @
         if failed_assertions <> []
         then [
           h3 [pcdata "Tests failed"];
           ul (aux "failed" failed_assertions);
         ] else []
       ))

(******************************************************************************)

let the_number = 100

let ocaml_service =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       ignore {unit{
         Eliom_testsuite_base.log "From ocaml service";
       }};
       Lwt.return the_number)
 
let test_client_value_on_caml_service =
  Eliom_testsuite_base.test
    ~title:"Client values in Ocaml-services"
    ~path:["holes"; "caml_service"]
    ~description:Html5.F.([
      pcdata "On loading: \"From main service\"";
      br ();
      pcdata "On click button";
      ul [
        li [pcdata "\"From ocaml service\""];
        li [Printf.ksprintf pcdata "\"number: %d\"" the_number];
      ]
    ])
    (fun () ->
       ignore {unit{
         Eliom_testsuite_base.log "From main service";
       }};
       let onclick = {{
         fun _ ->
           Lwt.ignore_result
             (lwt number = Eliom_client.call_caml_service %ocaml_service () () in
              Eliom_testsuite_base.log "number: %d" number;
              Lwt.return ())
       }} in
       Lwt.return Html5.F.([
         button ~a:[a_onclick onclick ] ~button_type:`Submit [
           pcdata "Click to get ocaml service";
         ]
       ]))

(******************************************************************************)
(*                          Binding of escaped nodes                          *)

let free_global =
  Html5.(Id.create_global_elt (D.div F.([b [pcdata "Persistent (free)"]])))
let bound_global =
  Html5.(Id.create_global_elt (D.div F.([b [pcdata "Persistent (bound)"]])))
let free_request =
  Html5.(D.div F.([b [pcdata "Reset each request (free)"]]))
let bound_request =
  Html5.(D.div F.([b [pcdata "Reset each request (bound)"]]))

(*
let other_service =
  Eliom_registration.Ocaml.register_coservice'
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       ignore {unit{
         debug "on other service";
         Html5.Manip.appendChild
           %free_request
           Html5.F.(div [pcdata "onclick"]);
         Html5.Manip.appendChild
           %free_global
           Html5.F.(div [pcdata "onclick"]);
         Html5.Manip.appendChild
           %bound_request
           Html5.F.(div [pcdata "onclick"]);
         Html5.Manip.appendChild
           %bound_global
           Html5.F.(div [pcdata "onclick"]);
         ()
       }};
       Lwt.return ())
 *)

{client{
  Eliom_client.onload
    (fun () ->
       Html5.Manip.appendChild
         %free_request
         Html5.F.(div [pcdata "from client"]);
       Html5.Manip.appendChild
         %free_global
         Html5.F.(div [pcdata "from client"]);
       Html5.Manip.appendChild
         %bound_request
         Html5.F.(div [pcdata "from client"]);
       Html5.Manip.appendChild
         %bound_global
         Html5.F.(div [pcdata "from client"]);
       ())
}}

let node_bindings =
  Eliom_testsuite_base.test
    ~title:"Binding of nodes"
    ~path:["holes"; "node_binding"]
    ~description:Html5.F.([
      p [pcdata "Observe when HTML5 elements with DOM semantics are reused."];
      p [pcdata "Bound nodes are sent in the page; free nodes are added by client value side effect after loading the page."];
      ul [
        li [pcdata "All four nodes should receive an \"onclick\" line when THE BUTTON is clicked."];
        li [pcdata "The free ones should reset on each request"];
      ];
    ])
    (fun () ->
       ignore {unit{
         debug "init free adder";
         Eliom_client.onload
           (fun () ->
              debug "Adding free";
              Html5.Manip.appendChild
                (Html5.Of_dom.of_element Dom_html.document##body)
                %free_request;
              Html5.Manip.appendChild
                (Html5.Of_dom.of_element Dom_html.document##body)
                %free_global;
           )
       }};
       let onclick = {{
         fun _ ->
           debug "onclick";
           Html5.Manip.appendChild
             %free_request
             Html5.F.(div [pcdata "onclick"]);
           Html5.Manip.appendChild
             %free_global
             Html5.F.(div [pcdata "onclick"]);
           Html5.Manip.appendChild
             %bound_request
             Html5.F.(div [pcdata "onclick"]);
           Html5.Manip.appendChild
             %bound_global
             Html5.F.(div [pcdata "onclick"]);
           ()
       }} in
       Lwt.return Html5.F.( [
         (* p [a ~service:other_service [pcdata "action other service"] ()]; *)
         button ~a:[a_class ["thebutton"]; a_onclick onclick] ~button_type:`Submit
           [ pcdata "THE BUTTON" ];
         bound_request;
         bound_global;
       ]))
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

let test_custom_data =
  let description = "Custom data: modification and defaults" in
  let path = ["custom_data"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let open Html5.F in
       let container =
         Html5.D.div ~a:[Html5.Custom_data.attrib my_data { x = 100; y = 100 }]
           [pcdata "A: click me (my_data is originally {x=100;y=100})"]
       in
       let change_button =
         button ~a:[a_onclick {{ fun _ -> change_data %container }}] ~button_type:`Submit
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
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
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
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h1 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick onclick]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.void_coservice' [pcdata "self"] ();
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
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (handler !ix)]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.void_coservice'
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
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata name];
             div ~a:[a_class ["description"]] [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (onclick !ix)]
               [ pcdata (Printf.sprintf "Click me (ix:%d)" !ix) ];
             a ~service:Eliom_service.void_coservice' ~xhr:true
               [ pcdata "Reload, keep application running" ] ();
           ])
       ))
(******************************************************************************)


(******************************************************************************)
(*                          Client value custom data                          *

{shared{
  let my_unit_unit = Html5.Custom_data.create_client_value ~name:"my_unit_unit" ()
}}

{client{

  let run_element : _ Eliom_content.Html5.elt -> unit -> unit =
    let f = Html5.Custom_data.get_dom (To_dom.of_element trg) my_unit_unit in
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
    Html5.F.(
      let onclick = {{
        fun ev -> run_element ev##target ()
      }} in
      div ~a:[a_onclick onclick; Html5.Custom_data.attrib my_unit_unit cv] [
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
         Lwt.return Html5.F.(
           html
             (Eliom_tools.Html5.head
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
    Html5.F.(div ~a:[a_onclick {{ fun _ -> debug "click!"}}] [pcdata ("click ("^src^")")])

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
             (Eliom_content.Html5.To_dom.of_div (elt "client"))
       }} in
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick {{ debug "init handler"; fun _ -> debug "%s" ( %v6 ()) }}]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.void_coservice' [pcdata "self"] ();
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

  let unused_handler_server = {unit->unit{
    fun () -> (assert false)
  }}
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
  let description = "Client event handler syntax" in
  let path = ["client_handler_syntax"] in
  description,
  Eliom_testsuite_base.My_appl.register_service
    ~path
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let open Html5.F in
       let tests_id = Html5.Id.new_elt_id ~global:false () in
       Eliom_service.onload {{
         fun _ ->
           List.iter (Html5.Manip.Named.appendChild %tests_id) Html5.F.([
             div [pcdata "Client elements"];
             ul (client_tests);
           ]);
           List.iter (Html5.Manip.Named.appendChild %tests_id) Html5.F.([
             div [pcdata "Shared elements used from client"];
             ul (shared_tests "client");
           ])
       }};
       Lwt.return
         (html
           (Eliom_tools.Html5.head ~title:(String.concat "/" path) ())
           (body [
             h1 [pcdata description];
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

  type hidden_widget = {
    content : Html5_types.div_content Html5.elt list;
    widget_id : Html5_types.flow5 Html5.Id.id;
    overlay_id : Html5_types.div Html5.Id.id;
    container_id : Html5_types.div Html5.Id.id;
    mutable show_callback : (unit -> unit) option;
    mutable content_getter : (unit -> Html5_types.div_content Html5.elt list Lwt.t) option;
    mutable set_content_thread : unit Lwt.t option;
  }

}}

{shared{

  let hidden_widget content = {
    content = (content :> Html5_types.div_content Html5.elt list);
    widget_id = Html5.Id.new_elt_id ();
    overlay_id = Html5.Id.new_elt_id ();
    container_id = Html5.Id.new_elt_id ();
    show_callback = None;
    content_getter = None;
    set_content_thread = None;
  }

  let hidden_widget_html w =
    let open Html5.F in
    let container =
      Html5.Id.create_named_elt ~id:w.container_id
        (div ~a:[a_class ["container"]; a_style "display: none"] w.content)
    in
    let onclick_overlay = {{
      fun _ ->
        Html5.(Manip.SetCss.display (Id.get_element %w.overlay_id) "none");
        Html5.(Manip.SetCss.display (Id.get_element %w.container_id) "block");
        Option.iter (fun f -> f ()) %w.show_callback;
        Option.iter
          (fun f ->
             let waiter, wakener = Lwt.task () in
             let t =
               lwt () = waiter in
               lwt content = f () in
               Html5.Manip.Named.replaceAllChild %w.container_id content;
               Lwt.return ()
             in
             Lwt.wakeup wakener ();
             %w.set_content_thread <- Some t)
          %w.content_getter;
      ()
    }} in
    let overlay =
      Html5.Id.create_named_elt ~id:w.overlay_id
        (div ~a:[
          a_class ["overlay"];
          a_onclick onclick_overlay;
        ] [ pcdata "click to show"; ])
    in
    Html5.Id.create_named_elt ~id:w.widget_id
      (div ~a:[a_class ["hidden_widget"]] [ overlay; container; ])

}}

{client{

  let hidden_widget_hide w _ =
    Html5.Manip.Named.replaceAllChild w.container_id w.content;
    Html5.(Manip.SetCss.display (Id.get_element w.overlay_id) "block");
    Html5.(Manip.SetCss.display (Id.get_element w.container_id) "none");
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
       Lwt.return Html5.F.([
         (h2 [pcdata "Slow content"] :> Html5_types.div_content Html5.elt);
         p [pcdata (Printf.sprintf "Had to sleep %f seconds for this" sleep_time)];
       ]))

let client_handler_syntax_2 =
  Eliom_testsuite_base.My_appl.register_service
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
       let add_another_waiter_button =
         let onclick = {{
           fun _ ->
             let w = hidden_widget [Html5.F.pcdata "Incredible content!"] in
             ignore (Dom.appendChild (Dom_html.document##body)
                       (Html5.To_dom.of_element (hidden_widget_html w)))
         }} in
         Html5.D.(
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
                     (Html5.To_dom.of_element %hide_button)
                     Js.null));
           hidden_widget_set_content_getter %w
             (fun () ->
                Eliom_client.call_caml_service ~service: %get_slow_content () ())
       }};
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
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
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (shared_onclick "server")]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.void_coservice' [pcdata "self"] ();
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
    Html5.F.(div [pcdata ("shared elt from "^src)])
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
    Html5.F.(div [pcdata "client elt"])
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
    Html5.F.(div [pcdata "server elt"])
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
              (Html5.To_dom.of_div (shared_elt "client"));
            Dom.appendChild
              (Dom_html.document##body)
              (Html5.To_dom.of_div client_elt);
            Lwt.return ())
       }};
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick (shared_onclick "server")]
               [ pcdata "Click me" ];
             a ~service:Eliom_service.void_coservice' [pcdata "self"] ();
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
     Html5.F.(div [(*a ~service: %s [pcdata "Action! (on server)"] ()*)]) 

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
              (Html5.To_dom.of_element (link ()));
            Lwt.return ())
       }};
       Lwt.return Html5.F.(
         html
           (Eliom_tools.Html5.head
              ~title:(String.concat "/" path)
              ~css:[["style.css"]]
              ())
           (body [
             h2 [pcdata description];
             div ~a:[a_class ["thebutton"]; a_onclick {{ show_server_injections }}]
               [ pcdata "Click me" ];
             div [ a ~service:Eliom_service.void_coservice' [pcdata "reload in app"] () ];
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

let an_elt = Html5.D.div []

{client{

  let () =
    Lwt.ignore_result
      (lwt () = Eliom_client.wait_load_end () in
       debug "---> !!! 2";
       Html5.Manip.appendChild
         %an_elt
         Html5.F.(pcdata "!!! 2");
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
    ~description:Html5.F.([
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
           Html5.Manip.appendChild
             %an_elt
             Html5.F.(pcdata "!!! 1")
       }};
       Lwt.return Html5.F.([
         div [a ~service:Eliom_service.void_coservice' [pcdata "reload in app"] ()];
         div [
           button ~a:[a_onclick onclick ] ~button_type:`Submit [
             pcdata "Click";
           ]
         ];
         an_elt;
       ]))

(******************************************************************************)
           *)

(******************************************************************************)

let tests = [
(*
  "Mixed", [
    test_custom_data;
  ];
 *)
  "Holes", [
    test_escape_scoping;
    test_injection_scoping;
    test_client_value_on_caml_service;
    node_bindings;
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
