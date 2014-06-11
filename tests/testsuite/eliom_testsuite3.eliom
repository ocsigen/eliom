(*zap* *)
{shared{
  open Eliom_lib
  open Eliom_content
  open Ocsigen_cookies
}}

(* *zap*)
(*zap*
   This is the Eliom documentation.
   You can find a more readable version of comments on http://www.ocsigen.org
*zap*)
(*wiki*
%<||1>%
%<div class='leftcol'|%<leftcoldoc version="dev">%>%
      %<div class="colprincipale"|
        ==4. Using Eliom client side

//Warning: Features presented here are experimental.
We have been working on them for more than two years and
they will be released very soon.
Use it if you want to test,
but syntax and interfaces will change a lot during the next weeks,
as we are currently working on simplifying the syntax and
uniformizing server and client sides.//

//The manual is very basic for now.
Turn back in a few days for a more complete manual!//

This part of the manuel describes how to use Eliom for mixing client side
and server side programming.
Eliom allows one to write the client and server parts of a Web application
fully in Objectice Caml.
Running OCaml in the client's browser is acheived by compiling OCaml bytecode
into Javascript. Check the [[wiki(30):/|js_of_ocaml]] project for news and
information.


===@@id="p4basics"@@

====Your first client-server application
        %<div class="onecol"|

First, I need to create my Eliom application, by applying the functor
{{{Eliom_output.Eliom_appl}}}. You can define here what will be
the default title for pages belonging to this application, the
default container for pages, the default stylesheets you want for your
whole application.

*wiki*)
(* for client side only, one can use : {client{ ... }} and for shared code, one
* can place {shared{ ... }} *)
{shared{
open Html5.F
}}

(****** server only *******)
{server{ (* note that {server{ ... }} is optional. *)
open Eliom_parameter
open Eliom_service
}}

(* This is server only because there are no delimiters. *)
module My_appl =
  Eliom_registration.App (
    struct
      let application_name = "testsuite_client"
    end)
(*wiki* Now I can define my first service belonging to that application: *wiki*)

let header_id : Html5_types.body_content_fun Html5.Id.id =
  Html5.Id.new_elt_id ~global:true ()
let header () =
  Html5.Id.create_named_elt ~id:header_id
    (p [pcdata "Random value in the container: ";
        (span [pcdata (string_of_int (Random.int 1000))]);
        br ();
        a ~service:Eliom_testsuite_base.main [pcdata "Back to the main page of the test suite."] ();])

let make_page ?(css = []) content =
  html
    (head
       (title (pcdata "Eliom application example"))
       ([ style
           [pcdata "a,.clickable {color: #111188; cursor: pointer;}"];
          My_appl.application_script ();
          meta ~a:[a_charset "utf-8"] ();
        ] @ css))
    (body
       [h1 [pcdata "Eliom application"];
        header ();
        div content ] )

{client{
  module My_appl = Eliom_registration.Html5
}}

{server{ (* note that {server{ ... }} is optional. *)
open My_appl
}}

let eliomclient1 =
  My_appl.register_service
    ~path:["plop"; "eliomclient1"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        (make_page
           [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
          (* with {{ expr }}, the expression is executed by the client. *)
             a_onclick {{ fun _ -> Dom_html.window##alert(Js.string "clicked!") }}
            ]
               [pcdata "I am a clickable paragraph"];

           ]))
(*wiki*
All services belonging to the application will be entry points to the
application. It means that if you call such a service, the client side
code will be sent to the browser, and the client side execution will
start, //and will not stop if you go to another service belonging to
the same application!//

====Compiling
//soon (have a look at Ocsigen source for now -- //examples// directory)//

====Using a distant Eliom service in client side code

For now, the syntax extension has not been implemented, thus the syntax
is somewhat more complicated. Here are some examples of what you can do:
*wiki*)
let eliomclient2 = App.service ~path:["plip"; "eliomclient2"] ~get_params:unit ()

let myblockservice =
  Eliom_registration.Flow5.register_post_coservice
    ~fallback:eliomclient2
    ~post_params:unit
    (fun () () ->
       Lwt.return
         [p [pcdata ("I come from a distant service! Here is a random value: "^
                       string_of_int (Random.int 100))]])

let eliom_caml_tree =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:unit
    (fun () () ->
      Lwt.return
        Html5.F.(([div [p [pcdata "Coucou, voici un Div construit avec TyXML sur le serveur"];
                        ul [li [pcdata "item1"];
                            li [pcdata "item2"];
                            li [pcdata "item3"];
                           ];
                        p [a
                              ~service:eliomclient1 [pcdata "Lien"] ()];
                        p ~a:[a_onclick
                                 {{ fun _ -> Dom_html.window##alert(Js.string "clicked!") }}]
                          [pcdata "I am a clickable paragraph"];
                       ]] : Html5_types.div elt list)))

;; (* This ";;" is necessary in order to have the "shared" following entry being
      parsed as "str_item" (instead of "expr"). This is Camlp4 related, it may
      evolve.
    *)

{shared{
let item () = li [pcdata Sys.ocaml_version]
}} ;;



let _ =
  My_appl.register
    eliomclient2
    (fun () () ->
      Lwt.return
        (make_page
           ([
(*wiki*
  The following example shows how to go to another service,
  exactly like pressing a link (here a service that do not belong to
  the application):
*wiki*)
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick
                {{ fun _ ->
                     Eliom_client.exit_to
                       ~service: %Eliom_testsuite1.coucou (* just as [coucou] *)
                       () ()
                }}
            ]
            [pcdata "Link to a service outside the Eliom application, with exit_to"];

(*wiki*
To use server values inside client code one should use the syntax {{{ %id }}}
where and {{{id}}} an identifier for the value.
*wiki*)

(*zap* *)
(*wiki*
  The following examples shows how to do a request to a service,
  and use the content:
*wiki*)
(*
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick
                ((fun.client
                    (myblockservice : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_service.http) Eliom_service.Http.service) ->
                      let body = Dom_html.document##body in
                      (*Js_old.get_element_by_id "bodyid"*)
                      Eliom_client.call_service
                        ~service:myblockservice () () >>= fun s ->
                      (try
                         let l = Js_old.Node.children (Js_old.dom_of_xml s) in
                         List.iter (Js_old.Node.append body) l
                       with e -> Js_old.alert (Printexc.to_string e));
(* does not work with chrome. A solution is probably to use set "innerHTML". *)
                       Lwt.return ()
                 ) myblockservice)
            ]
            [pcdata "Click here to add content from the server."];
 *)
(* *zap*)

(*wiki*
  The following examples shows how to change the current page,
  without stopping the client side program.
*wiki*)
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick
                {{ fun _ ->
                     ignore(Eliom_client.change_page
                             ~service:%eliomclient1
                             () ())
                }}
            ]
            [pcdata "Click here to change the page without stopping the program (with change_page)."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                fun _ ->
                  Eliom_client.exit_to ~service:%eliomclient2 () ()
              }}
            ]
            [pcdata "Click here to relaunch the program by reloading the page (with exit_to)."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                fun _ ->
                  ignore(Eliom_client.change_page ~service:%eliomclient1
                           () ())
              }}
            ]
            [pcdata "A generic client-side function for calling ";
             code [pcdata "change_page"];
             pcdata "."];

(*wiki*
  The following examples shows how to get a subpage from the server,
  and put it inside your page.
*wiki*)
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                fun _ ->
                  ignore (try_lwt
                            Eliom_client.call_ocaml_service ~service:%eliom_caml_tree
                            () () >|= fun blocks ->
                              List.iter
                                (Dom.appendChild Dom_html.document##body)
                                (List.map Html5.To_dom.of_element blocks)
                          with
                            | e -> Dom_html.window##alert(Js.string (Printexc.to_string e));
                              Lwt.return ()
)
              }}
            ]
            [pcdata "Click here to get a subpage from server."];


(*wiki*
====Refering to parts of the page in client side code
*wiki*)

          (let container = Html5.D.ul [ item () ; item () ; item ()] in
           div [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                               a_onclick {{
                                 fun _ ->
                                   Dom.appendChild
                                     (Html5.To_dom.of_ul %container) (* node is the wrapper keyword for HTML5.M nodes. *)
                                     (Html5.To_dom.of_li (item ()))
                               }}
                  ]
                  [pcdata "Click here to add an item below with the current version of OCaml."];
                container]);

(*wiki*
====Refering to server side data in client side code
  In the case you want to send some server side value with your page,
  just do:
*wiki*)

          (let my_value = 1.12345 in
           p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
                {{
                  fun _ ->
                    Dom_html.window##alert
                      (Js.string (string_of_float %my_value))
                }}
                ]
             [pcdata "Click here to see a server side value sent with the page."]);



(*wiki*
====Other tests
  *wiki*)

          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                fun _ ->
                  let coucou = %Eliom_testsuite1.coucou in
                  let eliomclient1 = %eliomclient1 in
                  (Dom.appendChild
                     (Dom_html.document##body)
                     (Html5.To_dom.of_p
                        (p [Html5.D.a
                              ~service:coucou
                              [pcdata "An external link generated client side"]
                              ();
                            pcdata ", ";
                            Html5.D.a
                              (*zap* *)~a:[a_class ["clickable"]](* *zap*)
                              ~service:eliomclient1
                              [pcdata "another, inside the application, "]
                              ();
                            pcdata " and ";
                            span
                              ~a:[a_class ["clickable"];
                                  a_onclick (fun _ -> Dom_html.window##alert(Js.string "clicked!"))]
                              [pcdata "Here a client-side span with onclick"]
                           ]
                        ))
                  )
              }}
            ]
            [pcdata "Click here to add client side generated links."];



        ])))


(*wiki*
====Using OCaml values as service parameters
It is now possible to send OCaml values to services.
To do that, use the {{{Eliom_parameter.ocaml}}} function:
*wiki*)
{shared{

type ec3 = (int * string * string list) deriving (Json)

}}

let caml_value : ec3 = (299, "oo", ["a";"b";"c"])

let eliomclient3' =
  My_appl.register_post_coservice'
    ~post_params:(ocaml "isb" Json.t<ec3>)
    (fun () (i, s, l as v) ->
       Lwt.return
         (make_page
            [p [if v = caml_value
                then pcdata "The expected data were correctly received."
                else pcdata "Do not received the expected data."];
             p (pcdata (Printf.sprintf "i = %d, s = %s" i s)::
                  List.map (fun a -> pcdata a) l
                 )]))


let eliomclient3 =
  My_appl.register_service
    ~path:["eliomclient3"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        (make_page
           [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
             {{
               fun _ ->
                 ignore (Eliom_client.change_page
                           ~service:%eliomclient3'
                           () %caml_value)
              }}
            ]
               [pcdata "Click to send Ocaml data as Post parameter"]
           ]))

(*wiki*
====Sending OCaml values using services
It is possible to do services that send any caml value. For example:
*wiki*)
let eliomclient4' =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:unit
    (fun () () -> Lwt.return [1; 2; 3])


let eliomclient4 =
  My_appl.register_service
    ~path:["eliomclient4"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        (make_page
           [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
              {{
                fun _ ->
                  lwt_ignore
                   (let body = Dom_html.document##body in
                    lwt l =
                      Eliom_client.call_ocaml_service
                        ~service:%eliomclient4'
                        () () in
                    List.iter
                      (fun i -> Dom.appendChild body
                         (Dom_html.document##createTextNode
                            (Js.string (string_of_int i))))
                      l;
                    Lwt.return ())
                 }}
              ]
           [pcdata "Click to receive Ocaml data and append them below (Should append \"123\")."]
        ]))

(******************************)
(* caml service set reference *)
(******************************)

let ref_caml_service =
  Eliom_reference.eref ~scope:Eliom_common.default_process_scope None

let caml_incr_service =
  Eliom_registration.Ocaml.register_service
    ~path:["caml_service_cookies_request"]
    ~get_params:unit
    (fun () () ->
      lwt i =
        match_lwt Eliom_reference.get ref_caml_service with
          | None -> Lwt.return 0
          | Some i -> Lwt.return i
      in
      lwt () = Eliom_reference.set ref_caml_service (Some (succ i)) in
      Lwt.return i)

let text_incr_service =
  Eliom_registration.String.register_service
    ~path:["text_service_cookies_request"]
    ~get_params:unit
    (fun () () ->
      lwt i =
        match_lwt Eliom_reference.get ref_caml_service with
          | None -> Lwt.return 0
          | Some i -> Lwt.return i
      in
      lwt () = Eliom_reference.set ref_caml_service (Some (succ i)) in
      Lwt.return ((string_of_int i),"text/plain"))

let caml_service_cookies =
  My_appl.register_service
    ~path:["caml_service_cookies"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        (make_page [
          div ~a:[a_onclick {{
            fun _ ->
              ignore (
                lwt i =
                  try_lwt
                    debug "caml_call_service";
                    Eliom_client.call_ocaml_service ~service:%caml_incr_service () ()
                  with
                    | e -> debug_exn "caml_call_service exception: " e; Lwt.fail e
                in
                Dom.appendChild (Dom_html.document##body)
                  (Dom_html.document##createTextNode
                     (Js.string ("ref: "^ string_of_int i ^";  ")));
                Lwt.return ())
          }}]
            [pcdata "click: caml_service"];
          div ~a:[a_onclick {{
            fun _ ->
              ignore (
                lwt i =
                  try_lwt
                    debug "call_service";
                    Eliom_client.call_service ~service:%text_incr_service () ()
                  with
                    | e -> debug_exn "call_service exception: " e; Lwt.fail e
                in
                Dom.appendChild (Dom_html.document##body)
                  (Dom_html.document##createTextNode
                     (Js.string ("ref: "^ i ^";  ")));
                Lwt.return ())
          }}]
            [pcdata "click: text service"];
          pcdata "when clicking on this div, it should print a value incremented each time";
          br ();
          pcdata "this test verifies that client process cookies are correctly sent with caml value services";
          br ();
        ])
    )

let default_no_appl =
  let module App = Eliom_registration.App (struct let application_name = "testsuite_client" end) in
  let open Html5.D in
  let id = Html5.Id.new_elt_id ~global:true () in
  let unique_content =
    let counter = ref 0 in
    fun () ->
      incr counter;
      pcdata (string_of_int !counter) in
  let get_service = Eliom_service.App.service ~path:["no-xhr"] ~get_params:Eliom_parameter.unit () in
  let post_service = Eliom_service.App.post_service ~fallback:get_service ~post_params:Eliom_parameter.unit () in
  let toggle_default_no_appl =
    Eliom_registration.Action.register_post_coservice'
      ~post_params:Eliom_parameter.unit
      (fun () () ->
         Eliom_config.(set_default_links_xhr (not (get_default_links_xhr ())));
         Lwt.return ()) in
  let handler () () =
    let global_elt = Html5.Id.create_named_elt ~id (div [pcdata "Named unique content: "; unique_content ()]) in
    Lwt.return
      (html
        (head (title (pcdata "default_link_xhr")) [])
        (body [
          global_elt;
          div [pcdata "Unique content: "; unique_content ()];
          div Html5.D.([
            a ~service:get_service [pcdata "Link to self"] ();
            get_form ~service:get_service
              (fun () -> [
                 string_input ~input_type:`Submit ~value:"Get to self" ()
              ]);
            post_form ~service:post_service
              (fun () -> [
                 string_input ~input_type:`Submit ~value:"Post to self" ()
              ]) ();
            post_form ~service:toggle_default_no_appl
              (fun () -> [
                string_input ~input_type:`Submit ~value:"Toggle" ();
                pcdata " value of ";
                code [pcdata "sitedata.default_link_xhr"];
                pcdata (Printf.sprintf " (is %b)" (Eliom_config.get_default_links_xhr ()))
              ]) ();
            p [
              pcdata "You may also try to add the attribute ";
              code [pcdata "xhr-link='yes'"];
              pcdata " or ";
              code [pcdata "'no'"];
              pcdata " into the configuration of your Eliom module.";
            ]
          ])
        ])) in
  App.register ~service:get_service handler;
  App.register ~service:post_service handler;
  Eliom_service.((get_service :  (_, _, get_service_kind,_,_, _, _, _, registrable, appl_service) service))

(*wiki*
====Other tests:
*wiki*)
let withoutclient =
  Eliom_service.App.service
    ~path:["withoutclient"]
    ~get_params:unit
    ()


let gotowithoutclient =
  Eliom_service.App.service
    ~path:["gotowithoutclient"]
    ~get_params:unit
    ()

let _ =
  My_appl.register
    ~options:{Eliom_registration.do_not_launch = true}
    ~service:withoutclient
    (fun () () ->
       Lwt.return
         (make_page
         [p [pcdata "If the application was not launched before coming here (or if you reload), this page will not launch it. But if it was launched before, it is still running."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                fun _ ->
                  ignore (Eliom_client.change_page
                            ~service:%gotowithoutclient
                            () ())
              }}
            ]
            [pcdata "Click here to go to a page that launches the application every time (this link does not work if the appl is not launched)."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~service:gotowithoutclient
               [pcdata "Same link with ";
                code [pcdata "a"]; pcdata "."] ()];
         ]));
  My_appl.register
    ~service:gotowithoutclient
    (fun () () ->
       Lwt.return
         (make_page
            [p [pcdata "The application is launched."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                fun _ ->
                  ignore (Eliom_client.change_page
                            ~service:%withoutclient
                            () ())
              }}
            ]
            [pcdata "Click here to see the page that does not launch the application."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~service:withoutclient
               [pcdata "Same link with ";
                code [pcdata "a"]; pcdata "."] ()];
         ]))

let uri_test =
  My_appl.register_service
    ~path:["uritest"]
    ~get_params:unit
    (fun () () ->
      let div =
        Html5.D.div [
          p [pcdata "The following URLs are computed either on server or client side. They should be equal."];
          p [pcdata (Eliom_uri.make_string_uri ~service:eliomclient1 ())];
        ]
      in
      ignore {unit{
        Eliom_client.onload
          (fun _->
             Dom.appendChild (Html5.To_dom.of_div %div)
               (Html5.To_dom.of_p
                 (p [pcdata (Eliom_uri.make_string_uri ~service:%eliomclient1 ())])))
      }};
      Lwt.return (make_page [div])
    )

{client{
  let put n f =
    Printf.ksprintf (fun s ->
      Dom.appendChild (Html5.To_dom.of_element n)
        (Html5.To_dom.of_p (p [pcdata s; br ()]))) f

  let put_li n f =
    Printf.ksprintf (fun s ->
      Dom.appendChild (Html5.To_dom.of_element n)
        (Html5.To_dom.of_li (li [pcdata s]))) f
}}

let wrapping_big_values = My_appl.register_service
  ~path:["wrapping_big_values"]
  ~get_params:(int "size")
  (fun size () ->
    let div =
      Html5.D.div
        [pcdata (Printf.sprintf "there should be a line with: list length: %i"
                   size);
         br ()] in
    let list = Array.to_list (Array.init size (fun i -> i)) in
    ignore {unit{
      Eliom_client.onload
        (fun _->
          put %div "list length: %i" (List.length %list);)
    }};
    Lwt.return
      (make_page [div]))

{shared{
module Wrapping_test =
struct
  type 'a t1 =
      { v_int : int;
        v_float : float;
        v_string : string;
        (* v_int64 : int64; *)
        v_service : 'a }
end
}}

let v1 =
  { Wrapping_test.v_int = 42;
    v_float = 42.42;
    v_string = "fourty two";
    (* v_int64 = 0x4200000000000000L; *)
    v_service = eliom_caml_tree }

let rec rec_list = 1::2::3::rec_list

let react_up = Eliom_react.Up.create (Eliom_parameter.ocaml "react param" Json.t<int>)
let e = Eliom_react.Up.to_react react_up
let e' = React.E.map (fun i -> Printf.printf "event: %i\n%!" i) e

let rec rec_list_react = (react_up,42)::rec_list_react


let global_div = Html5.Id.create_global_elt (div [pcdata "global div"])
let other_global_div = Html5.Id.create_global_elt (div [pcdata "other global div"])

let wrapping1 = Eliom_service.App.service
    ~path:["wrapping1"]
    ~get_params:unit
    ()

let gc_service =
  Eliom_registration.Redirection.register_service
    ~path:["gc_wrapping1"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Gc.full_major (); Lwt.return wrapping1)

let () =
  My_appl.register wrapping1
    (fun () () ->
      let list = Html5.D.ul [] in

      (* Simple unwrapping *)
      ignore {unit{
        Eliom_client.onload
          (fun _ ->
            let v = %v1 in
            put_li %list "The following item must be: \"42=42 42.42=42.420000 fourty two=fourty two\"";
            put_li %list "42=%i 42.42=%f fourty two=%s"
              v.Wrapping_test.v_int v.Wrapping_test.v_float v.Wrapping_test.v_string;
            put_li %list "The following line must be: \"1::2::3::1::2::3::1::...\"";
            ( match %rec_list with
              | a::b::c::d::e::f::g::_ ->
                  put_li %list "%i::%i::%i::%i::%i::%i::%i::..." a b c d e f g;
              | _ -> put_li %list "problem with recursive list"; ))
      }};

      (* Node unwrapping and Caml servive *)
      let content = Html5.D.div [] in
      let unwrapping_div =
	div ~a:[ a_onclick {{
                   fun _ ->
                     let v = %v1 in
                     lwt_ignore
                       (lwt blocks =
                          Eliom_client.call_ocaml_service
                            ~service:v.Wrapping_test.v_service
                            () ()
                        in
                        List.iter (Html5.Manip.appendChild %content) blocks;
                        Lwt.return ())
	          }} ]
          [pcdata "Click here to append some content ";
	   pcdata "(received through an caml_service) ";
	   pcdata "at the bottom of the page (test service unwrapping).";]
      in

      (* React *)
      let react_div =
        p ~a:[ a_onclick {{
                 fun _ ->
                   let f_react = fst (List.hd %rec_list_react) in
                   ignore (f_react 42)
               }} ]
          [pcdata "The string \"event: 42\" should appear on stdout";
           pcdata "(of the server) when this is clicked.";
	   pcdata "(test unwrapping of react service)"]
      in
      Lwt.return
        (make_page
	   [ list;
	     unwrapping_div;
	     react_div;
	     content;
	   ] ) )

(*wiki*
====Implicit registration of services to implement distant function calls
*wiki*)
(*wiki*
*wiki*)
(*wiki*
*wiki*)
(*wiki*
        >% <<|onecol>>
      >% <<|colprincipale>>
*wiki*)
(*wiki*
%<||2>%
*wiki*)


(*wiki*
====Entity in client code
This check that entity are correctly rendered in client and server code.
 *wiki*)

let entity =
  My_appl.register_service
    ~path:["entity"]
    ~get_params:unit
    (fun () () ->
       let d = Eliom_content.Html5.D.div [] in
       ignore {unit{
           Eliom_content.Html5.Manip.appendChild %d (Eliom_content.Html5.F.entity "#947")
         }} ;
       Lwt.return
         (Eliom_tools.F.html
            ~title:"bug_entity"
            ~css:[["css";"bug_entity.css"]]
            Html5.F.(body [
                h2 [pcdata "Welcome from Eliom's distillery!"];
                entity "#946" ;
                d
              ])))

(*wiki*
====Comet programming
The first example demonstrate server-to-client channel communication. Channels
are wrapped and sent to the client. A second example uses channels to transmit
occurrences of an event.
 *wiki*)


(* random wait *)
let rand_tick =
  let i = ref 0 in
  fun () ->
    Lwt_unix.sleep (float_of_int (2 + (Random.int 2))) >>= fun () ->
    incr i; Lwt.return (Some !i)
let stream1 = Lwt_stream.from rand_tick

let _ = Lwt_stream.iter (fun _ -> ()) stream1

let comet1 =
  My_appl.register_service
    ~path:["comet1"]
    ~get_params:unit
    (fun () () ->
       let c1 = Eliom_comet.Channel.create (Lwt_stream.clone stream1) in

       let tick2 =
         let i = ref 0 in
         fun () ->
           Lwt_unix.sleep (float_of_int (6 + (Random.int 6))) >>= fun () ->
           incr i; Lwt.return (Some !i)
       in
       let stream2 = Lwt_stream.from tick2 in
       let c2 = Eliom_comet.Channel.create stream2 in

       ignore {unit{
       Eliom_client.onload
           (fun _ ->
             let _ = Lwt_stream.iter_s
             (fun i ->
               Dom.appendChild (Dom_html.document##body)
                 (Dom_html.document##createTextNode
                    (Js.string ("public: "^ string_of_int i ^";  "))) ;
               Lwt.return ()
             ) %c1 in
             let _ = Lwt_stream.iter_s
             (fun i ->
               Dom.appendChild (Dom_html.document##body)
                 (Dom_html.document##createTextNode
                    (Js.string ("private: "^ string_of_int i ^"; "))) ;
               Lwt.return ()
             ) %c2 in
             ())
         }};

       Lwt.return
         (make_page [
           div
             [pcdata "To fully understand the meaning of the public channel, \
                      use a couple browsers on this page."; br ();
              Html5.D.a ~service:Eliom_testsuite_base.main [pcdata "go outside of application"] ()] ;
         ])
    )


let caml_wrapping_service =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:(Eliom_parameter.unit)
    (fun () () -> Lwt.return
      (Eliom_comet.Channel.create (Lwt_stream.clone stream1)))

let keep_ref v t =
  ignore (lwt () = Lwt_unix.sleep t in
          ignore v;
          Lwt.return ())

let global_channel_wrapping_service =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:(Eliom_parameter.unit)
    (fun () () ->
      let channel = Eliom_comet.Channel.create ~scope:`Site
        (Lwt_stream.clone stream1) in
      keep_ref channel 3.;
      Lwt.return channel)

{client{
  let iter_stream_append f c =
    Lwt_stream.iter_s
      (fun i ->
        Dom.appendChild (Dom_html.document##body)
          (Dom_html.document##createTextNode (Js.string (f i)));
        (* let some time for the server to gc the channel: if the
           client is always requesting data on it, there will always
           be a reference: this allow us to call a Gc.full_major by
           hand, during that 0.5 seconds, to test the collection.*)
        lwt () = Lwt_js.sleep 0.5 in
        Lwt.return ()
      ) c
}}

let caml_service_wrapping =
  My_appl.register_service
    ~path:["caml_service_wrapping"]
    ~get_params:unit
    (fun () () ->
      ignore {unit{
        Eliom_client.onload
          (fun _ ->
            let c = Eliom_comet.Configuration.new_configuration () in
            Eliom_comet.Configuration.set_always_active c true)
      }};
      Lwt.return
        (make_page [
          div ~a:[a_class ["clickable"];
                  a_onclick {{
                    fun _ ->
                      ignore (
                        lwt c = Eliom_client.call_ocaml_service ~service:%caml_wrapping_service () () in
                        try_lwt
                          iter_stream_append (Printf.sprintf "message: %i;  ") c
              with
                | e -> debug_exn "caml_service_wrapping: exception: " e; Lwt.fail e
                    )
                  }}]
            [pcdata "click to create a channel with scope client_process"];
          div ~a:[a_class ["clickable"];
                  a_onclick {{
                    fun _ ->
                      ignore (
                        lwt c = Eliom_client.call_ocaml_service ~service:%global_channel_wrapping_service () () in
                        try_lwt
                          iter_stream_append (Printf.sprintf "site message: %i;  ") c
              with
                | Eliom_comet.Channel_closed ->
                  Dom.appendChild (Dom_html.document##body)
                    (Dom_html.document##createTextNode (Js.string ("channel closed")));
                  Lwt.return ()
                | e -> debug_exn "global_channel_wrapping_service: exception: " e; Lwt.fail e
                    )
                  }}]
            [pcdata "click to create a channel with scope site: it has a lifetime of 3 seconds: after 3 seconds, there is no garanty on availability of this channel"];
          pcdata "when clicking on this link, messages should be received every 1 second";
        ])
    )


(*wiki*
  This second example involves client-to-server and server to client event
  propagation. There is no manual handling of channel, only events are used.
 *wiki*)

let comet2 =
  My_appl.register_service
    ~path:["comet2"]
    ~get_params:unit
    (fun () () ->
      (* First create a server-readable client-writable event AKA up event AKA
         client-to-server asynchronous edge *)
      let e_up = Eliom_react.Up.create (Eliom_parameter.ocaml "letter" Json.t<string>) in
      let e_up_react = Eliom_react.Up.to_react e_up in
      let e_down =
        Eliom_react.Down.of_react
          (React.E.map
             (function "A" -> "alpha" | "B" -> "beta" | _ -> "what ?")
             e_up_react
          )
      in
      let `R _ = React.E.retain e_up_react (fun () -> ignore e_down) in
      ignore {unit{
        Eliom_client.onload
          (fun _ ->
            ignore (React.E.map
              (fun s -> Dom_html.window##alert (Js.string s))
              %e_down))
        }};

      (* We can send the page *)
      Lwt.return (make_page [
         h2 [pcdata "Dual events"] ;
         div (* This div is for pushing "A" to the server side event *)
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ fun _-> ignore ( %e_up "A") }} ]
           [pcdata "Push A"] ;
         div (* This one is for pushing "B" *)
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ fun _-> ignore ( %e_up "B") }} ]
           [pcdata "Push B"] ;
       ])
    )

(*wiki*
 This third example demonstrates the capacity for simultaneous server push.
 *wiki*)

let comet3 =
  My_appl.register_service
    ~path:["comet3"]
    ~get_params:unit
    (fun () () ->
       (* First create a server-readable client-writable event AKA up event AKA
          client-to-server asynchronous edge *)
       let e_up = Eliom_react.Up.create (Eliom_parameter.ocaml "double" Json.t<string>) in
       let e_up_react = Eliom_react.Up.to_react e_up in
       let e_down_1 =
         Eliom_react.Down.of_react
           (React.E.map
              (let i = ref 0 in fun _ -> incr i ; !i)
              e_up_react
           )
       in
       let e_down_2 =
         Eliom_react.Down.of_react (React.E.map (fun _ -> "haha") e_up_react)
       in
       let `R _ = React.E.retain e_up_react
                    (fun () -> ignore e_down_1 ; ignore e_down_2)
       in
       ignore {unit{
       Eliom_client.onload
           (fun _ ->
             ignore (React.E.map
             (fun s -> Dom_html.window##alert (Js.string s))
             (React.E.merge
                (^) ""
                [ React.E.map string_of_int %e_down_1 ;
                  %e_down_2 ;
                ]
             )))
         }};

       (* We can send the page *)
       Lwt.return (make_page [
         h2 [pcdata "Simultaneous events"] ;
         div
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ fun _ -> ignore ( %e_up "") }} ]
           [pcdata "Send me two values from different events !"] ;
       ])
    )

let comet_wrapping =
  My_appl.register_service
    ~path:["comet_wrapping"]
    ~get_params:unit
    (fun () () ->
      let node = Html5.D.div [pcdata "node created on server side"] in
      let service_stream,push_service = Lwt_stream.create () in
      push_service (Some Eliom_testsuite1.coucou);
      let c_service = Eliom_comet.Channel.create service_stream in
      let xml_stream,push_xml = Lwt_stream.create () in
      push_xml (Some (div [pcdata "basic xml wrapping";node]));
      push_xml (Some
                  (div [Html5.D.a ~service:Eliom_testsuite1.coucou
                     [pcdata "xml external link wrapping"] ()]));
      push_xml (Some
                  (div [Html5.D.a ~service:comet1
                           [pcdata "xml internal link wrapping"] ();
                        br ();
                        pcdata "this link must not stop the process! (same random number in the container)."]));
      let c_xml = Eliom_comet.Channel.create xml_stream in
      let div_link = Html5.D.div [] in

      ignore {unit{
        Eliom_client.onload
         (fun _ ->
            ignore (Lwt_stream.iter
            (fun service ->
              Dom.appendChild (Html5.To_dom.of_element %div_link)
                (Html5.To_dom.of_element
                   ( Html5.D.a ~service
                       [pcdata "service wrapping"] ()))
            ) %c_service);
            ignore (Lwt_stream.iter
                      (fun xml ->
                        Dom.appendChild (Html5.To_dom.of_element %div_link)
                          (Html5.To_dom.of_element xml)
                      ) %c_xml))
        }};

      Lwt.return
        (make_page [
          div [pcdata "there should be a working links below"];
          node;
          div_link;
        ])
    )

let comet_signal_maker name time =
  My_appl.register_service
    ~path:[name]
    ~get_params:unit
    (fun () () ->
      let time_div = Html5.D.div [] in
      ignore {unit{
      Eliom_client.onload
          (fun _ ->
            Lwt_react.S.keep
            (React.S.map (fun t -> (Html5.To_dom.of_div %time_div)##innerHTML <-
              Js.string (string_of_float t)) %time))
        }};
       Lwt.return (make_page [
         h2 [pcdata "Signal"] ;
         time_div;
         br ();
         a ~service:Eliom_service.void_coservice' [pcdata "reload"] ();
       ])
    )

let time =
  let t = Unix.gettimeofday () in
  let e = Lwt_react.E.from (fun () -> Lwt_unix.sleep 0.1 >>= (fun () -> Lwt.return (Unix.gettimeofday ()))) in
  Eliom_react.S.Down.of_react (Lwt_react.S.hold t e)

let comet_signal = comet_signal_maker "comet_signal" time


(*wiki*
 Here is the code for a minimalistic message board.
 *wiki*)

let comet_message_board_maker name message_bus cb =
  My_appl.register_service
    ~path:[name]
    ~get_params:unit
    (fun () () ->
       cb ();
       Lwt.return (
         let container = Html5.D.ul [li [em [pcdata "This is the message board"]]] in
         let field = Html5.D.raw_input ~a:[a_id "msg"; a_name "message"] ~input_type:`Text () in
         ignore {unit{
         Eliom_client.onload
             (fun _ ->
               let c = Eliom_comet.Configuration.new_configuration () in
               Eliom_comet.Configuration.set_timeout c 3.;
               let _ =
                 Lwt.catch (fun () ->
                   Lwt_stream.iter_s
                     (fun msg ->
                       Dom.appendChild (Html5.To_dom.of_element %container)
                         (Html5.To_dom.of_li (li [pcdata msg]));
                       Lwt.return ())
                     (Eliom_bus.stream %message_bus))
                   (function
                     | Eliom_comet.Channel_full ->
                       Dom.appendChild (Html5.To_dom.of_element %container)
                         (Html5.To_dom.of_li (li [pcdata "channel full, no more messages"]));
                       Lwt.return ()
                     | e ->
                       debug_exn "comet exception: " e;
                       Lwt.fail e);
               in ())
           }} ;

         let go =
           div
             ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                  a_onclick {{
                    fun _ ->
                      let field =
                          (Js.Opt.get
                             (Dom_html.CoerceTo.input
                                (Js.Opt.get
                                   (Dom_html.document##getElementById (Js.string "msg"))
                                   (fun () -> failwith "No field found")
                                )
                             )
                             (fun () -> failwith "No field found")
                          )
                      in
                      let v = Js.to_string field##value in
                      field##value <- Js.string "" ;
                      ignore (Eliom_bus.write %message_bus v)
                  }}
             ]
             [pcdata "send"]
         in

         (make_page [ h2 [pcdata "Message board"];
           raw_form ~a:[a_action (Xml.uri_of_string "")] [div [field; go]];
           container; br ();
           Html5.D.a ~service:Eliom_testsuite_base.main [pcdata "go outside of application"] ();
         ]))
    )

let message_bus =
  Eliom_bus.create ~scope:Eliom_common.default_process_scope
    ~size:10 Json.t<string>
let _ =
  Lwt_stream.iter (fun msg -> Printf.printf "msg: %s\n%!" msg)
    (Eliom_bus.stream message_bus)
let message_board_callback () =
  let _ =
    lwt () = Eliom_bus.write message_bus "a user joined in" in
    lwt () = Eliom_comet.Channel.wait_timeout
      ~scope:Eliom_common.default_process_scope 1. in
    Eliom_bus.write message_bus "a user went away"
  in ()

let comet_message_board = comet_message_board_maker "message_board" message_bus message_board_callback

(* bus stream received multiple times *)

let multiple_bus = Eliom_bus.create ~scope:Eliom_common.default_process_scope ~name:"multiple_bus" ~size:10 Json.t<int>
let _ = Lwt_stream.iter (fun _ -> ()) (Eliom_bus.stream multiple_bus)
let multiple_bus_stateless = Eliom_bus.create ~name:"multiple_bus_stateless" ~scope:`Site ~size:10 Json.t<int>

let multiple_bus_position = ref 0

let _ =
  let rec tick () =
    lwt () = Lwt_unix.sleep 1. in
    lwt () = Eliom_bus.write multiple_bus !multiple_bus_position in
    lwt () = Eliom_bus.write multiple_bus_stateless !multiple_bus_position in
    incr multiple_bus_position;
    tick ()
  in tick ()

let bus_multiple_times =
  My_appl.register_service
    ~path:["multiple_bus"]
    ~get_params:unit
    (fun () () ->
      let container = Html5.D.ul [li [em [pcdata "there will be lines"]]] in
      let onload s message_bus =
        ignore {unit{
          Eliom_client.onload
            (fun _ ->
              let _ =
                try_lwt
                  Lwt_stream.iter_s
                    (fun msg ->
                      Dom.appendChild (Html5.To_dom.of_element %container)
                        (Html5.To_dom.of_li (li [pcdata (Printf.sprintf "stream %s: %i" %s msg)]));
                      Lwt.return ())
                    (Eliom_bus.stream %message_bus)
                 with
                   | Eliom_comet.Channel_full ->
                     Dom.appendChild (Html5.To_dom.of_element %container)
                       (Html5.To_dom.of_li (li [pcdata "channel full, no more messages"]));
                     Lwt.return ()
                   | e -> Lwt.fail e;
              in ())
        }} in
      onload "statefull 1" multiple_bus;
      onload "statefull 2" multiple_bus;
      onload "statefull 3" multiple_bus;
      onload "stateless 1" multiple_bus_stateless;
      onload "stateless 2" multiple_bus_stateless;
      onload "stateless 3" multiple_bus_stateless;
      Lwt.return (make_page [ h2 [pcdata "Multiple streams from one bus"];
                              br ();
                              a ~service:Eliom_service.void_coservice' [pcdata "reload"] ();
                              br ();
                              pcdata (Printf.sprintf "original position: %i" !multiple_bus_position);
                              br ();
                              container;])
    )

(*wiki*
===Stateless comet channels
 *wiki*)

(* random wait *)
let rand_tick =
  let i = ref 0 in
  fun () ->
    Lwt_unix.sleep (float_of_int (2 + (Random.int 2))) >>= fun () ->
    incr i; Lwt.return (Some !i)
let stream_sl = Lwt_stream.from rand_tick
let stateless_channel = Eliom_comet.Channel.create ~scope:`Site ~name:"stateless" stream_sl
let _ = Eliom_comet.Channel.get_wrapped stateless_channel
let external_stateless_channel : int Eliom_comet.Channel.t =
  Eliom_comet.Channel.external_channel
    ~prefix:"http://localhost:8080"
    ~name:"stateless"
    ()

let comet_stateless =
  My_appl.register_service
    ~path:["comet_stateless"]
    ~get_params:unit
    (fun () () ->

       ignore
         {unit{
           Eliom_client.onload (fun _->
             let _ = Lwt_stream.iter_s
             (fun i ->
               Dom.appendChild (Dom_html.document##body)
                 (Dom_html.document##createTextNode
                    (Js.string ("msg: "^ string_of_int i ^";  "))) ;
               Lwt.return ()
             ) %stateless_channel in
             ())
         }};

       Lwt.return
         (make_page [
           div
             [pcdata "Comet channel with no client specific server side state."] ;
         ])
    )

let comet_stateless_external =
  My_appl.register_service
    ~path:["comet_stateless_external"]
    ~get_params:unit
    (fun () () ->

       ignore
         {unit{
          Eliom_client.onload (fun _ ->
             let _ = Lwt_stream.iter_s
             (fun i ->
               Dom.appendChild (Dom_html.document##body)
                 (Dom_html.document##createTextNode
                    (Js.string ("msg: "^ string_of_int i ^";  "))) ;
               Lwt.return ()
             ) %external_stateless_channel in
             ())
         }};

       Lwt.return
         (make_page [
           div
             [pcdata "External Comet channel: access the channel at http://localhost:8080.";
              br ();
             pcdata "If it is another server, that server must run the Cross-Origin Resource Sharing extension of ocsigenserver to allow requests from this page."];
         ])
    )


let time =
  let t = Unix.gettimeofday () in
  let e = Lwt_react.E.from (fun () -> Lwt_unix.sleep 0.1 >>= (fun () -> Lwt.return (Unix.gettimeofday ()))) in
  Eliom_react.S.Down.of_react ~scope:`Site ~name:"time" (Lwt_react.S.hold t e)
let comet_signal_stateless = comet_signal_maker "comet_signal_stateless" time

let message_bus_site = Eliom_bus.create ~scope:`Site ~size:10 Json.t<string>
let _ =
  Lwt_stream.iter (fun msg -> Printf.printf "msg site: %s\n%!" msg)
    (Eliom_bus.stream message_bus_site)
let comet_message_board_stateless = comet_message_board_maker "message_board_stateless"
  message_bus_site (fun () -> ())


(*wiki*
===Header manipulation with eliom client

 *wiki*)

let service_style1 =
  Eliom_service.App.service
    ~path:["css"; "style1"]
    ~get_params:unit
    ()
let service_style2 =
  Eliom_service.App.service
    ~path:["css"; "style2"]
    ~get_params:unit
    ()
let service_no_style =
  Eliom_service.App.service
    ~path:["css"; "no_style"]
    ~get_params:unit
    ()

let page_css_test () =
  [Html5.D.a ~service:service_style1
      [pcdata "same page with style 1"] (); br ();
   Html5.D.a ~service:service_style2
     [pcdata "same page with style 2"] (); br ();
   Html5.D.a ~service:service_no_style
     [pcdata "same page with no style"] (); br ();
   div ~a:[a_class ["some_class"];] [pcdata "div with style"]]

let make_css_link file =
  Html5.D.css_link
    (Html5.D.make_uri
       ~service:(Eliom_service.static_dir ()) [file]) ()

let () =
  My_appl.register ~service:service_style1
    (fun () () ->
      Lwt.return (make_page ~css:[make_css_link "test_style1.css"] (page_css_test ())));
  My_appl.register ~service:service_style2
    (fun () () ->
       Lwt.return (make_page ~css:[make_css_link "test_style2.css"] (page_css_test ())));
  My_appl.register ~service:service_no_style
    (fun () () ->
       Lwt.return (make_page (page_css_test ())))


(*wiki*
===Events with lwt with cancellers

 *wiki*)

{client{
open Lwt_js_events
}}

let event2_service =
  My_appl.register_service
    ~path:["events2"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->

      let make_target s = Html5.D.p [Html5.F.Raw.a [pcdata s]] in
      let target1 = make_target "Un seul clic" in
      let target2 = make_target "Annuler le précédent" in
      let target3 = make_target "Drag vers la ligne au dessus une seule fois" in
      let target4 = make_target "Plein de clics seq" in
      let target5 = make_target "Annuler le précédent" in
      let target6 = make_target "Deux clics" in
      let target7 = make_target "Un clic sur deux" in
      let target8 = make_target "All clicks but the first" in
      let target9 = make_target "Un des deux, premier" in
      let target10 = make_target "Un des deux, deuxieme" in
      let target11 = make_target "Annuler les deux précédents" in
      let target12 = make_target "Drag" in
      let target13 = make_target "Annuler le précédent" in
      let target14 = make_target "Drag with long handler" in
      let target15 = make_target "Annuler le précédent" in
      let target16 = make_target "Mouse over change color" in
      let target17 = make_target "Mouse wheel (browser dependant - test in several browsers)" in
      let target18 = Html5.D.raw_textarea ~name:"a" () in
      let target19 = make_target "If you click very quickly after having entered a letter below, my handler (short) will occure, and the long handler for the keypress will be cancelled (event if it already started)." in
      let target20 = Html5.D.raw_textarea ~name:"b" () in
      let target21 = make_target "If you click very quickly after having entered a letter below, my handler will not occure because the long handler for the keypress below is detached." in

      let targetresult = Html5.D.p [] in
      ignore {unit{
        Eliom_client.onload
          (fun _ ->
            let targetresult = (Html5.To_dom.of_p %targetresult) in

            let handler ev =
              ignore (targetresult##appendChild
                        ((Html5.To_dom.of_element (Html5.F.pcdata " plip") :> Dom.node Js.t)));
              Lwt.return ()
            in
            let handler_long ev =
              Lwt_js.sleep 0.7 >>= fun () ->
              ignore (targetresult##appendChild
                        ((Html5.To_dom.of_element (Html5.F.pcdata " plop") :> Dom.node Js.t)));
              Lwt.return ()
            in
            let handler1 ev _ = handler ev in
            let handler_long1 ev _ = handler_long ev in
            let c = click (Html5.To_dom.of_p %target1) >>= handler in
            ignore (click (Html5.To_dom.of_p %target2) >|= fun _ ->
                           Lwt.cancel c);
            ignore
              (mousedown (Html5.To_dom.of_p %target3) >>= fun ev ->
               Dom.preventDefault ev;
               mouseup (Html5.To_dom.of_p %target2) >>= handler);
            let c = clicks (Html5.To_dom.of_p %target4) handler_long1 in
            ignore
              (click (Html5.To_dom.of_p %target5) >|= fun _ ->
               Lwt.cancel c);
            ignore
              (click (Html5.To_dom.of_p %target6) >>= handler >>= fun () ->
               click (Html5.To_dom.of_p %target6) >>= handler);
            ignore
              (clicks (Html5.To_dom.of_p %target7)
                 (fun _ _ -> click (Html5.To_dom.of_p %target7) >>= handler));
            ignore
              (click (Html5.To_dom.of_p %target8) >>= fun _ ->
               clicks (Html5.To_dom.of_p %target8) handler1);
            let c =
              Lwt.pick [click (Html5.To_dom.of_p %target9) >>= handler;
                        click (Html5.To_dom.of_p %target10) >>= handler]
            in
            ignore (click (Html5.To_dom.of_p %target11) >|= fun _ ->
                    Lwt.cancel c);
            let c = mousedowns (Html5.To_dom.of_p %target12)
              (fun _ _ -> Lwt.pick [(mouseup Dom_html.document >|= fun _ -> ());
                                     mousemoves Dom_html.document handler1])
            in
            ignore (click (Html5.To_dom.of_p %target13) >|= fun _ ->
                    Lwt.cancel c);
            let c = mousedowns (Html5.To_dom.of_p %target14)
              (fun _ _ -> Lwt.pick [(mouseup Dom_html.document >|= fun _ -> ());
                                     mousemoves Dom_html.document handler_long1])
            in
            ignore (click (Html5.To_dom.of_p %target15) >|= fun _ ->
                    Lwt.cancel c);
            let t16 = Html5.To_dom.of_p %target16 in
            ignore (mouseovers t16
                      (fun _ _ ->
                        t16##style##backgroundColor <- Js.string "red";
                        Lwt.return ()));
            ignore (mouseouts t16
                      (fun _ _ ->
                        t16##style##backgroundColor <- Js.string "";
                        Lwt.return ()));
            ignore (mousewheels (Html5.To_dom.of_p %target17)
                      (fun (_, (dx, dy)) _ ->
                        ignore (targetresult##appendChild
                                  ((Html5.To_dom.of_element
                                      (Html5.F.pcdata
                                      (Printf.sprintf "(%d, %d)" dx dy)) :> Dom.node Js.t)));
                        Lwt.return ()));
            ignore (Lwt.pick [(keypress (Html5.To_dom.of_textarea %target18) >>=
                               handler_long);
                               click (Html5.To_dom.of_p %target19) >>=
                               handler
                             ]);
            ignore (Lwt.pick [(keypress (Html5.To_dom.of_textarea %target20) >>=
                               fun _ -> ignore (handler_long ()); Lwt.return () );
                               click (Html5.To_dom.of_p %target21) >>=
                               handler
                             ]))


        }};

       Lwt.return
         (make_page [target1; target2; target3; target4; target5; target6;
                     target7; target8; target9; target10; target11;
                     target12; target13; target14; target15; target16;
                     target17; target18; target19; target20; target21;
                     targetresult]) )



(*wiki*

===Tab sessions

  *wiki*)


open Lwt


(************************************************************)
(************ Connection of users, version 1 ****************)
(************************************************************)

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "tsession_data"
let scope = `Client_process scope_hierarchy
(* *zap*)

(* "my_table" will be the structure used to store
   the session data (namely the login name): *)

let my_table = Eliom_state.create_volatile_table ~scope ()



(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let tsession_data_example =
  Eliom_service.App.service
    ~path:["tsessdata"]
    ~get_params:Eliom_parameter.unit
    ()


let tsession_data_example_with_post_params =
  Eliom_service.App.post_service
    ~fallback:tsession_data_example
    ~post_params:(Eliom_parameter.string "login")
    ()


let tsession_data_example_close =
  Eliom_service.App.service
    ~path:["tclose"]
    ~get_params:Eliom_parameter.unit
    ()




(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example" service:          *)

let tsession_data_example_handler _ _  =
  let sessdat =
    Eliom_state.get_volatile_data ~table:my_table ()
  in
  return
    (make_page [
      match sessdat with
        | Eliom_state.Data name ->
          p [pcdata ("Hello "^name);
             br ();
             Html5.D.a
               tsession_data_example_close
               [pcdata "close session"] ()]
        | Eliom_state.Data_session_expired
        | Eliom_state.No_data ->
          Html5.D.post_form
            tsession_data_example_with_post_params
            (fun login ->
              [p [pcdata "login: ";
                  Html5.D.string_input
                    ~input_type:`Text ~name:login ()]]) ()
    ])


(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_with_post_params"  *)
(* service with POST params:                                *)

let tsession_data_example_with_post_params_handler _ login =
  lwt () = Eliom_state.discard ~scope () in
  Eliom_state.set_volatile_data
    ~table:my_table login;
  return
    (make_page [p [pcdata ("Welcome " ^ login ^ ". You are now connected.");
        br ();
        Html5.D.a tsession_data_example [pcdata "Try again"] ()
       ]])




(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_close" service:    *)

let tsession_data_example_close_handler () () =
  let sessdat = Eliom_state.get_volatile_data
    ~table:my_table () in
  lwt () = Eliom_state.discard ~scope () in
  return
    (make_page [
      (match sessdat with
        | Eliom_state.Data_session_expired -> p [pcdata "Your session has expired."]
        | Eliom_state.No_data -> p [pcdata "You were not connected."]
        | Eliom_state.Data _ -> p [pcdata "You have been disconnected."]);
      p [Html5.D.a tsession_data_example [pcdata "Retry"] () ]])



(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register
    tsession_data_example_close tsession_data_example_close_handler;
  My_appl.register
    tsession_data_example tsession_data_example_handler;
  My_appl.register
    tsession_data_example_with_post_params
    tsession_data_example_with_post_params_handler



(************************************************************)
(************ Connection of users, version 2 ****************)
(************************************************************)

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "tsession_services"
let scope = `Client_process scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let tsession_services_example =
  Eliom_service.App.service
    ~path:["tsessionservices"]
    ~get_params:Eliom_parameter.unit
    ()


let tsession_services_example_with_post_params =
  Eliom_service.App.post_service
    ~fallback:tsession_services_example
    ~post_params:(Eliom_parameter.string "login")
    ()


let tsession_services_example_close =
  Eliom_service.App.service
    ~path:["tclose2"]
    ~get_params:Eliom_parameter.unit
    ()



(* ------------------------------------------------------------- *)
(* Handler for the "tsession_services_example" service:           *)
(* It displays the main page of our site, with a login form.     *)

let tsession_services_example_handler () () =
  let f =
    Html5.D.post_form
      tsession_services_example_with_post_params
      (fun login ->
        [p [pcdata "login: ";
            string_input ~input_type:`Text ~name:login ()]]) ()
  in
  return (make_page [f])



(* ------------------------------------------------------------- *)
(* Handler for the "tsession_services_example_close" service:     *)

let tsession_services_example_close_handler () () =
  lwt () = Eliom_state.discard ~scope () in
  Lwt.return
    (make_page
       [p [pcdata "You have been disconnected. ";
           a tsession_services_example
             [pcdata "Retry"] ()
          ]])


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_with_post_params"   *)
(* service:                                                      *)

let tlaunch_session () login =

  (* New handler for the main page: *)
  let new_main_page () () =
    return
      (make_page
         [p [pcdata "Welcome ";
             pcdata login;
             pcdata "!"; br ();
             a eliomclient1 [pcdata "coucou"] (); br ();
             a tsession_services_example_close
               [pcdata "close session"] ()]])
  in

  (* If a session was opened, we close it first! *)
  lwt () = Eliom_state.discard ~scope () in

  (* Now we register new versions of main services in the
     session service table: *)
  My_appl.register
    ~scope
    ~service:tsession_services_example
    (* service is any public service already registered,
       here the main page of our site *)
    new_main_page;

  My_appl.register
    ~scope
    ~service:eliomclient1
    (fun () () ->
      return
        (make_page [p [pcdata "Coucou ";
            pcdata login;
            pcdata "!"]]));

  new_main_page () ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register
    ~service:tsession_services_example
    tsession_services_example_handler;
  My_appl.register
    ~service:tsession_services_example_close
    tsession_services_example_close_handler;
  My_appl.register
    ~service:tsession_services_example_with_post_params
    tlaunch_session




(************************************************************)
(************** Coservices. Basic examples ******************)
(************************************************************)

(* -------------------------------------------------------- *)
(* We create one main service and two coservices:           *)

let tcoservices_example =
  Eliom_service.App.service
    ~path:["tcoserv"]
    ~get_params:Eliom_parameter.unit
    ()

let tcoservices_example_post =
  Eliom_service.App.post_coservice
    ~fallback:tcoservices_example
    ~post_params:Eliom_parameter.unit
    ()

let tcoservices_example_get =
  Eliom_service.App.coservice
    ~fallback:tcoservices_example
    ~get_params:Eliom_parameter.unit
    ()

(* -------------------------------------------------------- *)
(* The three of them display the same page,                 *)
(* but the coservices change the counter.                   *)

let _ =
  let c = ref 0 in
  let page () () =
    let l3 = Html5.D.post_form tcoservices_example_post
        (fun _ -> [p [Html5.D.string_input
                        ~input_type:`Submit
                        ~value:"incr i (post)" ()]]) ()
    in
    let l4 = Html5.D.get_form tcoservices_example_get
        (fun _ -> [p [Html5.D.string_input
                        ~input_type:`Submit
                        ~value:"incr i (get)" ()]])
    in
    return
      (make_page [p [pcdata "The random number in the container must not change!"; br ();
          pcdata "i is equal to ";
          pcdata (string_of_int !c); br ();
          a tcoservices_example [pcdata "internal application link to myself"] (); br ();
          a tcoservices_example_get [pcdata "incr i"] ()];
       l3;
       l4])
  in
  My_appl.register tcoservices_example page;
  let f () () = c := !c + 1; page () () in
  My_appl.register tcoservices_example_post f;
  My_appl.register tcoservices_example_get f




(************************************************************)
(*************** calc: sum of two integers ******************)
(************************************************************)

(*zap* *)
let scope = `Client_process Eliom_testsuite1.calc_example_scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create two main services on the same URL,             *)
(* one with a GET integer parameter:                        *)

let tcalc =
  App.service
    ~path:["tcalc"]
    ~get_params:unit
    ()


let tcalc_i =
  App.service
    ~path:["tcalc"]
    ~get_params:(int "i")
    ()



(* -------------------------------------------------------- *)
(* The handler for the service without parameter.           *)
(* It displays a form where you can write an integer value: *)

let tcalc_handler () () =
  let create_form intname =
    [p [pcdata "Write a number: ";
        Html5.D.int_input ~input_type:`Text ~name:intname ();
        br ();
        Html5.D.string_input ~input_type:`Submit ~value:"Send" ()]]
  in
  let f = Html5.D.get_form tcalc_i create_form in
  return (make_page [f])



(* -------------------------------------------------------- *)
(* The handler for the service with parameter.              *)
(* It creates dynamically and registers a new coservice     *)
(* with one GET integer parameter.                          *)
(* This new coservice depends on the first value (i)        *)
(* entered by the user.                                     *)

let tcalc_i_handler i () =
  let create_form is =
    (fun entier ->
       [p [pcdata (is^" + ");
           int_input ~input_type:`Text ~name:entier ();
           br ();
           string_input ~input_type:`Submit ~value:"Sum" ()]])
  in
  let is = string_of_int i in
  let tcalc_result =
    My_appl.register_coservice
      ~scope:Eliom_common.default_process_scope
      ~fallback:tcalc
      ~get_params:(int "j")
      (fun j () ->
        let js = string_of_int j in
        let ijs = string_of_int (i+j) in
        return (make_page [p [pcdata (is^" + "^js^" = "^ijs)]]))
  in
  let f = get_form tcalc_result (create_form is) in
  return (make_page [f])



(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register tcalc   tcalc_handler;
  My_appl.register tcalc_i tcalc_i_handler



(************************************************************)
(************ Connection of users, version 3 ****************)
(************************************************************)

(*zap* *)
let scope = `Client_process Eliom_testsuite1.connect_example3_scope_hierarchy
let my_table = Eliom_state.create_volatile_table ~scope ()
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let tconnect_example3 =
  Eliom_service.App.service
    ~path:["taction"]
    ~get_params:Eliom_parameter.unit
    ()

let tconnect_action =
  Eliom_service.Http.post_coservice'
    ~name:"tconnect3"
    ~post_params:(Eliom_parameter.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let tdisconnect_action =
  Eliom_registration.Action.register_post_coservice'
    ~name:"tdisconnect3"
    ~post_params:Eliom_parameter.unit
    (fun () () ->
      Eliom_state.discard ~scope ())

(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let tdisconnect_box s =
  Html5.D.post_form tdisconnect_action
    (fun _ -> [p [Html5.D.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let tlogin_box () =
  Html5.D.post_form tconnect_action
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Html5.D.string_input
                     ~input_type:`Text ~name:loginname ()]
         in l)
     ])
    ()

(* -------------------------------------------------------- *)
(* Handler for the "connect_example3" service (main page):    *)

let tconnect_example3_handler () () =
  let sessdat = Eliom_state.get_volatile_data
    ~table:my_table () in
  return
    (make_page (match sessdat with
      | Eliom_state.Data name ->
        [p [pcdata ("Hello "^name); br ()];
         Html5.D.a ~service:tconnect_example3 [pcdata "Try again to check whether you are still connected"] ();
         tdisconnect_box "Close session"]
      | Eliom_state.Data_session_expired
      | Eliom_state.No_data -> [tlogin_box ()]
    ))



(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let tconnect_action_handler () login =
  lwt () = Eliom_state.discard ~scope () in
  Eliom_state.set_volatile_data ~table:my_table login;
  return ()



(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register ~service:tconnect_example3 tconnect_example3_handler;
  Eliom_registration.Action.register ~service:tconnect_action tconnect_action_handler




(************************************************************)
(************ Connection of users, version 4 ****************)
(**************** (persistent sessions) *********************)
(************************************************************)

(*zap* *)
let scope = `Client_process Eliom_testsuite1.persistent_sessions_scope_hierarchy
(* *zap*)
let tmy_persistent_table =
  Eliom_state.create_persistent_table ~scope "teliom_example_table"


(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let tpersist_session_example =
  Eliom_service.App.service
    ~path:["tpersist"]
    ~get_params:unit
    ()


let tpersist_session_connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"tconnect4"
    ~post_params:(string "login")
    ()


(* disconnect_action, login_box and disconnect_box have been
   defined in the section about actions *)

(*zap* *)

(* -------------------------------------------------------- *)
(* Actually, no. It's a lie because we don't use the
   same session name :-) *)
(* new disconnect action and box:                           *)

let tdisconnect_action =
  Eliom_registration.Action.register_post_coservice'
    ~name:"tdisconnect4"
    ~post_params:Eliom_parameter.unit
    (fun () () ->
      Eliom_state.discard ~scope  ())


let tdisconnect_box s =
  Html5.D.post_form tdisconnect_action
    (fun _ -> [p [Html5.D.string_input
                    ~input_type:`Submit ~value:s ()]]) ()


let bad_user_key = Polytables.make_key ()
let get_bad_user table =
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false


(* -------------------------------------------------------- *)
(* new login box:                                           *)

let tlogin_box session_expired action =
  Html5.D.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         string_input ~input_type:`Text ~name:loginname ()]
      in
      [p (if get_bad_user (Eliom_request_info.get_request_cache ())
      then (pcdata "Wrong user")::(br ())::l
      else
        if session_expired
        then (pcdata "Session expired")::(br ())::l
        else l)
     ])
    ()


(* *zap*)

(* ----------------------------------------------------------- *)
(* Handler for "persist_session_example" service (main page):  *)

let tpersist_session_example_handler () () =
    let timeoutcoserv =
      Eliom_service.Http.coservice
        ~fallback:tpersist_session_example ~get_params:unit ~timeout:5. ()
    in
    let _ =
      Eliom_registration.Html5.register ~service:timeoutcoserv
        ~scope:Eliom_common.default_process_scope
        (fun _ _ ->
          return
             (html
               (head (title (pcdata "Cooooooooservices with timeouts")) [])
               (body [p
                 [pcdata "I am a coservice with timeout."; br ();
                  pcdata "Try to reload the page!"; br ();
                  pcdata "I will disappear after 5 seconds of inactivity.";
                  pcdata "Pour l'instant c'est un Eliom_output.Html5 au lieu de My_appl parce qu'il y a un bug à corriger dans ce cas. Remettre My_appl ici et ajouter un test pour ce bug." ];
                 ])))
(*            [p [pcdata "I am a coservice with timeout."; br ();
                a timeoutcoserv [pcdata "Try again"] (); br ();
                pcdata "I will disappear after 5 seconds of inactivity." ];
            ])
*)
    in
  Eliom_state.get_persistent_data ~table:tmy_persistent_table () >>= fun sessdat ->
  Lwt.return
    (make_page (match sessdat with
      | Eliom_state.Data name ->
        [p [pcdata ("Hello "^name); br ()];
         tdisconnect_box "Close session"]
      | Eliom_state.Data_session_expired ->
        [tlogin_box true tpersist_session_connect_action;
         p [em [pcdata "The only user is 'toto'."]]]
      | Eliom_state.No_data ->
        [tlogin_box false tpersist_session_connect_action;
         p [em [pcdata "The only user is 'toto'."]]]
    ))



(* ----------------------------------------------------------- *)
(* Handler for persist_session_connect_action (user logs in):  *)

let tpersist_session_connect_action_handler () login =
  lwt () = Eliom_state.discard ~scope () in
  if login = "toto" (* Check user and password :-) *)
  then
    Eliom_state.set_persistent_data ~table:tmy_persistent_table login
  else ((*zap* *)Polytables.set (Eliom_request_info.get_request_cache ()) bad_user_key true;(* *zap*)return ())



(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register
    ~service:tpersist_session_example
    tpersist_session_example_handler;
  Eliom_registration.Action.register
    ~service:tpersist_session_connect_action
    tpersist_session_connect_action_handler




(*
(************************************************************)
(************ Connection of users, version 6 ****************)
(************************************************************)
(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "connect_example6"
let scope = `Client_process scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let tconnect_example6 =
  Eliom_service.App.service
    ~path:["taction2"]
    ~get_params:unit
    ()


let tconnect_action =
  Eliom_service.App.post_coservice'
    ~name:"tconnect6"
    ~post_params:(string "login")
    ()


(* new disconnect action and box:                           *)

let tdisconnect_action =
  Eliom_registration.Action.register_post_coservice'
    ~name:"tdisconnect6"
    ~post_params:Eliom_parameter.unit
    (fun () () ->
      Eliom_state.discard (*zap* *) ~state_name (* *zap*) ~scope  ())


let tdisconnect_box s =
  Eliom_registration.Html5.post_form tdisconnect_action
    (fun _ -> [p [Html5.D.string_input
                    ~input_type:`Submit ~value:s ()]]) ()



let bad_user_key = Polytables.make_key ()

let get_bad_user table =
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false


(* -------------------------------------------------------- *)
(* new login box:                                           *)

let tlogin_box session_expired action =
  Html5.D.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         string_input ~input_type:`Text ~name:loginname ()]
      in
      [p (if get_bad_user (Eliom_request_info.get_request_cache ())
      then (pcdata "Wrong user")::(br ())::l
      else
        if session_expired
        then (pcdata "Session expired")::(br ())::l
        else l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "connect_example6" service (main page):   *)

let tconnect_example6_handler () () =
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~state_name (* *zap*) ()
  in
  let group = Eliom_state.get_volatile_data_session_group (*zap* *) ~state_name (* *zap*) ()
  in
  return
    (match group, status with
      | Some name, _ ->
        [p [pcdata ("Hello "^name); br ()];
         tdisconnect_box "Close session"]
      | None, Eliom_state.Expired_state ->
        [tlogin_box true tconnect_action;
         p [em [pcdata "The only user is 'toto'."]]]
      | _ ->
        [tlogin_box false tconnect_action;
         p [em [pcdata "The only user is 'toto'."]]]
    )


(* -------------------------------------------------------- *)
(* New handler for connect_action (user logs in):           *)

let tconnect_action_handler () login =
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope () >>= fun () ->
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_state.set_volatile_data_session_group ~set_max:4 (*zap* *) ~state_name (* *zap*) login;
    return ()
  end
  else begin
    Polytables.set (Eliom_request_info.get_request_cache ()) bad_user_key true;
    return ()
  end



(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register ~service:tconnect_example6 tconnect_example6_handler;
  Eliom_registration.Action.register ~service:tconnect_action tconnect_action_handler

*)





let csrf_scope = `Client_process Eliom_testsuite1.csrf_scope_hierarchy

let tcsrfsafe_example =
  Eliom_service.App.service
    ~path:["tcsrf"]
    ~get_params:Eliom_parameter.unit
    ()


let tcsrfsafe_example_post =
  Eliom_service.App.post_coservice
    ~csrf_safe:true
    ~csrf_scope
    ~csrf_secure:true
    ~timeout:10.
    ~max_use:1
    ~https:true
    ~fallback:tcsrfsafe_example
    ~post_params:Eliom_parameter.unit
    ()


let _ =
  let page () () =
    let l3 = Html5.D.post_form tcsrfsafe_example_post
        (fun _ -> [p [Html5.D.string_input
                         ~input_type:`Submit
                         ~value:"Click" ()]]) ()
    in
    Lwt.return
      (make_page
         [p [pcdata "A new coservice will be created each time this form is displayed. Your server must be running with HTTPS enabled. Clicking on the button should go to a new page (but this is probably broken for client process scope)."];
          l3])
  in
  My_appl.register tcsrfsafe_example page;
  My_appl.register tcsrfsafe_example_post
    (fun () () ->
      Lwt.return (make_page [p [pcdata "This is a CSRF safe service"]]))



(***** User cookies *****)
let cookiename = "mycookie"


let tcookies = App.service ["tcookies"] unit ()


let _ = My_appl.register tcookies
  (fun () () ->
    Eliom_state.set_cookie
      ~cookie_level:`Client_process
      ~name:cookiename ~value:(string_of_int (Random.int 100)) ();
    Lwt.return
      (make_page [p [pcdata "A new tab cookie is sent each time you load this page. If you reload, the cookie will be sent by the browser and you can observe the value changing."];
                  p [pcdata (try
                    "cookie value: "^
                      (CookiesTable.find
                         cookiename
                         (Eliom_request_info.get_cookies
                            ~cookie_level:`Client_process ()))
        with _ -> "<cookie not set>");
          br ();
          a tcookies [pcdata "send other cookie"] ()]]))






(***** Action outside the application:
       will ask the client program to do a redirection *****)

let coucouaction =
  Eliom_registration.Action.register_coservice
    ~fallback:Eliom_testsuite1.coucou
    ~get_params:unit
    (fun () () -> Lwt.return ())

let coucouaction2 =
  Eliom_registration.Action.register_coservice'
    ~get_params:unit
    (fun () () -> Lwt.return ())

let actionoutside =
  My_appl.register_service
    ~path:["actionoutside"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        (make_page
           [p [a ~service:coucouaction
                  [ pcdata "Click to do an action outside the application"] () ];
            p [a ~service:coucouaction2
                  [ pcdata "Click to do an action outside the application (with non-attached coservice)"] () ];
           ]))



(*****************************************************************************)
(* persistent references *)

let persref = Http.service ["persref"] unit ()

let _ =
  let next =
    let pr =
      Eliom_reference.eref
        ~scope:`Global ~persistent:"__eliom_example_persref" 0
    in
    let mutex = Lwt_mutex.create () in
    fun () ->
      Lwt_mutex.lock mutex >>= fun () ->
      Eliom_reference.get pr >>= fun v ->
      let v = v+1 in
      Eliom_reference.set pr v >>= fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return v
  in
  Eliom_registration.Html5.register persref
    (fun () () ->
      next () >>= fun v ->
      Lwt.return
        (html
           (head (title (pcdata "Persistent references")) [])
           (body
              [pcdata "This page has been viewed ";
               pcdata (string_of_int v);
               pcdata " times."]
           )
        )
    )



(*********)
let ttimeout = App.service ["ttimeout"] unit ()

let _ =
  let page () () =
    let timeoutcoserv =
      Eliom_service.App.coservice
        ~fallback:ttimeout ~get_params:unit ~timeout:5. ()
    in
    let _ =
      My_appl.register ~service:timeoutcoserv
        ~scope:Eliom_common.default_process_scope
        (fun _ _ ->
          Lwt.return
            (make_page [p [pcdata "I am a coservice with timeout."; br ();
                a timeoutcoserv [pcdata "Try again"] (); br ();
                pcdata "I will disappear after 5 seconds of inactivity." ];
            ]))
    in
    return
      (make_page
         [h2 [pcdata "Client process coservices with timeouts"];
          p [pcdata "I just created a coservice with 5 seconds timeout."; br ();
             a timeoutcoserv [pcdata "Try it"] (); ];
         ])
  in
  My_appl.register ttimeout page



(*****************************************************************************)
let nonapplprocessservice = App.service ["nonapplprocessservice"] unit ()

let _ =
  let page () () =
    let serv =
      Eliom_registration.Ocaml.register_post_coservice'
        ~scope:Eliom_common.default_process_scope
        ~post_params:unit
        (fun () () -> Lwt.return [1; 2; 3])
    in
    let serv2 =
      Eliom_registration.Html5.register_coservice'
        ~scope:Eliom_common.default_process_scope
        ~get_params:unit
        (fun () () -> Lwt.return (html
                                    (head (title (pcdata "mmmh")) [])
                                    (body [p [pcdata "It works"]])))
    in
    Lwt.return
      (make_page
      [h2 [pcdata "Client process service not registered with My_appl"];
       p [pcdata "I just created two coservices with scope `Client_process but not registered with My_appl."; br ();
          span ~a:[a_class ["clickable"];
                   a_onclick
                     {{
                       fun _ ->
                         let body = Dom_html.document##body in
                         ignore (Eliom_client.call_ocaml_service ~service:%serv () () >|=
                         List.iter
                           (fun i -> Dom.appendChild body
                             (Dom_html.document##createTextNode
                                (Js.string (string_of_int i)))))
                      }}
                  ]
            [pcdata "Click to call it and receive Ocaml data (service registered with Eliom_registration.Ocaml)."];
          br ();
          pcdata "It works, because we send tab cookies with Eliom_client.call_service or Eliom_client.call_ocaml_service.";
          br ();
          Html5.D.a ~service:serv2 [pcdata "Here a link to an client process service outside the application."] ();
          pcdata " For now it does not work, because we do not send tab cookies for non My_appl services ... How to solve this?";
          br ();
          pcdata "Add a test of link to another application."
         ]
      ])
  in
  My_appl.register nonapplprocessservice page



(*****************************************************************************)
(* Session + My_appl *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "session_appl"
let session = `Session scope_hierarchy

let connect_example789 =
  Eliom_service.App.service ~path:["session_appl"] ~get_params:unit ()

let connect_action789 =
  Eliom_service.Http.post_coservice'
    ~name:"connection789"
    ~post_params:(string "login")
    ()

let disconnect_action789 =
  Eliom_registration.Action.register_post_coservice'
    ~name:"disconnection789"
    ~post_params:Eliom_parameter.unit
    (fun () () -> Eliom_state.discard ~scope:session ())

let disconnect_box s =
  Html5.D.post_form disconnect_action789
    (fun _ -> [p [Html5.D.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let bad_user = Eliom_reference.eref ~scope:Eliom_common.request_scope false

let user = Eliom_reference.eref ~scope:session None

let login_box session_expired bad_u action =
  Html5.D.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Html5.D.string_input ~input_type:`Text ~name:loginname ()]
      in
      [p (if bad_u
        then (pcdata "Wrong user")::(br ())::l
        else
          if session_expired
          then (pcdata "Session expired")::(br ())::l
          else l)
      ])
    ()

let connect_example_handler () () =
  let status = Eliom_state.volatile_data_state_status ~scope:session () in
  lwt bad_u = Eliom_reference.get bad_user in
  lwt u = Eliom_reference.get user in
  Lwt.return
    (make_page
       (match u, status with
         | Some name, _ ->
           [p [pcdata ("Hello "^name); br ()];
            disconnect_box "Close session"]
         | None, Eliom_state.Expired_state ->
           [login_box true bad_u connect_action789;
            p [em [pcdata "The only user is 'toto'."]]]
         | _ ->
           [login_box false bad_u connect_action789;
            p [em [pcdata "The only user is 'toto'."]]]
       ))

let connect_action_handler () login =
  lwt () = Eliom_state.discard ~scope:session () in
  if login = "toto"
  then Eliom_reference.set user (Some login)
  else Eliom_reference.set bad_user true

let () =
  My_appl.register ~service:connect_example789 connect_example_handler;
  Eliom_registration.Action.register ~service:connect_action789 connect_action_handler

(*****************************************************************************)
(* Form towards internal suffix service *)
let isuffixc =
  My_appl.register_service
    ~path:["isuffixc"]
    ~get_params:(suffix_prod (int "suff" ** all_suffix_string "endsuff") (int "i"))
    (fun ((suff, endsuff), i) () ->
      Lwt.return
        (make_page
           [p [pcdata "The suffix of the url is ";
               strong [pcdata (string_of_int suff)];
               pcdata " followed by ";
               strong [pcdata endsuff];
               pcdata " and i is equal to ";
               strong [pcdata (string_of_int i)]]]))

{shared{
let create_suffixformc ((suff, endsuff),i) =
    [pcdata "Form to an (internal appl) suffix service.";
     pcdata "Write an int for the suffix:";
     Html5.D.int_input ~input_type:`Text ~name:suff ();
     pcdata "Write a string: ";
     Html5.D.string_input ~input_type:`Text ~name:endsuff ();
     pcdata "Write an int: ";
     Html5.D.int_input ~input_type:`Text ~name:i ();
     Html5.D.string_input ~input_type:`Submit ~value:"Click" ()
    ]
}}

(*****************************************************************************)
(* Redirections and Eliom applications: *)
let appl_redir1 =
  Eliom_registration.Redirection.register_service
    ~path:["internalredir"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return eliomclient2)

let appl_redir2 =
  Eliom_registration.Redirection.register_service
    ~path:["externalredir"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return Eliom_testsuite1.coucou)

let appl_redir =
  My_appl.register_service
    ~path:["applredir"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
       (make_page  [p [
          a ~service:appl_redir1 [ pcdata "Link to a redirection inside the Eliom application"] ();
          br ();
          a ~service:appl_redir2 [ pcdata "Link to a redirection outside the Eliom application"] ();
         ];
         Html5.D.get_form ~service:appl_redir1
            (fun () ->
              [Html5.D.string_input ~input_type:`Submit ~value:"Form to a redirection inside the Eliom application" ()]
            );
         Html5.D.get_form ~service:appl_redir2
            (fun () ->
              [Html5.D.string_input ~input_type:`Submit ~value:"Form to a redirection outside the Eliom application" ()]
            )
        ]))


(*****************************************************************************)
(* Void coservices with Eliom applications: *)
let applvoid_redir =
  Eliom_registration.Redirection.register_post_coservice'
    ~name:"applvoidcoserv"
    ~post_params:Eliom_parameter.unit
    (fun () () -> Lwt.return Eliom_service.void_hidden_coservice')


(*****************************************************************************)
(* Form examples: *)
let postformc =
  Eliom_registration.Html5.register_post_service
    ~fallback:Eliom_testsuite1.coucou
    ~post_params:(Eliom_parameter.string "zzz")
    (fun () s -> Lwt.return (make_page [p [pcdata "Yo man. ";
                                           pcdata s]]))

module Another_appl =
  Eliom_registration.App (
    struct
      let application_name = "testsuite_client_bis"
    end)

let make_page_bis ?(css = []) content =
  html
    (head
       (title (pcdata "Eliom application example (bis)"))
       ([ style
           [pcdata "a,.clickable {color: #111188; cursor: pointer;}"];
          My_appl.application_script ();
        ] @ css))
    (body
       [h1 [pcdata "Eliom application"];
        header ();
        div content ] )

{client{
  module Another_appl = Eliom_registration.Html5
}}

let otherappl =
  Another_appl.register_service
    ~path:["other"; "appl"]
    ~get_params:unit
    (fun () () -> Lwt.return (make_page_bis [p [pcdata "I am another application"] ]))

let long_page = Eliom_service.App.service ~path:["fragment";"main"] ~get_params:unit ()

let _ =
  My_appl.register long_page
  (fun () () ->
    let rec list i n =
      if i >= n  then
        [Html5.F.li
          [Html5.D.a
              ~fragment:""
              ~service:long_page [pcdata ("Goto TOP")] ()]]
      else
        Html5.F.li ~a:[Html5.F.a_id ("id" ^string_of_int i)]
          [Html5.F.pcdata ("Item #" ^ string_of_int i);
           Html5.F.pcdata "; ";
           Html5.D.a
             ~fragment:("id" ^ string_of_int (n-i))
             ~service:long_page [pcdata ("Goto #" ^ string_of_int (n-i))] ();] :: list (i+1) n in
    Lwt.return
      (make_page [Html5.F.ul (list 1 100)]))

let service_with_get_params =
  Eliom_service.App.service ~path:["intgp"]
    ~get_params:(suffix_prod (string "s") (string "t")) ()

let _ =
  My_appl.register service_with_get_params
    (fun (s, t) () -> Lwt.return
      (make_page [p [pcdata "Check that spaces and accents in parameters are ok"];
                  p [pcdata s]; p [pcdata t]]))

{client{
  let pinger : unit Lwt.t option ref = ref None
  let rec loop t i r =
    debug "Ping %d %d" i !r; incr r;
    try_lwt Lwt_js.sleep t >> loop t (i+1) r with _ -> debug "Pinger cancelled"; Lwt.return ()
  let loop_counter = ref 0
  let () = debug "Application loading"
}}

let live1 = Eliom_service.App.service ["live";"one"] unit ()
let live2 = Eliom_service.App.service ["live";"two"] unit ()
let live3 = Eliom_service.App.service ["live";"three"] unit ()

let live_description =
  div [pcdata "This is an application with three pages. ";
       pcdata "When loading the application shows a message in the console. ";
       pcdata "When loading each page show a message in the console.";
       pcdata "The first page display \"Ping\" every 2 seconds in the console.";
       br ();
       pcdata "Try to navigate between page. Try to leave the application and to get back.";]

let live_links =
  div [ ul [li [Html5.D.a ~service:live1 [pcdata "Page one"] ()];
            li [Html5.D.a ~service:live2 [pcdata "Page two"] ()];
            li [Html5.D.a ~service:live3 [pcdata "Page threee"] ()]]]

let dead_links =
  div [ ul [li [Html5.D.a ~service:Eliom_testsuite1.coucou
                   [pcdata "Link to a service outside the application."] ()];
            li [Html5.D.a ~service:otherappl
                   [pcdata "Link to another application."] ()];]]

let () = My_appl.register ~service:live1 (fun () () ->
    ignore {unit{ Eliom_client.onload (fun _ -> debug "Page 1 loading"; pinger := Some (loop 2. 0 loop_counter)) }};
    ignore {unit{ Eliom_client.onunload (fun _ -> debug "Page 1 unloading"; Option.iter Lwt.cancel !pinger) }};
    Lwt.return
      (make_page [h1 [pcdata "Page one"]; live_description; live_links; dead_links]))

let () = My_appl.register ~service:live2 (fun () () ->
    ignore {unit{
      Eliom_client.onload (fun _ -> debug "Page 2 loading");
      Eliom_client.onunload (fun _ -> debug "Page 2 unloading")
    }};
    Lwt.return
      (make_page [h1 [pcdata "Page two"];live_description; live_links; dead_links]))

let () = My_appl.register ~service:live3 (fun () () ->
    ignore {unit{
      Eliom_client.onload (fun _ -> debug "Page 3 loading");
      Eliom_client.onunload (fun _ -> debug "Page 3 unloading")
    }};
    Lwt.return
      (make_page [h1 [pcdata "Page threee"]; live_description; live_links; dead_links]))


let formc = My_appl.register_service ["formc"] unit
  (fun () () ->
    let div = Html5.D.div [h3 [pcdata "Forms and links created on client side:"]] in
    ignore {unit{
      Eliom_client.onload
        (fun _ ->

          let l =
            [
              h4 [pcdata "to outside the application:"];

              p [Html5.D.a ~service:%Eliom_testsuite1.coucou
                   [pcdata "Link to a service outside the application."]
                   ()];
              p [Html5.D.a ~service:%Eliom_testsuite1.coucou_params
                   [pcdata "Link to a service outside the application, with params (unicode)"]
                   (1, (2, "tutu cccéccc+ccc"))];

             Html5.D.get_form ~service:%Eliom_testsuite1.coucou
               (fun () ->
                 [Html5.D.string_input ~input_type:`Submit ~value:"GET form to a service outside the Eliom application" ()]
               );

             Html5.D.post_form ~service:%Eliom_testsuite1.my_service_with_post_params
               (fun s ->
                 [Html5.D.string_input ~input_type:`Hidden ~name:s ~value:"plop" ();
                  Html5.D.string_input ~input_type:`Submit ~value:"POST form to a service outside the Eliom application" ()]
               )
               ();

              p [Html5.D.a ~service:%otherappl
                    [pcdata "Link to another application."]
                    ();
                ];

              Html5.D.get_form ~service:%otherappl
                (fun () ->
                  [Html5.D.string_input ~input_type:`Submit ~value:"GET form to another application" ();];
                );


             h4 [pcdata "inside the application — must not stop the process! (same random number in the container)."];

             p [Html5.D.a ~service:%long_page
                   [pcdata "Link to a service inside the application."]
                   ()];
             p [Html5.D.a ~service:%long_page
                  ~fragment:"id40"
                  [pcdata "Link to a service inside the application (fragment)."]
                  ()];
             p [Html5.D.a ~https:false ~service:%long_page
                   [pcdata "Link to a service inside the application (force http)."]
                   ()];
             p [Html5.D.a ~https:true ~service:%long_page
                   [pcdata "Link to a service inside the application (force https)."]
                   ()];
             p [Html5.D.a ~service:%service_with_get_params
                   [pcdata "Link to a service inside the application (GET parameters, with spaces and Unicode)."]
                   ("toto aaaéaaa+aaa", "tata oooéooo+ooo")];
             p
               ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                 a_onclick
                   ( fun _ -> ignore(Eliom_client.change_page
                                        ~service:%service_with_get_params
                                        ("toto aaaéaaa+aaa", "tata oooéooo+ooo") ())) ]
               [pcdata "Change page to a service inside the application (GET parameters, with spaces and Unicode)."];

             Html5.D.get_form ~service:%eliomclient1
               (fun () ->
                 [Html5.D.string_input ~input_type:`Submit ~value:"GET form to a service inside the Eliom application" ()]
               );

             Html5.D.post_form ~service:%postformc
               (fun s ->
                 [Html5.D.string_input ~input_type:`Submit ~name:s ~value:"POST form to a service inside the Eliom application" ()]
               )
               ();

             Html5.D.get_form %isuffixc create_suffixformc;

             Html5.D.post_form ~service:%applvoid_redir
               (fun () ->
                 [pcdata "POST form towards action with void service redirection. This must not stop the application (same random number in the container but not in the here: ";
                  pcdata (string_of_int (Random.int 1000));
                  pcdata ") ";
                  Html5.D.string_input ~input_type:`Submit ~value:"Click to send POST form to myself." ()]
               )
               ();

            ]
          in
          List.iter
            (fun e -> Dom.appendChild
              (Html5.To_dom.of_div %div)
              (Html5.To_dom.of_element e)) l)
       }};

    Lwt.return
      (make_page [

        h3 [pcdata "Forms and links created on server side:"];

        h4 [pcdata "to outside the application:"];

        p [Html5.D.a ~service:Eliom_testsuite1.coucou
              [pcdata "Link to a service outside the application."]
              ()];
        p [Html5.D.a ~service:Eliom_testsuite1.coucou_params
              [pcdata "Link to a service outside the application, with params (unicode)"]
              (1, (2, "tutu cccéccc+ccc"))];

        Html5.D.get_form ~service:Eliom_testsuite1.coucou
          (fun () ->
            [Html5.D.string_input ~input_type:`Submit ~value:"GET form to a service outside the Eliom application" ()]
          );

        Html5.D.post_form ~service:Eliom_testsuite1.my_service_with_post_params
          (fun s ->
            [Html5.D.string_input ~input_type:`Hidden ~name:s ~value:"plop" ();
             Html5.D.string_input ~input_type:`Submit ~value:"POST form to a service outside the Eliom application" ()]
          )
          ();

        p [Html5.D.a ~service:otherappl
              [pcdata "Link to another application."]
              ();
           pcdata " (The other appl won't work as it is not compiled)"];

        Html5.D.get_form ~service:otherappl
          (fun () ->
            [Html5.D.string_input ~input_type:`Submit ~value:"GET form to another application" ();]
          );


        h4 [pcdata "inside the application — must not stop the process! (same random number in the container)."];

        p [Html5.D.a ~service:long_page
              [pcdata "Link to a service inside the application."]
              ()];
        p [Html5.D.a ~service:long_page
             ~fragment:"id40"
              [pcdata "Link to a service inside the application (fragment)."]
              ()];
        p [Html5.D.a ~https:false ~service:long_page
              [pcdata "Link to a service inside the application (force http)."]
              ()];
        p [Html5.D.a ~https:true ~service:long_page
              [pcdata "Link to a service inside the application (force https)."]
              ()];
        p [Html5.D.a ~service:service_with_get_params
              [pcdata "Link to a service inside the application (GET parameters, with spaces and Unicode)."]
              ("toto aaaéaaa+aaa", "tata oooéooo+ooo")];

        Html5.D.get_form ~service:eliomclient1
          (fun () ->
            [Html5.D.string_input ~input_type:`Submit ~value:"GET form to a service inside the Eliom application" ()]
          );

        Html5.D.post_form ~service:postformc
          (fun s ->
            [Html5.D.string_input ~input_type:`Submit ~name:s ~value:"POST form to a service inside the Eliom application" ()]
          )
          ();

        Html5.D.get_form isuffixc create_suffixformc;

        Html5.D.post_form ~service:applvoid_redir
          (fun () ->
            [pcdata "POST form towards action with void service redirection. This must not stop the application (same random number in the container but not in the here: ";
             pcdata (string_of_int (Random.int 1000));
             pcdata ") ";
             Html5.D.string_input ~input_type:`Submit ~value:"Click to send POST form to myself." ()]
          )
          ();

        h4 [pcdata "Changing the URL without doing a request."];

        p [Html5.D.Raw.a
              ~a:[a_onclick {{fun _ ->
                Eliom_client.change_url ~service:%long_page
                  ~fragment:"id40"
                  ()
                             }}]
              [pcdata "Change the URL."]
          ];

        div;

      ]))

(*****************************************************************************)
(* Any with Eliom applications: *)

let any_service =
  Eliom_service.App.service
    ~path:["appl_any"]
    ~get_params:(Eliom_parameter.int "with_eliom_appl")
    ()

let any_service_fallback =
  My_appl.register_service
    ~path:["appl_any_"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return (make_page [p [pcdata "any_appl_ fallback"; br (); pcdata "it should never be called"]]))

let () =
  let make_content name =
    let sp = Eliom_common.get_sp () in
    let appl_name = sp.Eliom_common.sp_client_appl_name in
    let links f =
      span [f "version generated by Eliom_registration.Html5" 0;
            br ();
            f "version generated by My_appl" 1;
            br ();
            f "version generated by Another_appl" 2;
            br ();
            f "a file" 3;
            br ()]
    in
    [p
        [pcdata ("this page was generated by " ^ name);
         br ();
         links (fun text i -> Html5.D.a ~service:any_service [pcdata text] i);
         br ();
         pcdata "back button do not work after exiting application (with link to Eliom_registration.Html5) when the last request had post parameters: the post parameters does not appear in the url: it will lead to the fallback";
         br ();
         match appl_name with
           | None -> pcdata "last request was not from an application"
           | Some an -> pcdata ("last request was from application "^an)
        ]
    ]
  in
  let make_any = function
    | 0 ->
      Printf.printf "html5 case\n%!";
      Eliom_registration.appl_self_redirect Eliom_registration.Html5.send
        (html
           (head (title (pcdata "html5 content")) [])
           (body (make_content "Eliom_registration.Html5.send")))
    | 1 ->
      Printf.printf "my appl case\n%!";
      My_appl.send (make_page (make_content "My_appl.send"))
    | 2 ->
      Printf.printf "another appl case\n%!";
      Eliom_registration.appl_self_redirect Another_appl.send
        (make_page (make_content "Another_appl.send"))
    | _ ->
      Printf.printf "Files case\n%!";
      Eliom_registration.appl_self_redirect Eliom_registration.File.send "/var/www/ocsigen/tutorial/ocsigen5.png"
  in
  Eliom_registration.Any.register ~service:any_service
    (fun choice () -> make_any choice)

(*****************************************************************************)
(* Gracefull fail to external content *)

let never_shown_service =
  My_appl.register_service
    ~path:["service_hidden_by_a_file.html"]
    ~get_params:(Eliom_parameter.unit)
    (fun () () ->
      return
        (make_page
           [pcdata "this page should never appear: a file with the same name is hidding it";]))

let gracefull_fail_with_file =
  My_appl.register_service
    ~path:["gracefull_fail_with_file"]
    ~get_params:unit
    (fun () () ->
      return
        (make_page
           [Html5.D.a ~service:never_shown_service
               [pcdata "link to a service hidden by a file"] ();]))

(*****************************************************************************)
(* correct url with redirections *)

let redirected_src_service =
  My_appl.register_service
    ~path:["redirect_src"]
    ~get_params:(Eliom_parameter.unit)
    (fun () () ->
      return
        (make_page
           [pcdata "this page should never appear: a redirection happen before";]))

let redirected_dst_service =
  My_appl.register_service
    ~path:["redirect_dst"]
    ~get_params:(Eliom_parameter.unit)
    (fun () () ->
      return
        (make_page
           [pcdata "the url in the browser bar should contain redirect_dst and not redirect_src";]))

let appl_with_redirect_service =
  My_appl.register_service
    ~path:["appl_with_redirect"]
    ~get_params:unit
    (fun () () ->
      return
        (make_page
           [Html5.D.a ~service:redirected_src_service
               [pcdata "link to a service hidden by a redirection"] ();
            br ();
            pcdata "there should be a line like: <redirect suburl=\"redirect_src\" dest=\"http://localhost:8080/redirect_dst\"/> in the configuration file";
           ]))


(*****************************************************************************)
(* Actions with `NoReload option *)
let noreload_ref = ref 0

let noreload_action =
  Eliom_registration.Action.register_coservice'
    ~options:`NoReload
    ~get_params:unit
    (fun () () -> noreload_ref := !noreload_ref + 1; Lwt.return ())

let noreload_appl =
  My_appl.register_service
    ~path:["noreloadappl"]
    ~get_params:unit
    (fun () () ->
      return
        (html
         (head (title (pcdata "counter")) [])
         (body [p [pcdata (string_of_int (!noreload_ref)); br ();
                   Html5.D.a ~service:noreload_action
                     [pcdata "Click to increment the counter."] ();
                   br ();
                   pcdata "You should not see the result if you do not reload the page."
                  ]])))



(*****************************************************************************)
(* XHR form with files: *)

(* TODO: to be fixed in 2.1:
   need a new parameter for ocaml_call_service to send files or formulary.
   Now it uses Eliom_request.send_post_form which is low level and should not be exported

let page_content () ((((((case,radio),select),multi),text),pass),file) =
  Lwt_io.with_file ~mode:Lwt_io.input file.Ocsigen_extensions.tmp_filename Lwt_io.read
  >|=
  (fun contents ->
    [
      p [pcdata (if case then "checked" else "not checked")];
      p [pcdata
            (match radio with
              | None -> "no choice"
              | Some radio -> Printf.sprintf "radio = %i" radio)];
      p [pcdata (Printf.sprintf "select: %s" select)];
      p [pcdata (Printf.sprintf "selects: %s" (String.concat ", " multi))];
      p [pcdata (Printf.sprintf "text: %s" text)];
      p [pcdata (Printf.sprintf "pass: %s" pass)];
      p [pcdata (Printf.sprintf "file: name %s length %Li hash: %s"
                   file.Ocsigen_extensions.tmp_filename
                   file.Ocsigen_extensions.filesize
                   (Digest.to_hex (Digest.string contents)))];
    ])

let block_form_fallback = Eliom_registration.Blocks5.register_service
  ~path:["resultblocks"]
  ~get_params:unit
  (fun () () -> return [pcdata "nothing"])

let block_form_result = Eliom_registration.Blocks5.register_post_service
  ~post_params:((((((bool "case" ** (radio (int "radio")))
                    ** string "select")
                   ** set string "multi")
                  ** string "text")
                 ** string "password")
                ** file "file")
  ~fallback:block_form_fallback
  page_content

let make_xhr_form ((((((casename,radio),select),multi),text),pass),file) =
  [p [pcdata "check ?";
      bool_checkbox ~name:casename ()];
   p [pcdata "choose ?";
      int_radio ~name:radio ~value:1 ();
      int_radio ~name:radio ~value:42 ()];
   p [pcdata "select:";
       string_select ~name:select (Option ([],"toto",None,false))
         [Option ([],"tutu",Some (pcdata "tutu ?"),true);
          Optgroup ([],"machin",([],"chose",None,true),
                    [([],"chose2",None,false);
                     ([],"chose3",None,false)]);
         ];
     ];
   p [pcdata "multi:";
       string_multiple_select ~name:multi (Option ([],"toto",None,false))
         [Option ([],"tutu",Some (pcdata "tutu ?"),true);
          Optgroup ([],"machin",([],"chose",None,true),
                    [([],"chose2",None,false);
                     ([],"chose3",None,false)]);
         ];
     ];
   p [string_input ~name:text ~input_type:`Text ~value:"text" ()];
   p [string_input ~name:pass ~input_type:`Password ~value:"pass" ()];
   p [file_input ~name:file ()]]

let xhr_form_with_file = My_appl.register_service ["xhr_form_with_file"] unit
  (fun () () ->
    let form = Html5.D.(post_form block_form_result make_xhr_form ()) in
    let subpage = Html5.D.(div []) in
    let launch = p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
      a_onclick {{
        fun _ ->
          let uri = Eliom_uri.make_string_uri ~service:%block_form_result () in
          ignore (Eliom_request.send_post_form (Html5.To_dom.of_form %form) uri >|=
              (fun contents -> ( Html5.To_dom.of_div %subpage )##innerHTML <- (Js.string contents)))
      }}]
      [pcdata "send form with an xhr"]
    in
    Lwt.return
      (make_page
         [
           pcdata "this test need upload: add <uploaddir>/tmp/upload</uploaddir> to the configuration file";
           form; launch; subpage]))
*)


let global_div =
  Html5.Id.create_global_elt
    (p ~a:[a_onload {{ fun _ -> debug "Div1: plop once." }}]
       [pcdata "Div: ";
        span ~a:[a_onload {{ fun _ -> debug "Span inside Div1: plop once"}}]
          [pcdata "global"]])

let local_div =
  Html5.D.p ~a:[a_onload {{ fun _ -> debug "Div2: always plop." }}]
    [pcdata "Div2: ";
     span ~a:[a_onload {{ fun _ -> debug "Span inside Div2: always plop";}}]
       [pcdata "local"]]

let simple_div =
  p ~a:[a_onload {{ fun _ -> debug "Div3: always plop." }}]
    [pcdata "Div3: ";
     span ~a:[a_onload {{ fun _ -> debug "Span inside Div3: always plop";}}]
       [pcdata "classical"]]

let unique1 =
  Eliom_service.App.service
    ~path:["appl";"unique1"]
    ~get_params:Eliom_parameter.unit
    ()

let unique2 =
  Eliom_service.App.service
    ~path:["appl";"unique2"]
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  My_appl.register
    unique1
    (fun () () ->
      ignore {unit{Eliom_client.onload (fun _ -> debug "Load page 1") }};
      return
        (make_page [h1 [pcdata "Page 1"];
                    p [pcdata "This page contains three div with attached onload event.";
                       pcdata "Those events display debug messages in the console. ";
                       pcdata "One div is a global element, the other one is not. ";
                       Html5.D.a ~service:unique2
                         [pcdata "Follow this link"] ();
                       pcdata ", and then press the back button: ";
                       pcdata "only the onload event of the non-global div should be executed.";
                       pcdata "Each div contains a span with an attached onload event. They should be fired whenever the onload event is fired in their fathers."
                      ];
                    global_div; local_div; simple_div;
                   ]))

let body_onload =
  My_appl.register_service
    ["body_onload"]
    Eliom_parameter.unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "body onload")) [])
           (body ~a:[a_onload {{ fun _ ->debug "it works"}}]
              [
                p [pcdata "onload on the body element.\n There should be \"it works\" in the console"]; br ();
                p [pcdata "there will also probably be an error message (caml_closure_id... is not defined). It is not a problem, and we can't simply avoid it"];
              ])))

let _ =
  My_appl.register
    unique2
    (fun () () ->
      ignore {unit{Eliom_client.onload (fun _ -> debug "Load page 2") }};
      return
        (make_page [h1 [pcdata "Page 2"];
                    Html5.D.a ~service:unique1 [pcdata "Get back to Page 1."] ();
                    p [pcdata "Try also to reload this page before switching back to Page 1:";
                       pcdata "The onload event of the unique div should be executed ";
                       pcdata "(when it is appears for the first time)"];
                   ]))

let big_service =
  Eliom_service.App.service
    ~path:["big_page"]
    ~get_params:Eliom_parameter.unit
    ()

let rec big_page n =
  let link = Html5.D.a ~service:big_service [pcdata "same page"] () in
  (*let link = p ~a:[a_class ["toto"]] [span [pcdata "rien"]] in*)
  if n = 0
  then link
  else
    div [big_page (n-1);
         big_page (n-1);]

let _ =
 My_appl.register
   big_service
   (fun () () ->
     return
       (make_page [h1 [pcdata "Big page"];
                   div [big_page 12]]))

let relink_test =
  Eliom_service.App.service
    ~path:["relink_test"]
    ~get_params:Eliom_parameter.unit
    ()

let global_list = Html5.Id.create_global_elt (ul [li [pcdata "First element"]])
let local_list = Html5.D.ul [li [pcdata "First element"]]

let relink_page () =
  ignore {unit{
   Eliom_client.onload (fun _ ->
      debug "onload";
      put_li %global_list "Global.";
      put_li %local_list "Request.")
  }};
  [ div [pcdata "This div contains a global list sent by reference. While the application runs, there should be one new item in the list each time the page is loaded.";
         global_list];
    div [pcdata "This div contains a request list sent by reference. There should be only one item in the list.";
         local_list];
    div
      [Html5.D.a ~service:relink_test
         [pcdata "Same page"] ();
       pcdata " (This should add an item in the global list)";
       br ();
       Html5.D.a ~service:eliomclient1
         [pcdata "Another page inside the application"] ();
       pcdata " (If you use the back button to redisplay this page, there should be a new item in the global list)";
       br ();
       Html5.D.a ~service:Eliom_testsuite_base.main [pcdata "Outside
  application"] ();
       pcdata " (If you use the back button to redisplay this page, there should be only only item in the global list)";
       br ();
      ]
  ]

let _ =
 My_appl.register
   ~content_type:"text/html"
   ~service:relink_test
   (fun () () ->
     return
       (make_page (relink_page ())))

{client{

  let react_div ?a r =
    let init = React.S.value r in
    let node = Html5.D.div ?a init in
    let node_dom = Html5.To_dom.of_element node in
    let s = React.S.map (fun sons ->
      List.iter (fun n -> ignore (node_dom##removeChild((n:> Dom.node Js.t))))
        (Dom.list_of_nodeList (node_dom##childNodes));
      List.iter (fun n ->
        let n = Html5.To_dom.of_element n in
        ignore (node_dom##appendChild((n:> Dom.node Js.t))))
        sons) r in
    Lwt_react.S.keep s;
    node

  let count = ref 0

  let r,push = React.S.create 0

  let react_node node r =
    let _init = React.S.value r in
    let node_dom = Html5.To_dom.of_element node in
    let s = React.S.map (fun sons ->
      List.iter (fun n -> ignore (node_dom##removeChild((n:> Dom.node Js.t))))
        (Dom.list_of_nodeList (node_dom##childNodes));
      List.iter (fun n ->
        let n = Html5.To_dom.of_element n in
        ignore (node_dom##appendChild((n:> Dom.node Js.t))))
        sons) r in
    Lwt_react.S.keep s

}}

let react_example =
  Eliom_service.App.service
    ~path:["react_example"]
    ~get_params:Eliom_parameter.unit
    ()

let () =
  My_appl.register
    react_example
    (fun () () ->
      let click_div = Html5.D.div ~a:[a_onclick {{ fun _ -> push (incr count; !count) }}] [] in
      ignore {unit{
        Eliom_client.onload
        (fun _ ->
          react_node %click_div
            (React.S.map (fun i -> [pcdata (Printf.sprintf "value: %i" i)]) r))
      }};
      Lwt.return (make_page [click_div]))

(************ onload with caml service ***********)

let caml_service_with_onload' =
  Eliom_registration.Ocaml.register_service
    ~path:["caml_service_with_onload'"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      let node = Html5.D.div [pcdata "new div"] in
      ignore {unit{
        Eliom_client.onload
          (fun _ ->
             let node = Html5.To_dom.of_div %node in
             ignore (Dom_html.addEventListener node Dom_html.Event.click
                       (Dom_html.handler (fun _ -> Dom_html.window##alert(Js.string "clicked!"); Js._true))
                       Js._true);
             ())
      }};
      Lwt.return (node : Html5_types.div Html5.F.elt))

let caml_service_with_onload =
  My_appl.register_service
    ~path:["caml_service_with_onload"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      let click_div = div
        ~a:[a_onclick {{
          fun _ ->
            ignore (
              lwt node = Eliom_client.call_ocaml_service ~service:( %caml_service_with_onload' ) () () in
              let node = Html5.To_dom.of_div node in
              ignore (Dom_html.document##body##appendChild( (node:> Dom.node Js.t) ));
              Lwt.return ()
            )
        }}]
        [pcdata "click"] in
      Lwt.return (make_page [ pcdata "onload with caml call service. A node should appear when clicking. An alert should be displayed when clicking the new nodes.";
                              click_div]))

(***********************)
(* Request unique node *)

let rec dom_div_tree v width height =
  if height = 0 then
    (incr v; [pcdata (string_of_int !v ^ " - ")])
  else
    Array.to_list
      (Array.init width
         (fun i -> Html5.D.div (dom_div_tree v width (height-1))))
let dom_div_tree w h = dom_div_tree (ref 0) w h

let rec div_tree v width height =
  if height = 0 then
    (incr v; [pcdata (string_of_int !v ^ " - ")])
  else
    Array.to_list
      (Array.init width
         (fun i -> Html5.F.div (div_tree v width (height-1))))
let div_tree w h = div_tree (ref 0) w h

let domnodes_timings = Eliom_service.App.service
  ~path:["domnodes_timings"]
  ~get_params:(int "widht" ** int "height")
  ()

let nodes_timings = Eliom_service.App.service
  ~path:["nodes_timings"]
  ~get_params:(int "widht" ** int "height")
  ()

let rec power n m =
  if m = 0 then 1 else let h = power n (m/2) in if m mod 2 = 1 then h * h * n else h * h

{client{
  let change_target_value ev =
    let v =
      if !Eliom_config.debug_timings then
        "Deactivate timings"
      else
        "Activate timings"
    in
    Js.Opt.case (ev##target)
      (fun () -> assert false)
      (fun elt -> Js.Opt.case (Dom_html.CoerceTo.input (elt))
        (fun () -> assert false)
        (fun input -> input##value <- Js.string v));
}}

let activate_timings_button =
  Html5.Id.create_global_elt
    (Html5.F.string_input
       ~a:[ Html5.F.a_onclick {{
            fun ev ->
              Eliom_config.debug_timings := not (!Eliom_config.debug_timings);
              change_target_value ev;
            }}]
       ~input_type:`Submit
       ~value:"Activate timings" ())

let update_tree service w h =
  Html5.F.get_form ~service (fun (wn,hn) ->
    [ Html5.F.fieldset
        [ Html5.F.label ~a:[Html5.D.a_for wn] [pcdata "Tree width: "];
          Html5.F.int_input ~name:wn ~input_type:`Text ~value:w ();
          Html5.F.label ~a:[Html5.D.a_for wn] [pcdata "and height: "];
          Html5.F.int_input ~name:hn ~input_type:`Text ~value:h ();
          Html5.F.string_input ~input_type:`Submit ~value:"Update" ();
        ]
    ])

let _ = My_appl.register
  ~service:domnodes_timings
  (fun (w,h) () ->
    let div = Html5.F.div ~a:[a_style "display:none;"] (dom_div_tree w h) in
    Lwt.return
      (make_page
         [Html5.F.h2 [pcdata (Printf.sprintf "Huge tree of dom nodes (%d^%d = %d)"
                                w h (power w h))];
          Html5.F.p [pcdata "This page contains a hidden tree of dom nodes (a.k.a unique nodes of scope request). ";
                     pcdata "Activate timings and look into the console how the 'relink_request_nodes' value ";
                     pcdata "evolves when the number of unique nodes increase. Then compare timings on the ";
                     Html5.D.a nodes_timings
                       [pcdata "same page with non-unique nodes"] (w,h);
                     pcdata "."];
          activate_timings_button;
          update_tree domnodes_timings w h;
          div]))

let _ = My_appl.register
  ~service:nodes_timings
  (fun (w,h) () ->
    let div = Html5.F.div ~a:[a_style "display:none;"] (div_tree w h) in
    Lwt.return
      (make_page
         [Html5.F.h2 [pcdata (Printf.sprintf "Huge tree of classical nodes (%d^%d = %d)"
                                w h (power w h))];
          Html5.F.p [Html5.D.a domnodes_timings [pcdata "Back to unique nodes."] (w,h)];
          activate_timings_button;
          update_tree domnodes_timings w h;
          div]))

let shared_dom_nodes = My_appl.register_service
  ~path:["shared_dom_nodes"]
  ~get_params:unit
  (fun () () ->
    let li = Html5.D.li [pcdata "Shared item"] in
    let li_appl = Html5.Id.create_global_elt (Html5.F.li [pcdata "Shared item"]) in
    Lwt.return
      (make_page
         [Html5.F.h2 [pcdata "Multiple occurences of a unique node"];
          Html5.F.p [pcdata "The following list contains two occurences of a unique node items (of scope request). ";
                     pcdata "One between item A and item B ; one between B and C. ";
                     pcdata "Only the second one should be displayed."];
          Html5.F.ul [ Html5.F.li [pcdata "Non-shared item A"];
                       li;
                       Html5.F.li [pcdata "Non-shared item B"];
                       li;
                       Html5.F.li [pcdata "Non-shared item C"];];
          Html5.F.p [pcdata "It is possible that for a very short period of time the first one appears. ";
                     pcdata "However, programmer probably do not want to use multiple occurences of a unique node ";
                     pcdata "and this \"blink\" will be a good reminder of unique node misuse..."];
          Html5.F.p [pcdata "Same game with scope application."];
          Html5.F.ul [ Html5.F.li [pcdata "Non-shared item A"];
                       li_appl;
                       Html5.F.li [pcdata "Non-shared item B"];
                       li_appl;
                       Html5.F.li [pcdata "Non-shared item C"];];
         ]))


(**** TEMPLATE ****)

let tmpl1_page1 = Eliom_service.App.service
  ~path:["tmpl1";"page1"]
  ~get_params:unit
  ()

let tmpl1_page2 = Eliom_service.App.service
  ~path:["tmpl1";"page2"]
  ~get_params:unit
  ()

let tmpl1_page3 = Eliom_service.App.service
  ~path:["tmpl1";"page3"]
  ~get_params:unit
  ()


let tmpl2_page1 = Eliom_service.App.service
  ~path:["tmpl2";"page1"]
  ~get_params:unit
  ()

let tmpl2_page2 = Eliom_service.App.service
  ~path:["tmpl2";"page2"]
  ~get_params:unit
  ()

let tmpl_update (id : Html5_types.flow5 Html5.Id.id) (contents : Html5_types.flow5 Html5.elt list) = {{
  Eliom_client.onload
    (fun () ->
      debug "Update";
      Html5.Manip.Named.replaceChildren %id %contents)
}}
module Tmpl_1 = Eliom_registration.Eliom_tmpl(My_appl)(struct
  type t = Html5_types.flow5 Html5.elt list
  let name = "template_one"
  let content_id = Html5.Id.new_elt_id ()
  let make_page contents =
    Lwt.return
      Html5.F.(make_page
         [h2 [pcdata "Template #1"];
          ul [li [Html5.D.a ~service:tmpl1_page1 [pcdata "Page 1"] ()];
              li [Html5.D.a ~service:tmpl1_page2 [pcdata "Page 2"] ()];
              li [Html5.D.a ~service:tmpl1_page3 [pcdata "Page 3"] ()];
              li [Html5.D.a ~service:tmpl2_page1 [pcdata "Page 1 (tmpl2)"] ()];
              li [Html5.D.a ~service:tmpl2_page2 [pcdata "Page 2 (tmpl2)"] ()];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl1_page1 () ())}}]
                [pcdata "Click me 1 (change_page)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl1_page2 () ())}}]
                 [pcdata "Click me 2 (change_page)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl1_page3 () ())}}]
                 [pcdata "Click me 3 (change_page)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl2_page1 () ())}}]
                 [pcdata "Click me 1 (change_page, tmpl2)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl2_page2 () ())}}]
                 [pcdata "Click me 2 (change_page, tmpl2)"];
             ];
          Html5.Id.create_named_elt ~id:content_id (div contents)])
  let update  = tmpl_update content_id
end)

module Tmpl_2 = Eliom_registration.Eliom_tmpl(My_appl)(struct
  type t = Html5_types.flow5 Html5.elt list
  let name = "template_two"
  let content_id = Html5.Id.new_elt_id ()
  let make_page contents =
    Lwt.return
      Html5.F.(make_page
         [h2 [pcdata "Template #2"];
          ul [li [Html5.D.a ~service:tmpl1_page1 [pcdata "Page 1 (tmpl1)"] ()];
              li [Html5.D.a ~service:tmpl1_page2 [pcdata "Page 2 (tmpl1)"] ()];
              li [Html5.D.a ~service:tmpl1_page3 [pcdata "Page 3 (tmpl1)"] ()];
              li [Html5.D.a ~service:tmpl2_page1 [pcdata "Page 1"] ()];
              li [Html5.D.a ~service:tmpl2_page2 [pcdata "Page 2"] ()];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl1_page1 () ())}}]
                [pcdata "Click me 1 (change_page, tmpl1)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl1_page2 () ())}}]
                 [pcdata "Click me 2 (change_page, tmpl1)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl1_page3 () ())}}]
                 [pcdata "Click me 3 (change_page, tmpl1)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl2_page1 () ())}}]
                 [pcdata "Click me 1 (change_page)"];
              li ~a:[a_onclick {{ fun _ -> lwt_ignore(Eliom_client.change_page ~service:%tmpl2_page2 () ())}}]
                 [pcdata "Click me 2 (change_page)"];
             ];
          Html5.Id.create_named_elt ~id:content_id (div contents)])
  let update  = tmpl_update content_id
end)

let () = Tmpl_1.register ~service:tmpl1_page1
  (fun () () -> Lwt.return [h3 [pcdata "Page 1 with tmpl1"]])

let () = Tmpl_1.register ~service:tmpl1_page2
  (fun () () -> Lwt.return [h3 [pcdata "Page 2 with tmpl1" ]])

let () = Tmpl_1.register ~service:tmpl1_page3
  (fun () () -> Lwt.return [h3 [pcdata "Page 3 with tmpl1"]])

let () = Tmpl_2.register ~service:tmpl2_page1
  (fun () () -> Lwt.return [h3 [pcdata "Page 1 with tmpl2"]])

let () = Tmpl_2.register ~service:tmpl2_page2
  (fun () () -> Lwt.return [h3 [pcdata "Page 2 with tmpl2"]])


(**** HISTORY ****)

let hist_page1 = Eliom_service.App.service
  ~path:["hist";"page1"]
  ~get_params:unit
  ()

let hist_page2 = Eliom_service.App.service
  ~path:["hist";"page2"]
  ~get_params:unit
  ()

let hist_page3 = Eliom_service.App.service
  ~path:["hist";"page3"]
  ~get_params:unit
  ()


let hist_page4 = Eliom_service.App.service
  ~path:["hist";"page4"]
  ~get_params:unit
  ()

let hist_page5 = Eliom_service.App.service
  ~path:["hist";"page5"]
  ~get_params:unit
  ()

let make_hist_page contents =
  Html5.F.(make_page
             [h2 [pcdata "Test History"];
              ul [li [Html5.D.a ~service:hist_page1 [pcdata "Page 1"] ()];
                  li [Html5.D.a ~service:hist_page2 [pcdata "Page 2"] ()];
                  li [Html5.D.a ~service:hist_page3 [pcdata "Page 3"] ()];
                  li [Html5.D.a ~service:hist_page4 [pcdata "Page 4"] ()];
                  li [Html5.D.a ~service:hist_page5 [pcdata "Page 5"] ()];
                 ];
             div contents])

let () = My_appl.register ~service:hist_page1
  (fun () () -> Lwt.return (make_hist_page [h3 [pcdata "Page 1"]]))

let () = My_appl.register ~service:hist_page2
  (fun () () -> Lwt.return (make_hist_page [h3 [pcdata "Page 2"]]))

let () = My_appl.register ~service:hist_page3
  (fun () () -> Lwt.return (make_hist_page [h3 [pcdata "Page 3"]]))

let () = My_appl.register ~service:hist_page4
  (fun () () -> Lwt.return (make_hist_page [h3 [pcdata "Page 4"]]))

let () = My_appl.register ~service:hist_page5
  (fun () () -> Lwt.return (make_hist_page [h3 [pcdata "Page 5"]]))




(**************************************************************)

let nl_params =
  Eliom_parameter.make_non_localized_parameters
    ~prefix:"tutoeliom"
    ~name:"mynlparams"
    (Eliom_parameter.int "a" ** Eliom_parameter.string "s")

let nl_serv = App.service ~path:["appl_nlparams"] ~get_params:(unit) ()

let _ = My_appl.register
  ~service:nl_serv
  (fun () () ->
    Lwt.return (
      make_page [
        p [a ~service:nl_serv
              ~nl_params:(Eliom_parameter.add_nl_parameter
                            Eliom_parameter.empty_nl_params_set
                            nl_params
                            (22, "oh")
              )
              [pcdata "with nl params"]
              ();
           br ();
           a ~service:Eliom_service.void_hidden_coservice'
             [pcdata "without nl params"]
             ();
           pcdata "there is a problem here: click many times on \"witout nl params\" and inspect it";
          ];
      ]))


(***********)
let nlpost_entry =
  Eliom_service.App.service
    ~path:["appl_nlpost"]
    ~get_params:(Eliom_parameter.unit)
    ()

let nlpost =
  Eliom_service.App.post_coservice
    ~fallback:nlpost_entry
    ~name:"appl_nlpost"
    ~post_params:(Eliom_parameter.unit)
    ()

let nlpost_with_nlp =
  Eliom_service.add_non_localized_get_parameters
    nl_params nlpost

let create_form_nl s =
  (fun () -> [Html5.F.p [Html5.D.string_input ~input_type:`Submit ~value:s ()]])

let () = My_appl.register nlpost
  (fun () () ->
    let nlp =
      match Eliom_parameter.get_non_localized_get_parameters nl_params with
        | None -> "no non localised parameter"
        | Some _ -> "some non localised parameter" in
     Lwt.return
       Html5.F.(html
          (head (title (pcdata "")) [])
          (body [div [
            pcdata nlp; br();
            Html5.D.post_form nlpost_with_nlp (create_form_nl "with nl param") ((),(12, "ab"));
            br ();
            Html5.D.post_form nlpost (create_form_nl "without nl param") ();
          ]])))

let () = My_appl.register nlpost_entry
  (fun () () ->
     Lwt.return
       Html5.F.(html
          (head (title (pcdata "")) [])
          (body [div [
            Html5.D.post_form nlpost_with_nlp (create_form_nl "with nl param") ((),(12, "ab"));
            br ();
            Html5.D.post_form nlpost (create_form_nl "without nl param") ();
          ]])))


(********************************************************)
(* test external xhr ( and see if cookies are sent ) *)

let some_external_service =
  Eliom_service.Http.external_service ~prefix:"http://remysharp.com"
    ~path:["demo";"cors.php"]
    ~get_params:(Eliom_parameter.unit) ()

let external_xhr = App.service ~path:["external_xhr"] ~get_params:(unit) ()

let _ = My_appl.register
  ~service:external_xhr
  (fun () () ->
    Lwt.return (
      make_page [
        p ~a:[a_class ["clickable"];
          a_onclick {{
            fun _ ->
              debug "click";
              ignore (
                lwt r = Eliom_client.call_service ~service:%some_external_service () () in
                debug "result: %s" r;
                Lwt.return ())
          }}] [pcdata "click to do an external xhr"]
      ]))

(********************************************************)
(* Test Eliom_config.parse_config *)

let () =
  let elt1_a1 = ref None in
  let elt1_a2 = ref None in
  let elt1_init_called = ref false in
  let elt1_e1_pcdata = ref None in
  let elt2_other_attributes = ref [] in
  let elt2_other_elements = ref [] in
  let x1 = ref None in
  let x2 = ref None in
  Eliom_config.parse_config
    Ocsigen_extensions.Configuration.([
      element ~name:"optional-elt"
        ~init:(fun () -> elt1_init_called := true)
        ~attributes:[
          attribute ~name:"optional-attr"
            (fun a -> elt1_a1 := Some a);
          attribute ~name:"obligatory-attr" ~obligatory:true
            (fun a -> elt1_a2 := Some a);
        ]
        ~elements:[
          element ~name:"inner-obligatory-elt" ~obligatory:true
            ~pcdata:(fun str -> elt1_e1_pcdata := Some str)
            ()
        ]
        ();
      element ~name:"obligatory-elt" ~obligatory:true
        ~attributes:[
          attribute ~name:"optional-attr" (fun v -> x1 := Some v);
          attribute ~name:"obligatory-attr" ~obligatory:true (fun v -> x2 := Some v);
        ]
        ~other_attributes:(fun name value -> elt2_other_attributes := (name^"="^value) :: !elt2_other_attributes)
        ~other_elements:(fun name _ _ -> elt2_other_elements := name :: !elt2_other_elements)
        ()
    ]);
  Printf.printf "*************************************\n";
  Printf.printf "* Eliom_config.parse_config results *\n%!";
  Printf.printf "optional-elt@optional-attr: %s\n%!"
    (Option.get
       (fun () -> "---") !elt1_a1);
  Printf.printf "optional-elt@obligatory-attr: %s\n%!"
    (Option.get (fun () -> "---") !elt1_a2);
  Printf.printf "optional-elt init called: %b\n%!" !elt1_init_called;
  Printf.printf "optional-elt > obligatory-elt PCDATA: %s\n%!"
    (Option.get (fun () -> "---") !elt1_e1_pcdata);
  Printf.printf "obligatory-elt@optional-attr-b: %s\n%!"
    (Option.get (fun () -> "---") !x1);
  Printf.printf "obligatory-elt@obligatory-attr-b: %s\n%!"
    (Option.get (fun () -> "---") !x2);
  Printf.printf "obligatory-elt ATTRIBUTES: %s\n%!"
    (String.concat " " !elt2_other_attributes);
  Printf.printf "obligatory-elt ELEMENTS: %s\n%!"
    (String.concat " " !elt2_other_elements);
  Printf.printf "*************************************\n%!";
  ()


(********************************************************)
(* Test Eliom_service.static_dir *)
{shared{
  let image : _ Html5.elt =
    Html5.D.(
      img ~alt:"some static file"
        ~src:(Html5.D.make_uri
                ~service:(Eliom_service.static_dir ())
                ["some static file"])
        ()
    )
}}


(********************************************************)
(* Extensive test of states *)

let states_test =
  Eliom_service.App.service
    ~path:["states"; ""]
    ~get_params:(Eliom_parameter.unit)
    ()

let states_test_bis =
  Eliom_service.App.service
    ~path:["states"]
    ~get_params:(Eliom_parameter.suffix (string "group"))
    ()

let next =
  let c = ref 0 in
  (fun () -> c := !c + 1; !c)

let nexti _ = next ()

let () = My_appl.register states_test
  (fun () () ->
     Lwt.return
       Html5.F.(
         html
           (head (title (pcdata "States test")) [])
           (body [
             h1 [pcdata "Testing states for different scopes"];
             p [
               pcdata "This test is an extensive test of Eliom references of different scopes, accessed from inside or outside the state itself. To run this test, you need at least two different browsers, one with several tabs on ";
               Html5.D.a states_test_bis [pcdata "this page"] "A";
               pcdata " and another with several tabs on ";
               Html5.D.a states_test_bis [pcdata "this other page"] "B";
               pcdata ". All tabs of a same browser must be one the same page, because loading the page sets the session group. Read the instructions on these pages.";
             ]])))

let vgr =
  Eliom_reference.Volatile.eref_from_fun
    ~scope:Eliom_common.default_group_scope
    next

let vsr =
  Eliom_reference.Volatile.eref_from_fun
    ~scope:Eliom_common.default_session_scope
    next

let vpr =
  Eliom_reference.Volatile.eref_from_fun
    ~scope:Eliom_common.default_process_scope
    next

let pgr =
  Eliom_reference.eref_from_fun
    ~scope:Eliom_common.default_group_scope
    ~persistent:"pgr"
    next

let psr =
  Eliom_reference.eref_from_fun
    ~scope:Eliom_common.default_session_scope
    ~persistent:"psr"
    next

let ppr =
  Eliom_reference.eref_from_fun
    ~scope:Eliom_common.default_process_scope
    ~persistent:"ppr"
    next

let change_gr =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:unit
    (fun () () ->
      let () = Eliom_reference.Volatile.modify vgr nexti in
      Eliom_reference.modify pgr nexti)

let change_sr =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:unit
    (fun () () ->
      let () = Eliom_reference.Volatile.modify vsr nexti in
      Eliom_reference.modify psr nexti)

let change_pr =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:unit
    (fun () () ->
      let () = Eliom_reference.Volatile.modify vpr nexti in
      Eliom_reference.modify ppr nexti)

let change_other_gr =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:(string "g")
    (fun () g ->
      let vstate = Eliom_state.Ext.volatile_data_group_state g in
      let pstate = Eliom_state.Ext.persistent_data_group_state g in
      Eliom_reference.Volatile.Ext.modify vstate vgr nexti;
      Eliom_reference.Ext.modify pstate pgr nexti)

let change_other_sr =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:(string "g")
    (fun () g ->
      let vstate = Eliom_state.Ext.volatile_data_group_state g in
      lwt () =
        Eliom_state.Ext.iter_sub_states
          vstate
          (fun state -> Eliom_reference.Volatile.Ext.modify state vsr nexti;
            Lwt.return ())
      in
      let pstate = Eliom_state.Ext.persistent_data_group_state g in
      Eliom_state.Ext.iter_sub_states
        pstate
        (fun state -> Eliom_reference.Ext.modify state psr nexti))

let change_other_pr =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:(string "g")
    (fun () g ->
      let vstate = Eliom_state.Ext.volatile_data_group_state g in
      lwt () =
        Eliom_state.Ext.iter_sub_states
          vstate
          (fun state ->
            Eliom_state.Ext.iter_sub_states
              state
              (fun state ->
                Eliom_reference.Volatile.Ext.modify state vpr nexti;
                Lwt.return ()))
      in
      let pstate = Eliom_state.Ext.persistent_data_group_state g in
      Eliom_state.Ext.iter_sub_states
        pstate
        (fun state ->
          Eliom_state.Ext.iter_sub_states
            state
            (fun state -> Eliom_reference.Ext.modify state ppr nexti)))


let () = My_appl.register states_test_bis
  (fun group () ->
    let other_group = if group = "A" then "B" else "A" in
    Eliom_state.set_volatile_data_session_group ~set_max:4 group;
    lwt () = Eliom_state.set_persistent_data_session_group ~set_max:(Some 4) group in
    let vgr = Eliom_reference.Volatile.get vgr in
    let vsr = Eliom_reference.Volatile.get vsr in
    let vpr = Eliom_reference.Volatile.get vpr in
    lwt pgr = Eliom_reference.get pgr in
    lwt psr = Eliom_reference.get psr in
    lwt ppr = Eliom_reference.get ppr in
    Lwt.return
      Html5.F.(
        html
          (head (title (pcdata ("States test — group "^group))) [])
          (body [
            h1 [pcdata ("Testing states for different scopes — This browser session belongs to group "^group)];
            p [ pcdata ("These (persistent and data) sessions belongs to a group called \""^group^"\".")];
            p [
              pcdata "Here are the values of differents Eliom references. To update the values after a test, ";
              strong [Raw.a
                         ~a:[a_class ["clickable"];
                           a_onclick
                             {{ fun _ ->
                               ignore(Eliom_client.change_page
                                        ~service:%Eliom_service.void_coservice'
                                        () ())
                              }}]
                         [pcdata "click here"]];
              pcdata " (change_page to myself)."
            ];

            p [pcdata "(volatile/persistent) group references: ";
               strong [pcdata (string_of_int vgr)];
               pcdata "/";
               strong [pcdata (string_of_int pgr)];
               em [pcdata " — check that they are the same on all tabs, all browsers in this group"];
               br ();
               strong [Raw.a
                          ~a:[a_class ["clickable"];
                              a_onclick
                                {{ fun _ ->
                                  ignore(Eliom_client.call_ocaml_service
                                           ~service:%change_gr
                                           () ())
                                 }}]
                          [pcdata "Click here to change the values on server side"]];
               pcdata ", then update and check.";
               br ();
               strong [Raw.a
                          ~a:[a_class ["clickable"];
                              a_onclick
                                {{ fun _ ->
                                  ignore(Eliom_client.call_ocaml_service
                                           ~service:%change_other_gr
                                           () %other_group)
                                 }}]
                          [pcdata "Click here to change the values on server side for the other group"]];
               pcdata ", then update and check group references on browsers belonging to the other group.";

              ];

            p [pcdata "(volatile/persistent) session references: ";
               strong [pcdata (string_of_int vsr)];
               pcdata "/";
               strong [pcdata (string_of_int psr)];
               em [pcdata " — check that they are the same on all tabs of this browser, but not other browsers."];
               br ();
               strong [Raw.a
                          ~a:[a_class ["clickable"];
                              a_onclick
                                {{ fun _ ->
                                  ignore(Eliom_client.call_ocaml_service
                                           ~service:%change_sr
                                           () ())
                                 }}]
                          [pcdata "Click here to change the values on server side"]];
               pcdata ", then update and check.";
               br ();
               strong [Raw.a
                          ~a:[a_class ["clickable"];
                              a_onclick
                                {{ fun _ ->
                                  ignore(Eliom_client.call_ocaml_service
                                           ~service:%change_other_sr
                                           () %other_group)
                                 }}]
                          [pcdata "Click here to change the values on server side for the other group"]];
               pcdata ", then update and check session references on browsers belonging to the other group.";
              ];

            p [pcdata "(volatile/persistent) process references: ";
               strong [pcdata (string_of_int vpr)];
               pcdata "/";
               strong [pcdata (string_of_int ppr)];
               em [pcdata " — check that they are different on all tabs"];
               br ();
               strong [Raw.a
                          ~a:[a_class ["clickable"];
                              a_onclick
                                {{ fun _ ->
                                  ignore(Eliom_client.call_ocaml_service
                                           ~service:%change_pr
                                           () ())
                                 }}]
                          [pcdata "Click here to change the values on server side"]];
               pcdata ", then update and check.";
               br ();
               strong [Raw.a
                          ~a:[a_class ["clickable"];
                              a_onclick
                                {{ fun _ ->
                                  ignore(Eliom_client.call_ocaml_service
                                           ~service:%change_other_pr
                                           () %other_group)
                                 }}]
                          [pcdata "Click here to change the values on server side for the other group"]];
               pcdata ", then update and check on browsers belonging to the other group.";
              ];

          ])))
