(*zap* *)
{shared{
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
}}

(* Main page for the test suite *)
let main = Eliom_services.service [] Eliom_parameters.unit ()

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
Eliom allows to write the client and server parts of a Web application
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
open XHTML5.M
}}

(****** server only *******)
{server{ (* note that {server{ ... }} is optionnal. *)
open Eliom_parameters
open Eliom_output.Xhtml5compact
open Eliom_services
}}

(* This is server only because there are no delimiters. *)
module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "eliom_testsuite3"
      let params =
        {Eliom_output.default_appl_params with
           Eliom_output.ap_title = "Eliom application example";
           Eliom_output.ap_headers_before =
            [XHTML5.M.style
               [XHTML5.M.pcdata "a,.clickable {color: #111188; cursor: pointer;}"]];
           Eliom_output.ap_container =
            Some (None,
                  fun div ->
                    [h1 [pcdata "Eliom application"];
                     p [pcdata "Random value in the container: ";
                        pcdata (string_of_int (Random.int 1000)); br ();
                        a ~service:main [pcdata "Back to the main page of the test suite."] ();];
                     div ])
        }
    end)
(*wiki* Now I can define my first service belonging to that application: *wiki*)

{server{ (* note that {server{ ... }} is optionnal. *)
open My_appl
}}

let eliomclient1 =
  My_appl.register_service
    ~path:["plop"; "eliomclient1"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                        (* with {{ expr }}, the expression is executed by the client. *)
          a_onclick {{Dom_html.window##alert(Js.string "clicked!") ; Lwt.return ()}}]
            [pcdata "I am a clickable paragraph"];

        ])
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
let eliomclient2 = service ~path:["plip"; "eliomclient2"] ~get_params:unit ()

let myblockservice =
  Eliom_output.Blocks5.register_post_coservice
    ~fallback:eliomclient2
    ~post_params:unit
    (fun () () ->
       Lwt.return
         [p [pcdata ("I come from a distant service! Here is a random value: "^
                       string_of_int (Random.int 100))]])

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
        [
(*wiki*
  The following example shows how to go to another service,
  exactly like pressing a link (here a service that do not belong to
  the application):
*wiki*)
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick
                {{Eliom_client.exit_to
                    ~service: %Eliom_testsuite1.coucou (* just as [coucou] *)
                    () ()
                }}
            ]
            [pcdata "Link to a service outside the Eliom application"];

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
                    (myblockservice : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.http) Eliom_services.service) ->
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
  The following examples shows how to change the URL.
  This is a low level function and is usually not to be used directly.
  As browsers do not not allow to change the URL,
  we write the new URL in the fragment part of the URL.
  A script must do the redirection if there is something in the fragment
  while the page is loading.
*wiki*)
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_url
                  ~service: %Eliom_testsuite1.coucou
                  () ()
              }}
            ]
            [pcdata "Click here to change the URL."];

(*wiki*
  The following examples shows how to change the current page,
  without stopping the client side program.
*wiki*)
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick
                {{Eliom_client.change_page
                    ~service:%eliomclient1
                    () ()
                }}
            ]
            [pcdata "Click here to change the page without stopping the program."];

(*wiki* Actually the usual {{{a}}} function to create link will
  use {{{change_page}}} if you do a link inside the same application.
  The latter example is equivalent to the following. *wiki*)
          p [a (*zap* *) ~a:[a_class ["clickable"]](* *zap*)
               ~service:eliomclient1
               [pcdata "Click here to change the page without stopping the program (with ";
                code [pcdata "a"];
                pcdata ")."]
               ()];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick{{
                Eliom_client.change_page ~service:%Eliom_testsuite1.coucou
                  () ()
              }}
            ]
            [pcdata "Click here to go to a page outside the application, using ";
             code [pcdata "change_page"];
             pcdata "."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.exit_to ~service:%eliomclient2 () ()
              }}
            ]
            [pcdata "Click here to relaunch the program by reloading the page."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_page ~service:%eliomclient1
                  () ()
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
                Eliom_client.get_subpage ~service:%eliomclient1
                  () () >|= fun blocks ->
                List.iter
                  (Dom.appendChild Dom_html.document##body)
                  (XHTML5.M.toeltl blocks)
              }}
            ]
            [pcdata "Click here to get a subpage from server."];


(*wiki*
====Refering to parts of the page in client side code
*wiki*)

          (let container = ul [ item () ; item () ; item ()] in
           div [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                               a_onclick {{
                                 Dom.appendChild
                                   %container (* node is the wrapper keyword for XHTML5.M nodes. *)
                                   (XHTML5.M.toelt (item ()))
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
                {{ Dom_html.window##alert
                     (Js.string (string_of_float %my_value));
                   Lwt.return ()
                }}
                ]
             [pcdata "Click here to see a server side value sent with the page."]);



(*wiki*
====Other tests
  *wiki*)

          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                let coucou = %Eliom_testsuite1.coucou in
                let eliomclient1 = %eliomclient1 in
                (Dom.appendChild
                   (Dom_html.document##body)
                   (XHTML5.M.toelt
                      (p [Eliom_output.Xhtml5.a
                            ~service:coucou
                            [pcdata "An external link generated client side"]
                            ();
                          pcdata ", ";
                          Eliom_output.Xhtml5.a
                            (*zap* *)~a:[a_class ["clickable"]](* *zap*)
                            ~service:eliomclient1
                            [pcdata "another, inside the application, but wrongly created with Eliom_output.Xhtml5 (what module to use client side? how to avoid using Eliom_output.Xhtml5?). "]
                            ();
                          pcdata " and ";
                          span
                            ~a:[a_class ["clickable"];
                                a_onclick (fun () -> Dom_html.window##alert(Js.string "clicked!"))]
                            [pcdata "Here a client-side span with onclick"]
                         ]
                      ))
                );
                Lwt.return ()
              }}
            ]
            [pcdata "Click here to add client side generated links."];



        ])


(*wiki*
====Using OCaml values as service parameters
It is now possible to send OCaml values to services.
To do that, use the {{{Eliom_parameters.caml}}} function:
*wiki*)
type ec3 = (int * string * string list) deriving (Json)

let eliomclient3' =
  My_appl.register_post_coservice'
    ~post_params:(caml "isb" Json.t<ec3>)
    (fun () (i, s, l) ->
      Lwt.return
        [p (pcdata (Printf.sprintf "i = %d, s = %s" i s)::
              List.map (fun a -> pcdata a) l
           )])


let eliomclient3 =
  My_appl.register_service
    ~path:["eliomclient3"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
                {{ Eliom_client.change_page
                     ~service:%eliomclient3'
                     () (299, "oo", ["a";"b";"c"])
                }}
              ]
           [pcdata "Click to send Ocaml data"]
        ])

(*wiki*
====Sending OCaml values using services
It is possible to do services that send any caml value. For example:
*wiki*)
let eliomclient4' =
  Eliom_output.Caml.register_post_coservice'
    ~post_params:unit
    (fun () () -> Lwt.return [1; 2; 3])


let eliomclient4 =
  My_appl.register_service
    ~path:["eliomclient4"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
                 {{let body = Dom_html.document##body in
                   Eliom_client.call_caml_service
                     ~service:%eliomclient4'
                     () () >|=
                   List.iter
                     (fun i -> Dom.appendChild body
                                 (Dom_html.document##createTextNode
                                    (Js.string (string_of_int i))))
                 }}
              ]
           [pcdata "Click to receive Ocaml data"]
        ])

(*wiki*
====Other tests:
*wiki*)
let withoutclient =
  Eliom_services.service
    ~path:["withoutclient"]
    ~get_params:unit
    ()


let gotowithoutclient =
  Eliom_services.service
    ~path:["gotowithoutclient"]
    ~get_params:unit
    ()



let _ =
  My_appl.register
    ~options:{Eliom_output.default_appl_service_options
              with Eliom_output.do_not_launch = true}
    ~service:withoutclient
    (fun () () ->
       Lwt.return
         [p [pcdata "If the application was not launched before coming here (or if you reload), this page will not launch it. But if it was launched before, it is still running."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_page
                  ~service:%gotowithoutclient
                  () ()
              }}
            ]
            [pcdata "Click here to go to a page that launches the application every time (this link does not work if the appl is not launched)."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~service:gotowithoutclient
               [pcdata "Same link with ";
                code [pcdata "a"]; pcdata "."] ()];
         ]);
  My_appl.register
    ~service:gotowithoutclient
    (fun () () ->
       Lwt.return
         [p [pcdata "The application is launched."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_page
                  ~service:%withoutclient
                  () ()
              }}
            ]
            [pcdata "Click here to see the page that does not launch the application."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~service:withoutclient
               [pcdata "Same link with ";
                code [pcdata "a"]; pcdata "."] ()];
         ])





let on_load =
  My_appl.register_service
    ~path:["onload"]
    ~get_params:unit
    (fun () () ->
      let div =
        div [p [a ~service:eliomclient1 [pcdata "go to another page"] ()] ]
      in
      Eliom_services.onload
        {{ Lwt_js.sleep 1. >|= fun () ->
           Dom.appendChild %div
             (XHTML5.M.toelt (p [pcdata "on_load executed after 1s."]))
         }};
      Eliom_services.onload
        {{ Lwt_js.sleep 3. >|= fun () ->
           Dom.appendChild %div
             (XHTML5.M.toelt (p [pcdata "on_load executed after 3s."]))
         }};
      Eliom_services.onunload
        {{
          Dom.appendChild %div
          (XHTML5.M.toelt (p [pcdata "on_unload executed. Waiting 1s."]));
          Lwt_js.sleep 1.
        }};
      Lwt.return [div]
    )

let uri_test =
  My_appl.register_service
    ~path:["uritest"]
    ~get_params:unit
    (fun () () ->
      let div =
        div [
          p [pcdata "The following URLs are computed either on server or client side. They should be equal."];
          p [pcdata (Eliom_uri.make_string_uri ~service:eliomclient1 ())];
            ]
      in
      Eliom_services.onload
        {{ Dom.appendChild %div
             (XHTML5.M.toelt (p [pcdata (Eliom_uri.make_string_uri ~service:%eliomclient1 ())]))
         }};
      Lwt.return [div]
    )

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
    v_service = eliomclient1 }
    
let rec rec_list = 1::2::3::rec_list

let react_up = Eliom_react.Up.create (Eliom_parameters.caml "react param" Json.t<int>)
let e = Eliom_react.Up.to_react react_up
let e' = React.E.map (fun i -> Printf.printf "event: %i\n%!" i) e

let rec rec_list_react = (react_up,42)::rec_list_react

{client{
  let add_body v =
    Dom.appendChild (Dom_html.document##body) v

  let put f =
    Printf.ksprintf (fun s ->
      add_body
        (XHTML5.M.toelt (p [pcdata s]))) f
}}

let wrapping1 =
  Eliom_appl.register_service
    ~path:["wrapping1"]
    ~get_params:unit
    (fun () () ->
      let div = div [] in
      Eliom_services.onload
	{{ 
	  let v = %v1 in
	  put "42=%i 42.42=%f fourty two=%s" v.Wrapping_test.v_int v.Wrapping_test.v_float v.Wrapping_test.v_string;
	  ( match %rec_list with
	    | a::b::c::d::e::f::g::_ ->
	      put "%i::%i::%i::%i::%i::%i::%i::..." a b c d e f g;
	    | _ -> put "problem with recursive list"; );

          add_body
            (XHTML5.M.toelt
	       (p ~a:[ a_onclick
			 (fun _ ->
			   ignore (Eliom_client.get_subpage ~service:v.Wrapping_test.v_service
				     () () >|= (fun blocks ->
				       List.iter
					 (Dom.appendChild Dom_html.document##body)
					 (XHTML5.M.toeltl blocks);)))] [pcdata "test serice"]));

	  let f_react = fst (List.hd %rec_list_react) in

          add_body
            (XHTML5.M.toelt
	       (p ~a:[ a_onclick (fun _ -> f_react 42)] [pcdata "test react service: event 42 should apear on stdout (of the server) when this is clicked "]));


	}};
      Lwt.return [div])

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
       let c1 = Eliom_comet.Channels.create (Lwt_stream.clone stream1) in

       let tick2 =
	 let i = ref 0 in
	 fun () ->
	   Lwt_unix.sleep (float_of_int (6 + (Random.int 6))) >>= fun () ->
	   incr i; Lwt.return (Some !i)
       in
       let stream2 = Lwt_stream.from tick2 in
       let c2 = Eliom_comet.Channels.create stream2 in

       Eliom_services.onload
         {{
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
	   ()
         }};

       Lwt.return
         [
           div
             [pcdata "To fully understand the meaning of the public channel, \
                      use a couple browsers on this page."] ;
         ]
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
      let e_up = Eliom_react.Up.create (Eliom_parameters.caml "letter" Json.t<string>) in
      let e_up_react = Eliom_react.Up.to_react e_up in
      let e_down =
        Eliom_react.Down.of_react
          (React.E.map
             (function "A" -> "alpha" | "B" -> "beta" | _ -> "what ?")
             e_up_react
          )
      in
      let `R _ = React.E.retain e_up_react (fun () -> ignore e_down) in
      Eliom_services.onload
        {{
          React.E.map
            (fun s -> Dom_html.window##alert (Js.string s))
            %e_down
        }};

      (* We can send the page *)
      Lwt.return [
         h2 [pcdata "Dual events"] ;
         div (* This div is for pushing "A" to the server side event *)
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ %e_up "A" }} ]
           [pcdata "Push A"] ;
         div (* This one is for pushing "B" *)
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ %e_up "B" }} ]
           [pcdata "Push B"] ;
       ]
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
       let e_up = Eliom_react.Up.create (Eliom_parameters.caml "double" Json.t<string>) in
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
       Eliom_services.onload
         {{
           React.E.map
           (fun s -> Dom_html.window##alert (Js.string s))
           (React.E.merge
              (^) ""
              [ React.E.map string_of_int %e_down_1 ;
                %e_down_2 ;
              ]
           )
         }};

       (* We can send the page *)
       Lwt.return [
         h2 [pcdata "Simultaneous events"] ;
         div
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ %e_up "" }} ]
           [pcdata "Send me two values from different events !"] ;
       ]
    )


(*wiki*
 Here is the code for a minimalistic message board.
 *wiki*)

let message_bus = Eliom_bus.create Json.t<string>
let _ =
  Lwt_stream.iter (fun msg -> Printf.printf "msg: %s\n%!" msg)
    (Eliom_bus.stream message_bus)

let comet_message_board =
  My_appl.register_service
    ~path:["message_board"]
    ~get_params:unit
    (fun () () ->

       Lwt.return (
         let container = ul [li [em [pcdata "This is the message board"]]] in
         let field = input ~a:[a_id "msg"; a_input_type `Text; a_name "message"] () in
         Eliom_services.onload
           {{
             let _ = Lwt_stream.iter_s
               (fun msg ->
                 Dom.appendChild %container
                   (XHTML5.M.toelt (li [pcdata msg]));
                 Lwt.return ()
               )
	       (Eliom_client_bus.stream %message_bus)
	     in ()
           }} ;

         let go =
           div
             ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                  a_onclick {{
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
                    Eliom_client_bus.write %message_bus v
                  }}
             ]
             [pcdata "send"]
         in

         [ h2 [pcdata "Message board"];
           form ~a:[a_action (uri_of_string "")] (div [field; go]) [];
           container;
         ])
    )

(*wiki*
===Events with arrows

 *wiki*)

{client{
open Event_arrows
}}

let event_service =
  My_appl.register_service
    ~path:["events"]
    ~get_params:Eliom_parameters.unit
    (fun () () ->

      let make_target s = XHTML5.M.p [XHTML5.M.a [XHTML5.M.pcdata s]] in
      let target1 = make_target "Un seul clic" in
      let target2 = make_target "Annuler le précédent" in
      let target3 = make_target "Drag vers la ligne au dessus une seule fois" in
      let target4 = make_target "Plein de clics seq" in
      let target5 = make_target "Annuler le précédent" in
      let target6 = make_target "Deux clics" in
      let target7 = make_target "Un clic sur deux" in
      let target8 = make_target "Un clic, puis tous les suivants" in
      let target9 = make_target "Un des deux, premier" in
      let target10 = make_target "Un des deux, deuxieme" in
      let target11 = make_target "Annuler les deux précédents" in
      let target12 = make_target "Drag" in
      let target13 = make_target "Annuler le précédent" in
      let target14 = make_target "Drag with long handler" in
      let target15 = make_target "Annuler le précédent" in

      let targetresult = XHTML5.M.p [] in
      Eliom_services.onload
        {{
          let target1 = %target1 in
          let target2 = %target2 in
          let target3 = %target3 in
          let target4 = %target4 in
          let target5 = %target5 in
          let target6 = %target6 in
          let target7 = %target7 in
          let target8 = %target8 in
          let target9 = %target9 in
          let target10 = %target10 in
          let target11 = %target11 in
          let target12 = %target12 in
          let target13 = %target13 in
          let target14 = %target14 in
          let target15 = %target15 in

          let targetresult = %targetresult in
    
          let handler =
            lwt_arr
              (fun ev ->
                ignore (targetresult##appendChild
                          (XHTML5.M.toelt (XHTML5.M.pcdata " plip")));
                Lwt.return ())
          in
          let handler_long =
            lwt_arr
              (fun ev ->
                Lwt_js.sleep 0.7 >>= fun () ->
                ignore (targetresult##appendChild
                          (XHTML5.M.toelt (XHTML5.M.pcdata " plop")));
                Lwt.return ()
              )
          in
          let cancel c = arr (fun _ -> cancel c) in
          let c = run (click target1 >>> handler) () in
          let _ = run (click target2 >>> cancel c) () in
          let _ = run (mousedown target3 >>> mouseup target2 >>> handler) () in
          let c = run (clicks target4 handler_long) () in
          let _ = run (click target5 >>> cancel c) () in
          let _ = run (click target6 >>> handler >>> click target6 >>> handler) () in
          let _ = run (clicks target7 (click target7 >>> handler)) () in
          let _ = run (click target8 >>> clicks target8 handler) () in
          let c = run (first [click target9 >>> handler;
                              click target10 >>> handler]) ()
          in
          let _ = run (click target11 >>> cancel c) ()
          in
          let c = run (mousedowns target12 
                         (first [mouseup Dom_html.document;
                                 mousemoves Dom_html.document handler])) ()
          in
          let _ = run (click target13 >>> cancel c) ()
          in
          let c = run (mousedowns target14
                         (first [mouseup Dom_html.document;
                                 mousemoves Dom_html.document handler_long])) ()
          in
          let _ = run (click target15 >>> cancel c) ()
          in
          ()

        }};

       Lwt.return [target1; target2; target3; target4; target5; target6;
                   target7; target8; target9; target10; target11;
                   target12; target13; target14; target15;
                   targetresult]
    )



(*wiki*

===Tab sessions

  *wiki*)


open Lwt


(************************************************************)
(************ Connection of users, version 1 ****************)
(************************************************************)

(*zap* *)
let state_name = "tsession_data"

(* *zap*)

(* "my_table" will be the structure used to store
   the session data (namely the login name): *)

let my_table = Eliom_state.create_volatile_table ~state_name ~scope:`Client_process ()



(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let tsession_data_example =
  Eliom_services.service
    ~path:["tsessdata"]
    ~get_params:Eliom_parameters.unit
    ()


let tsession_data_example_with_post_params =
  Eliom_services.post_service
    ~fallback:tsession_data_example
    ~post_params:(Eliom_parameters.string "login")
    ()


let tsession_data_example_close =
  Eliom_services.service
    ~path:["tclose"]
    ~get_params:Eliom_parameters.unit
    ()




(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example" service:          *)

let tsession_data_example_handler _ _  =
  let sessdat = 
    Eliom_state.get_volatile_data ~table:my_table () 
  in
  return
    [
      match sessdat with
        | Eliom_state.Data name ->
          p [pcdata ("Hello "^name);
             br ();
             My_appl.a
               tsession_data_example_close
               [pcdata "close session"] ()]
        | Eliom_state.Data_session_expired
        | Eliom_state.No_data ->
          My_appl.post_form
            tsession_data_example_with_post_params
            (fun login ->
              [p [pcdata "login: ";
                  My_appl.string_input
                    ~input_type:`Text ~name:login ()]]) ()
    ]


(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_with_post_params"  *)
(* service with POST params:                                *)

let tsession_data_example_with_post_params_handler _ login =
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
  Eliom_state.set_volatile_data
    ~table:my_table login;
  return
    [p [pcdata ("Welcome " ^ login ^ ". You are now connected.");
        br ();
        My_appl.a tsession_data_example [pcdata "Try again"] ()
       ]]




(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_close" service:    *)

let tsession_data_example_close_handler () () =
  let sessdat = Eliom_state.get_volatile_data
    ~table:my_table () in
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
  return
    [
      (match sessdat with
        | Eliom_state.Data_session_expired -> p [pcdata "Your session has expired."]
        | Eliom_state.No_data -> p [pcdata "You were not connected."]
        | Eliom_state.Data _ -> p [pcdata "You have been disconnected."]);
      p [My_appl.a tsession_data_example [pcdata "Retry"] () ]]



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
let state_name = "tsession_services"

(* *zap*)
(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let tsession_services_example =
  Eliom_services.service
    ~path:["tsessionservices"]
    ~get_params:Eliom_parameters.unit
    ()


let tsession_services_example_with_post_params =
  Eliom_services.post_service
    ~fallback:tsession_services_example
    ~post_params:(Eliom_parameters.string "login")
    ()


let tsession_services_example_close =
  Eliom_services.service
    ~path:["tclose2"]
    ~get_params:Eliom_parameters.unit
    ()



(* ------------------------------------------------------------- *)
(* Handler for the "tsession_services_example" service:           *)
(* It displays the main page of our site, with a login form.     *)

let tsession_services_example_handler () () =
  let f =
    My_appl.post_form
      tsession_services_example_with_post_params
      (fun login ->
        [p [pcdata "login: ";
            string_input ~input_type:`Text ~name:login ()]]) ()
  in
  return [f]



(* ------------------------------------------------------------- *)
(* Handler for the "tsession_services_example_close" service:     *)

let tsession_services_example_close_handler () () =
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
  Lwt.return [p [pcdata "You have been disconnected. ";
                 a tsession_services_example
                   [pcdata "Retry"] ()
                ]]


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_with_post_params"   *)
(* service:                                                      *)

let tlaunch_session () login =

  (* New handler for the main page: *)
  let new_main_page () () =
    return
      [p [pcdata "Welcome ";
          pcdata login;
          pcdata "!"; br ();
          a eliomclient1 [pcdata "coucou"] (); br ();
          a tsession_services_example_close
            [pcdata "close session"] ()]]
  in

  (* If a session was opened, we close it first! *)
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->

  (* Now we register new versions of main services in the
     session service table: *)
  My_appl.register (*zap* *) ~state_name (* *zap*)
    ~scope:`Client_process
    ~service:tsession_services_example
    (* service is any public service already registered,
       here the main page of our site *)
    new_main_page;

  My_appl.register (*zap* *) ~state_name (* *zap*)
    ~scope:`Client_process
    ~service:eliomclient1
    (fun () () ->
      return
        [p [pcdata "Coucou ";
            pcdata login;
            pcdata "!"]]);

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
  Eliom_services.service
    ~path:["tcoserv"]
    ~get_params:Eliom_parameters.unit
    ()

let tcoservices_example_post =
  Eliom_services.post_coservice
    ~fallback:tcoservices_example
    ~post_params:Eliom_parameters.unit
    ()

let tcoservices_example_get =
  Eliom_services.coservice
    ~fallback:tcoservices_example
    ~get_params:Eliom_parameters.unit
    ()

(* -------------------------------------------------------- *)
(* The three of them display the same page,                 *)
(* but the coservices change the counter.                   *)

let _ =
  let c = ref 0 in
  let page () () =
    let l3 = My_appl.post_form tcoservices_example_post
        (fun _ -> [p [My_appl.string_input
                        ~input_type:`Submit
                        ~value:"incr i (post)" ()]]) ()
    in
    let l4 = My_appl.get_form tcoservices_example_get
        (fun _ -> [p [My_appl.string_input
                        ~input_type:`Submit
                        ~value:"incr i (get)" ()]])
    in
    return
      [p [pcdata "The random number in the container must not change!"; br ();
          pcdata "i is equal to ";
          pcdata (string_of_int !c); br ();
          a tcoservices_example [pcdata "internal application link to myself"] (); br ();
          a tcoservices_example_get [pcdata "incr i"] ()];
       l3;
       l4]
  in
  My_appl.register tcoservices_example page;
  let f () () = c := !c + 1; page () () in
  My_appl.register tcoservices_example_post f;
  My_appl.register tcoservices_example_get f




(************************************************************)
(*************** calc: sum of two integers ******************)
(************************************************************)

(*zap* *)
let state_name = "calc_example"

(* *zap*)
(* -------------------------------------------------------- *)
(* We create two main services on the same URL,             *)
(* one with a GET integer parameter:                        *)

let tcalc =
  service
    ~path:["tcalc"]
    ~get_params:unit
    ()


let tcalc_i =
  service
    ~path:["tcalc"]
    ~get_params:(int "i")
    ()



(* -------------------------------------------------------- *)
(* The handler for the service without parameter.           *)
(* It displays a form where you can write an integer value: *)

let tcalc_handler () () =
  let create_form intname =
    [p [pcdata "Write a number: ";
        My_appl.int_input ~input_type:`Text ~name:intname ();
        br ();
        My_appl.string_input ~input_type:`Submit ~value:"Send" ()]]
  in
  let f = My_appl.get_form tcalc_i create_form in
  return [f]



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
      ~scope:`Client_process
      ~fallback:tcalc
      ~get_params:(int "j")
      (fun j () ->
        let js = string_of_int j in
        let ijs = string_of_int (i+j) in
        return [p [pcdata (is^" + "^js^" = "^ijs)]])
  in
  let f = get_form tcalc_result (create_form is) in
  return [f]



(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register tcalc   tcalc_handler;
  My_appl.register tcalc_i tcalc_i_handler



(************************************************************)
(************ Connection of users, version 3 ****************)
(************************************************************)

(*zap* *)
let state_name = "connect_example3"

(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let tconnect_example3 =
  Eliom_services.service
    ~path:["taction"]
    ~get_params:Eliom_parameters.unit
    ()

let tconnect_action =
  Eliom_services.post_coservice'
    ~name:"tconnect3"
    ~post_params:(Eliom_parameters.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let tdisconnect_action =
  Eliom_output.Action.register_post_coservice'
    ~name:"tdisconnect3"
    ~post_params:Eliom_parameters.unit
    (fun () () ->
      Eliom_state.discard (*zap* *) ~state_name (* *zap*) ~scope:`Client_process ())

(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let tdisconnect_box s =
  My_appl.post_form tdisconnect_action
    (fun _ -> [p [My_appl.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let tlogin_box () =
  My_appl.post_form tconnect_action
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   My_appl.string_input
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
    (match sessdat with
      | Eliom_state.Data name ->
        [p [pcdata ("Hello "^name); br ()];
         tdisconnect_box "Close session"]
      | Eliom_state.Data_session_expired
      | Eliom_state.No_data -> [tlogin_box ()]
    )



(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let tconnect_action_handler () login =
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
  Eliom_state.set_volatile_data ~table:my_table login;
  return ()



(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  My_appl.register ~service:tconnect_example3 tconnect_example3_handler;
  Eliom_output.Action.register ~service:tconnect_action tconnect_action_handler




(************************************************************)
(************ Connection of users, version 4 ****************)
(**************** (persistent sessions) *********************)
(************************************************************)

(*zap* *)
let state_name = "persistent_sessions"

(* *zap*)
let tmy_persistent_table =
  Eliom_state.create_persistent_table ~scope:`Client_process (*zap* *) ~state_name (* *zap*) "teliom_example_table"


(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let tpersist_session_example =
  Eliom_services.service
    ~path:["tpersist"]
    ~get_params:unit
    ()


let tpersist_session_connect_action =
  Eliom_services.post_coservice'
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
  Eliom_output.Action.register_post_coservice'
    ~name:"tdisconnect4"
    ~post_params:Eliom_parameters.unit
    (fun () () ->
      Eliom_state.discard ~state_name ~scope:`Client_process  ())


let tdisconnect_box s =
  My_appl.post_form tdisconnect_action
    (fun _ -> [p [My_appl.string_input
                    ~input_type:`Submit ~value:s ()]]) ()


let bad_user_key = Polytables.make_key ()
let get_bad_user table = 
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false


(* -------------------------------------------------------- *)
(* new login box:                                           *)

let tlogin_box session_expired action =
  My_appl.post_form action
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
      Eliom_services.coservice
        ~fallback:tpersist_session_example ~get_params:unit ~timeout:5. ()
    in
    let _ =
      Eliom_output.Xhtml5.register ~service:timeoutcoserv
        ~scope:`Client_process
        (fun _ _ ->
          return
             (html
               (head (title (pcdata "Cooooooooservices with timeouts")) [])
               (body [p
                 [pcdata "I am a coservice with timeout."; br ();
                  pcdata "Try to reload the page!"; br ();
                  pcdata "I will disappear after 5 seconds of inactivity.";
                  pcdata "Pour l'instant c'est un Eliom_output.Xhtml5 au lieu de My_appl parce qu'il y a un bug à corriger dans ce cas. Remettre My_appl ici et ajouter un test pour ce bug." ];
                 ])))
(*            [p [pcdata "I am a coservice with timeout."; br ();
                a timeoutcoserv [pcdata "Try again"] (); br ();
                pcdata "I will disappear after 5 seconds of inactivity." ];
            ])
*)
    in
  Eliom_state.get_persistent_data ~table:tmy_persistent_table () >>= fun sessdat ->
  Lwt.return
    (match sessdat with
      | Eliom_state.Data name ->
        [p [pcdata ("Hello "^name); br ()];
         tdisconnect_box "Close session"]
      | Eliom_state.Data_session_expired ->
        [tlogin_box true tpersist_session_connect_action;
         p [em [pcdata "The only user is 'toto'."]]]
      | Eliom_state.No_data ->
        [tlogin_box false tpersist_session_connect_action;
         p [em [pcdata "The only user is 'toto'."]]]
    )



(* ----------------------------------------------------------- *)
(* Handler for persist_session_connect_action (user logs in):  *)

let tpersist_session_connect_action_handler () login =
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
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
  Eliom_output.Action.register
    ~service:tpersist_session_connect_action
    tpersist_session_connect_action_handler




(*
(************************************************************)
(************ Connection of users, version 6 ****************)
(************************************************************)
(*zap* *)
let state_name = "connect_example6"

(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let tconnect_example6 =
  Eliom_services.service
    ~path:["taction2"]
    ~get_params:unit
    ()


let tconnect_action =
  Eliom_services.post_coservice'
    ~name:"tconnect6"
    ~post_params:(string "login")
    ()


(* new disconnect action and box:                           *)

let tdisconnect_action =
  Eliom_output.Action.register_post_coservice'
    ~name:"tdisconnect6"
    ~post_params:Eliom_parameters.unit
    (fun () () ->
      Eliom_state.discard (*zap* *) ~state_name (* *zap*) ~scope:`Client_process  ())


let tdisconnect_box s =
  My_appl.post_form tdisconnect_action
    (fun _ -> [p [My_appl.string_input
                    ~input_type:`Submit ~value:s ()]]) ()



let bad_user_key = Polytables.make_key ()

let get_bad_user table = 
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false


(* -------------------------------------------------------- *)
(* new login box:                                           *)

let tlogin_box session_expired action =
  My_appl.post_form action
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
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
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
  Eliom_output.Action.register ~service:tconnect_action tconnect_action_handler

*)







let tcsrfsafe_example =
  Eliom_services.service
    ~path:["tcsrf"]
    ~get_params:Eliom_parameters.unit
    ()


let tcsrfsafe_example_post =
  Eliom_services.post_coservice
    ~csrf_safe:true
    ~csrf_state_name:"csrf"
    ~csrf_scope:`Client_process
    ~csrf_secure:true
    ~timeout:10.
    ~max_use:1
    ~https:true
    ~fallback:tcsrfsafe_example
    ~post_params:Eliom_parameters.unit
    ()


let _ =
  let page () () =
    let l3 = My_appl.post_form tcsrfsafe_example_post
        (fun _ -> [p [My_appl.string_input
                         ~input_type:`Submit
                         ~value:"Click" ()]]) ()
    in
    Lwt.return 
      [p [pcdata "A new coservice will be created each time this form is displayed"];
       l3]
  in
  My_appl.register tcsrfsafe_example page;
  My_appl.register tcsrfsafe_example_post
    (fun () () ->
      Lwt.return [p [pcdata "This is a CSRF safe service"]])



(***** User cookies *****)
let cookiename = "mycookie"


let tcookies = service ["tcookies"] unit ()


let _ = My_appl.register tcookies
  (fun () () ->
    Eliom_state.set_cookie
      ~cookie_scope:`Client_process
      ~name:cookiename ~value:(string_of_int (Random.int 100)) ();
    Lwt.return
      [p [pcdata (try
                    "cookie value: "^
                      (Ocsigen_lib.String_Table.find
                         cookiename
                         (Eliom_request_info.get_cookies
                            ~cookie_scope:`Client_process ()))
        with _ -> "<cookie not set>");
          br ();
          a tcookies [pcdata "send other cookie"] ()]])






(***** Action outside the application: 
       will ask the client program to do a redirection *****)

let coucouaction =
  Eliom_output.Action.register_coservice
    ~fallback:Eliom_testsuite1.coucou
    ~get_params:unit
    (fun () () -> Lwt.return ())


let actionoutside =
  My_appl.register_service
    ~path:["actionoutside"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        [p [a ~service:coucouaction
               [ pcdata "Click to do an action outside the application"] () ];
        ])



(*****************************************************************************)
(* persistent references *)

let persref = service ["persref"] unit ()

let _ = 
  let next =
    let pr =
      Eliom_references.eref
        ~scope:`Global ~persistent:"__eliom_example_persref" 0
    in
    let mutex = Lwt_mutex.create () in
    fun () ->
      Lwt_mutex.lock mutex >>= fun () ->
      Eliom_references.get pr >>= fun v ->
      let v = v+1 in
      Eliom_references.set pr v >>= fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return v
  in
  Eliom_output.Xhtml5compact.register persref
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
let ttimeout = service ["ttimeout"] unit ()

let _ =
  let page () () =
    let timeoutcoserv =
      Eliom_services.coservice
        ~fallback:ttimeout ~get_params:unit ~timeout:5. ()
    in
    let _ =
      My_appl.register ~service:timeoutcoserv
        ~scope:`Client_process
        (fun _ _ ->
          Lwt.return
            [p [pcdata "I am a coservice with timeout."; br ();
                a timeoutcoserv [pcdata "Try again"] (); br ();
                pcdata "I will disappear after 5 seconds of inactivity." ];
            ])
    in
    return
      [h2 [pcdata "Client process coservices with timeouts"];
       p [pcdata "I just created a coservice with 5 seconds timeout."; br ();
          a timeoutcoserv [pcdata "Try it"] (); ];
      ]
  in
  My_appl.register ttimeout page



(*****************************************************************************)
let nonapplprocessservice = service ["nonapplprocessservice"] unit ()

let _ =
  let page () () =
    let serv =
      Eliom_output.Caml.register_post_coservice'
        ~scope:`Client_process
        ~post_params:unit
        (fun () () -> Lwt.return [1; 2; 3])
    in
    let serv2 =
      Eliom_output.Xhtml5.register_coservice'
        ~scope:`Client_process
        ~get_params:unit
        (fun () () -> Lwt.return (html
                                    (head (title (pcdata "mmmh")) [])
                                    (body [p [pcdata "It works"]])))
    in
    Lwt.return
      [h2 [pcdata "Client process service not registered with My_appl"];
       p [pcdata "I just created two coservices with scope `Client_process but not registered with My_appl."; br ();
          span ~a:[a_class ["clickable"];
                   a_onclick
                     {{let body = Dom_html.document##body in
                       Eliom_client.call_caml_service ~service:%serv () () >|=
                       List.iter
                         (fun i -> Dom.appendChild body
                           (Dom_html.document##createTextNode
                              (Js.string (string_of_int i))))
                      }}
                  ]
            [pcdata "Click to call it and receive Ocaml data (service registered with Eliom_output.Caml)."];
          br ();
          pcdata "It works, because we send tab cookies with Eliom_client.call_service or Eliom_client.call_caml_service.";
          br ();
          My_appl.a ~service:serv2 [pcdata "Here a link to an client process service outside the application."] ();
          pcdata " For now it does not work, because we do not send tab cookies for non My_appl services ... How to solve this?";
          br ();
          pcdata "Add a test of link to another application."
         ]
      ]
  in
  My_appl.register nonapplprocessservice page



(*****************************************************************************)
(* Session + My_appl *)
let state_name = "session_appl"

let connect_example789 =
  Eliom_services.service ~path:["session_appl"] ~get_params:unit ()

let connect_action789 =
  Eliom_services.post_coservice'
    ~name:"connection789"
    ~post_params:(string "login")
    ()

let disconnect_action789 =
  Eliom_output.Action.register_post_coservice'
    ~name:"disconnection789"
    ~post_params:Eliom_parameters.unit
    (fun () () -> Eliom_state.close_session (*~state_name*) ())

let disconnect_box s =
  My_appl.post_form disconnect_action789
    (fun _ -> [p [My_appl.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let bad_user = Eliom_references.eref (*~state_name*) ~scope:`Request false

let user = Eliom_references.eref (*~state_name*) ~scope:`Session None

let login_box session_expired bad_u action =
  My_appl.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         My_appl.string_input ~input_type:`Text ~name:loginname ()]
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
  let status = Eliom_state.volatile_data_state_status (*~state_name*) () in
  Eliom_references.get bad_user >>= fun bad_u ->
  Eliom_references.get user >>= fun u ->
  Lwt.return
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
    )

let connect_action_handler () login =
  Eliom_state.close_session (*~state_name*) () >>= fun () ->
  if login = "toto"
  then Eliom_references.set user (Some login)
  else Eliom_references.set bad_user true

let () =
  My_appl.register ~service:connect_example789 connect_example_handler;
  Eliom_output.Action.register ~service:connect_action789 connect_action_handler

(*****************************************************************************)
(* Form towards internal suffix service *)
let isuffixc =
  My_appl.register_service
    ~path:["isuffixc"]
    ~get_params:(suffix_prod (int "suff" ** all_suffix "endsuff") (int "i"))
    (fun ((suff, endsuff), i) () ->
      Lwt.return
        [p [pcdata "The suffix of the url is ";
            strong [pcdata (string_of_int suff)];
            pcdata " followed by ";
            strong [pcdata (Ocsigen_lib.string_of_url_path ~encode:false endsuff)];
            pcdata " and i is equal to ";
            strong [pcdata (string_of_int i)]]])

let create_suffixformc ((suff, endsuff),i) =
    [h3 [pcdata "Form to an (internal appl) suffix service"];
     p [pcdata "Write an int for the suffix:";
        int_input ~input_type:`Text ~name:suff ();
        br ();
        pcdata "Write a string: ";
        user_type_input
          (Ocsigen_lib.string_of_url_path ~encode:false)
          ~input_type:`Text ~name:endsuff ();
        br ();
        pcdata "Write an int: ";
        int_input ~input_type:`Text ~name:i ();
        br ();
        string_input ~input_type:`Submit ~value:"Click" ()
       ]
    ]


(*****************************************************************************)
(* Redirections and Eliom applications: *)
let appl_redir1 =
  Eliom_output.Redirection.register_service
    ~path:["internalredir"]
    ~get_params:Eliom_parameters.unit
    (fun () () -> Lwt.return eliomclient2)

let appl_redir2 =
  Eliom_output.Redirection.register_service
    ~path:["externalredir"]
    ~get_params:Eliom_parameters.unit
    (fun () () -> Lwt.return Eliom_testsuite1.coucou)

let appl_redir =
  My_appl.register_service
    ~path:["applredir"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        [p [
          a ~service:appl_redir1 [ pcdata "Link to a redirection inside the Eliom application"] ();
          br ();
          a ~service:appl_redir2 [ pcdata "Link to a redirection outside the Eliom application"] ();
         ];
         My_appl.get_form ~service:appl_redir1
            (fun () ->
              [Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"Form to a redirection inside the Eliom application" ()]
            );
         My_appl.get_form ~service:appl_redir2
            (fun () ->
              [Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"Form to a redirection outside the Eliom application" ()]
            )
        ])


(*****************************************************************************)
(* Void coservices with Eliom applications: *)
let applvoid_redir =
  Eliom_output.Redirection.register_post_coservice'
    ~name:"applvoidcoserv"
    ~post_params:Eliom_parameters.unit
    (fun () () -> Lwt.return Eliom_services.void_hidden_coservice')


(*****************************************************************************)
(* Form examples: *)
let postformc =
  My_appl.register_post_service
    ~fallback:Eliom_testsuite1.coucou
    ~post_params:(Eliom_parameters.string "zzz")
    (fun () s -> Lwt.return [p [pcdata "Yo man. ";
                                pcdata s]])

let formc = My_appl.register_service ["formc"] unit
  (fun () () -> 
    Lwt.return
      [
        My_appl.get_form ~service:Eliom_testsuite1.coucou
          (fun () ->
            [Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"GET form to a service outside the Eliom application" ()]
          );
        
        My_appl.post_form ~service:Eliom_testsuite1.my_service_with_post_params
          (fun s ->
            [Eliom_output.Xhtml5.string_input ~input_type:`Hidden ~name:s ~value:"plop" ();
             Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"POST form to a service outside the Eliom application" ()]
          )
          ();
        
        My_appl.get_form ~service:eliomclient1
          (fun () ->
            [Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"GET form to a service inside the Eliom application" ()]
          );
        
        My_appl.post_form ~service:postformc
          (fun s ->
            [Eliom_output.Xhtml5.string_input ~input_type:`Submit ~name:s ~value:"POST form to a service inside the Eliom application" ()]
          )
          ();
        
        
        My_appl.get_form isuffixc create_suffixformc;

        h3 [pcdata "POST form towards action with void service redirection"];
        p [pcdata "Random value in the content: ";
           pcdata (string_of_int (Random.int 1000))];
        My_appl.post_form ~service:applvoid_redir
          (fun () ->
            [Eliom_output.Xhtml5.string_input ~input_type:`Submit ~value:"Click to send POST form to myself." ()]
          )
          ();
        p [pcdata "This must not stop the application (same random number in the container but not in the content)."];

      ])

