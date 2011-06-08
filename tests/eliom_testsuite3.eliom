(*zap* *)
{shared{
  open Ocsigen_cookies
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
open HTML5.M
}}

(****** server only *******)
{server{ (* note that {server{ ... }} is optionnal. *)
open Eliom_parameters
open Eliom_output.Html5
open Eliom_services
}}

(* This is server only because there are no delimiters. *)
module My_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "eliom_testsuite"
    end)
(*wiki* Now I can define my first service belonging to that application: *wiki*)

(* FIXME GRGR ... avoid fake_header and "unique" in the first example ! *)
let fake_header = unique (p [])
let header () =
  let p = p [pcdata "Random value in the container: ";
	     pcdata (string_of_int (Random.int 1000)); br ();
	     a ~service:main [pcdata "Back to the main page of the test suite."] ();] in
  unique ~copy:fake_header p

let make_page ?(css = []) content =
  html
    (head
       (title (pcdata "Eliom application example"))
       ([ style
           [pcdata "a,.clickable {color: #111188; cursor: pointer;}"];
          My_appl.application_script ();
	] @ css))
    (body
       [h1 [pcdata "Eliom application"];
	header ();
        div content ] )

{client{
  module My_appl = Eliom_output.Html5
}}

{server{ (* note that {server{ ... }} is optionnal. *)
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
             a_onclick {{ Dom_html.window##alert(Js.string "clicked!") }}
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
                {{Eliom_client.exit_to
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
            [pcdata "Click here to change the URL with change_url."];

(*wiki*
  The following examples shows how to change the current page,
  without stopping the client side program.
*wiki*)
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick
                {{ignore(Eliom_client.change_page
			   ~service:%eliomclient1
			   () ())
                }}
            ]
            [pcdata "Click here to change the page without stopping the program (with change_page)."];

          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick{{
                ignore (Eliom_client.change_page ~service:%Eliom_testsuite1.coucou
			  () ())
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
            [pcdata "Click here to relaunch the program by reloading the page (with exit_to)."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
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
                ignore (Eliom_client.get_subpage ~service:%eliomclient1
			  () () >|= fun blocks ->
			    List.iter
			      (Dom.appendChild Dom_html.document##body)
			      blocks)
              }}
            ]
            [pcdata "Click here to get a subpage from server."];


(*wiki*
====Refering to parts of the page in client side code
*wiki*)

          (let container = unique (ul [ item () ; item () ; item ()]) in
           div [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                               a_onclick {{
                                 Dom.appendChild
                                   (Eliom_client.Html5.of_uList %container) (* node is the wrapper keyword for HTML5.M nodes. *)
                                   (Eliom_client.Html5.of_li (item ()))
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
                let coucou = %Eliom_testsuite1.coucou in
                let eliomclient1 = %eliomclient1 in
                (Dom.appendChild
                   (Dom_html.document##body)
                   (Eliom_client.Html5.of_paragraph
                      (p [Eliom_output.Html5.a
                            ~service:coucou
                            [pcdata "An external link generated client side"]
                            ();
                          pcdata ", ";
                          Eliom_output.Html5.a
                            (*zap* *)~a:[a_class ["clickable"]](* *zap*)
                            ~service:eliomclient1
                            [pcdata "another, inside the application, "]
                            ();
                          pcdata " and ";
                          span
                            ~a:[a_class ["clickable"];
                                a_onclick (XML.event_of_function (fun () -> Dom_html.window##alert(Js.string "clicked!")))]
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
To do that, use the {{{Eliom_parameters.caml}}} function:
*wiki*)
{shared{
  type ec3 = (int * string * string list) deriving (Json)
}}

let eliomclient3' =
  My_appl.register_post_coservice'
    ~post_params:(caml "isb" Json.t<ec3>)
    (fun () (i, s, l) ->
      Lwt.return
        (make_page
	   [p (pcdata (Printf.sprintf "i = %d, s = %s" i s)::
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
             {{ ignore (Eliom_client.change_page
			  ~service:%eliomclient3'
			  () (299, "oo", ["a";"b";"c"]))
              }}
            ]
               [pcdata "Click to send Ocaml data"]
           ]))

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
        (make_page
	   [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
                 {{let body = Dom_html.document##body in
                   ignore (Eliom_client.call_caml_service
                     ~service:%eliomclient4'
                     () () >|=
                   List.iter
                     (fun i -> Dom.appendChild body
                                 (Dom_html.document##createTextNode
                                    (Js.string (string_of_int i)))))
                 }}
              ]
           [pcdata "Click to receive Ocaml data"]
        ]))

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
    ~options:{Eliom_output.do_not_launch = true}
    ~service:withoutclient
    (fun () () ->
       Lwt.return
	 (make_page
         [p [pcdata "If the application was not launched before coming here (or if you reload), this page will not launch it. But if it was launched before, it is still running."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
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





let on_load =
  My_appl.register_service
    ~path:["onload"]
    ~get_params:unit
    (fun () () ->
      let div =
	  unique (div [p [a ~service:eliomclient1 [pcdata "go to another page"] ()] ])
      in
      Eliom_services.onload
        {{ ignore
	     (Lwt_js.sleep 1. >|= fun () ->
               Dom.appendChild (Eliom_client.Html5.of_div %div)
		 (Eliom_client.Html5.of_paragraph (p [pcdata "on_load executed after 1s."])))
         }};
      Eliom_services.onload
        {{ ignore
	     (Lwt_js.sleep 3. >|= fun () ->
               Dom.appendChild  (Eliom_client.Html5.of_div %div)
		 (Eliom_client.Html5.of_paragraph (p [pcdata "on_load executed after 3s."])))
         }};
      Eliom_services.onunload
        {{
	  Dom.appendChild (Eliom_client.Html5.of_div %div)
             (Eliom_client.Html5.of_paragraph (p [pcdata "on_unload executed. Waiting 1s."]));
	  (* FIXME GRGR *)
          ignore(Lwt_js.sleep 1.)
        }};
      Lwt.return (make_page [div])
    )

let uri_test =
  My_appl.register_service
    ~path:["uritest"]
    ~get_params:unit
    (fun () () ->
      let div =
        unique
	  (div [
            p [pcdata "The following URLs are computed either on server or client side. They should be equal."];
            p [pcdata (Eliom_uri.make_string_uri ~service:eliomclient1 ())];
          ])
      in
      Eliom_services.onload
        {{ Dom.appendChild (Eliom_client.Html5.of_div %div)
             (Eliom_client.Html5.of_paragraph
		(p [pcdata (Eliom_uri.make_string_uri ~service:%eliomclient1 ())]))
         }};
      Lwt.return (make_page [div])
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
  let put n f =
    Printf.ksprintf (fun s ->
      Dom.appendChild (Eliom_client.Html5.of_element n)
        (Eliom_client.Html5.of_paragraph (p [pcdata s]))) f
}}

let global_div = unique (div [pcdata "global div"])
let other_global_div = unique (div [pcdata "other global div"])

let wrapping1 = Eliom_services.service
    ~path:["wrapping1"]
    ~get_params:unit
    ()

let gc_service =
  Eliom_output.Redirection.register_service
    ~path:["gc_wrapping1"]
    ~get_params:Eliom_parameters.unit
    (fun () () -> Gc.full_major (); Lwt.return wrapping1)

let () =
  My_appl.register wrapping1
    (fun () () ->
      let div = unique (div [pcdata "some page contents"]) in

      Eliom_services.onload
	{{
	  let v = %v1 in
	  put %div "42=%i 42.42=%f fourty two=%s" v.Wrapping_test.v_int v.Wrapping_test.v_float v.Wrapping_test.v_string;
	  ( match %rec_list with
	    | a::b::c::d::e::f::g::_ ->
	      put %div "%i::%i::%i::%i::%i::%i::%i::..." a b c d e f g;
	    | _ -> put %div "problem with recursive list"; );

          Dom.appendChild (Eliom_client.Html5.of_div %div)
            (Eliom_client.Html5.of_paragraph
	       (p ~a:[ a_onclick
			 (XML.event_of_function
			 (fun _ ->
			   ignore (Eliom_client.get_subpage ~service:v.Wrapping_test.v_service
				     () () >|= (fun blocks ->
				       List.iter
					 (Dom.appendChild (Eliom_client.Html5.of_div %div))
					 blocks;))))]
		  [pcdata "test service"]));

	  let f_react = fst (List.hd %rec_list_react) in

          Dom.appendChild (Eliom_client.Html5.of_div %div)
            (Eliom_client.Html5.of_paragraph
	       (p ~a:[ a_onclick (XML.event_of_function (fun _ -> ignore (f_react 42)))] [pcdata "test react service: event 42 should appear on stdout (of the server) when this is clicked "]));

	}};
      Lwt.return
	(make_page [
	global_div; br ();
	div; br ();
	p ~a:[
	  a_onclick
	    ({{
	      Dom.appendChild (Eliom_client.Html5.of_div %global_div)
	        (Eliom_client.Html5.of_div %other_global_div);
	      Dom.appendChild (Eliom_client.Html5.of_div %other_global_div)
		(Eliom_client.Html5.of_div %div);
	    }})
	] [pcdata "click here"]; br ();
	p [Eliom_output.Html5.a ~service:eliomclient1
              [pcdata "Link to a service inside the application."]
              ()];
	Eliom_output.Html5.a wrapping1 [pcdata "internal application link to myself"] (); br ();
	Eliom_output.Html5.a gc_service [pcdata "do a full major gc on the server"] (); br ();
	(* GRGR FIXME *)
	(* pcdata (Printf.sprintf "client_process_node_table_size: %i" (Eliom_xml.client_process_node_table_size ())); *)
      ]))

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
         (make_page [
           div
             [pcdata "To fully understand the meaning of the public channel, \
                      use a couple browsers on this page."] ;
         ])
    )


let caml_wrapping_service =
  Eliom_output.Caml.register_post_coservice'
    ~post_params:(Eliom_parameters.unit)
    (fun () () -> Lwt.return
      (Eliom_comet.Channels.create (Lwt_stream.clone stream1)))

let caml_service_wrapping =
  My_appl.register_service
    ~path:["caml_service_wrapping"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        [
          div ~a:[a_onclick {{
	    lwt c = Eliom_client.call_caml_service ~service:%caml_wrapping_service () () in
	    Lwt_stream.iter_s
              (fun i ->
		Dom.appendChild (Dom_html.document##body)
		  (Dom_html.document##createTextNode
                     (Js.string ("message: "^ string_of_int i ^";  "))) ;
		Lwt.return ()
              ) c
	    }}]
	    [pcdata "click"];
	  pcdata "when clicking on this link, messages should be received every 1 second";
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
          ignore (React.E.map
            (fun s -> Dom_html.window##alert (Js.string s))
            %e_down)
        }};

      (* We can send the page *)
      Lwt.return (make_page [
         h2 [pcdata "Dual events"] ;
         div (* This div is for pushing "A" to the server side event *)
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ ignore ( %e_up "A") }} ]
           [pcdata "Push A"] ;
         div (* This one is for pushing "B" *)
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ ignore ( %e_up "B") }} ]
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
           ignore (React.E.map
           (fun s -> Dom_html.window##alert (Js.string s))
           (React.E.merge
              (^) ""
              [ React.E.map string_of_int %e_down_1 ;
                %e_down_2 ;
              ]
           ))
         }};

       (* We can send the page *)
       Lwt.return (make_page [
         h2 [pcdata "Simultaneous events"] ;
         div
           ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick {{ ignore ( %e_up "") }} ]
           [pcdata "Send me two values from different events !"] ;
       ])
    )

let comet_wrapping =
  My_appl.register_service
    ~path:["comet_wrapping"]
    ~get_params:unit
    (fun () () ->
      let node = div [pcdata "node created on server side"] in
      let service_stream,push_service = Lwt_stream.create () in
      push_service (Some Eliom_testsuite1.coucou);
      let c_service = Eliom_comet.Channels.create service_stream in
      let xml_stream,push_xml = Lwt_stream.create () in
      push_xml (Some (div [pcdata "basic xml wrapping";node]));
      push_xml (Some
		  (div [Eliom_output.Html5.a ~service:Eliom_testsuite1.coucou
                     [pcdata "xml external link wrapping"] ()]));
      push_xml (Some
		  (div [Eliom_output.Html5.a ~service:comet1
			   [pcdata "xml internal link wrapping"] ();
			pcdata "this link must not stop the process! (same random number in the container)."]));
      let c_xml = Eliom_comet.Channels.create xml_stream in
      let div_link = div [] in

      Eliom_services.onload
        {{
	  ignore (Lwt_stream.iter
          (fun service ->
            Dom.appendChild (Eliom_client.Html5.of_element %div_link)
              (Eliom_client.Html5.of_element
		 ( Eliom_output.Html5.a ~service
                     [pcdata "service wrapping"] ()))
          ) %c_service);
	  ignore (Lwt_stream.iter
		    (fun xml ->
		      Dom.appendChild (Eliom_client.Html5.of_element %div_link)
			(Eliom_client.Html5.of_element xml)
		    ) %c_xml)
        }};
      
      Lwt.return
        (make_page [
          div [pcdata "there should be a working links below"];
	  node;
	  div_link
        ])
    )

let time =
  let t = Unix.gettimeofday () in
  let e = Lwt_react.E.from (fun () -> Lwt_unix.sleep 0.1 >>= (fun () -> Lwt.return (Unix.gettimeofday ()))) in
  Eliom_react.S.Down.of_react (Lwt_react.S.hold t e)

let comet_signal =
  My_appl.register_service
    ~path:["comet_signal"]
    ~get_params:unit
    (fun () () ->
      let time_div = div [] in
      Eliom_services.onload
        {{
          Lwt_react.S.keep
	  (React.S.map (fun t -> (Eliom_client.Html5.of_div %time_div)##innerHTML <-
	    Js.string (string_of_float t)) %time)
        }};
       Lwt.return (make_page [
         h2 [pcdata "Signal"] ;
         time_div
       ])
    )

(*wiki*
 Here is the code for a minimalistic message board.
 *wiki*)

let message_bus = Eliom_bus.create ~size:10 Json.t<string>
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
             let _ = 
	       Lwt.catch (fun () ->
		 Lwt_stream.iter_s
		   (fun msg ->
                     Dom.appendChild (Eliom_client.Html5.of_element %container)
                       (Eliom_client.Html5.of_li (li [pcdata msg]));
                     Lwt.return ())
		   (Eliom_bus.stream %message_bus))
		 (function
		   | Eliom_comet.Channel_full ->
		     Dom.appendChild (Eliom_client.Html5.of_element %container)
                       (Eliom_client.Html5.of_li (li [pcdata "channel full, no more messages"]));
		     Lwt.return ()
		   | e -> Lwt.fail e);
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
                    ignore (Eliom_bus.write %message_bus v)
                  }}
             ]
             [pcdata "send"]
         in

         (make_page [ h2 [pcdata "Message board"];
           form ~a:[a_action (uri_of_string "")] (div [field; go]) [];
           container;
         ]))
    )

(*wiki*
===Header manipulation with eliom client

 *wiki*)

let service_style1 =
  Eliom_services.service
    ~path:["css"; "style1"]
    ~get_params:unit
    ()
let service_style2 =
  Eliom_services.service
    ~path:["css"; "style2"]
    ~get_params:unit
    ()
let service_no_style =
  Eliom_services.service
    ~path:["css"; "no_style"]
    ~get_params:unit
    ()

let page_css_test () =
  [Eliom_output.Html5.a ~service:service_style1
      [pcdata "same page with style 1"] (); br ();
   Eliom_output.Html5.a ~service:service_style2
     [pcdata "same page with style 2"] (); br ();
   Eliom_output.Html5.a ~service:service_no_style
     [pcdata "same page with no style"] (); br ();
   div ~a:[a_class ["some_class"];] [pcdata "div with style"]]

let make_css_link file =
  Eliom_output.Html5.css_link
    (Eliom_output.Html5.make_uri
       ~service:(Eliom_services.static_dir ()) [file]) ()

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

      let make_target s = HTML5.M.p [HTML5.M.a [HTML5.M.pcdata s]] in
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

      let targetresult = HTML5.M.p [] in
      Eliom_services.onload
        {{
          let targetresult = (Eliom_client.Html5.of_paragraph %targetresult) in

          let handler =
            lwt_arr
              (fun ev ->
                ignore (targetresult##appendChild
                          ((Eliom_client.Html5.of_element (HTML5.M.pcdata " plip") :> Dom.node Js.t)));
                Lwt.return ())
          in
          let handler_long =
            lwt_arr
              (fun ev ->
                Lwt_js.sleep 0.7 >>= fun () ->
                ignore (targetresult##appendChild
                          ((Eliom_client.Html5.of_element (HTML5.M.pcdata " plop") :> Dom.node Js.t)));
                Lwt.return ()
              )
          in
          let cancel c = arr (fun _ -> cancel c) in
          let c = run (click (Eliom_client.Html5.of_paragraph %target1) >>> handler) () in
          let _ = run (click (Eliom_client.Html5.of_paragraph %target2) >>> cancel c) () in
          let _ = run (mousedown (Eliom_client.Html5.of_paragraph %target3) >>> mouseup (Eliom_client.Html5.of_paragraph %target2) >>> handler) () in
          let c = run (clicks (Eliom_client.Html5.of_paragraph %target4) handler_long) () in
          let _ = run (click (Eliom_client.Html5.of_paragraph %target5) >>> cancel c) () in
          let _ = run (click (Eliom_client.Html5.of_paragraph %target6) >>> handler >>> click (Eliom_client.Html5.of_paragraph %target6) >>> handler) () in
          let _ = run (clicks (Eliom_client.Html5.of_paragraph %target7) (click (Eliom_client.Html5.of_paragraph %target7) >>> handler)) () in
          let _ = run (click (Eliom_client.Html5.of_paragraph %target8) >>> clicks (Eliom_client.Html5.of_paragraph %target8) handler) () in
          let c = run (first [click (Eliom_client.Html5.of_paragraph %target9) >>> handler;
                              click (Eliom_client.Html5.of_paragraph %target10) >>> handler]) ()
          in
          let _ = run (click (Eliom_client.Html5.of_paragraph %target11) >>> cancel c) ()
          in
          let c = run (mousedowns (Eliom_client.Html5.of_paragraph %target12)
                         (first [mouseup Dom_html.document;
                                 mousemoves Dom_html.document handler])) ()
          in
          let _ = run (click (Eliom_client.Html5.of_paragraph %target13) >>> cancel c) ()
          in
          let c = run (mousedowns (Eliom_client.Html5.of_paragraph %target14)
                         (first [mouseup Dom_html.document;
                                 mousemoves Dom_html.document handler_long])) ()
          in
          let _ = run (click (Eliom_client.Html5.of_paragraph %target15) >>> cancel c) ()
          in
          ()

        }};

       Lwt.return
	 (make_page [target1; target2; target3; target4; target5; target6;
                     target7; target8; target9; target10; target11;
                     target12; target13; target14; target15;
                     targetresult]) )



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
    (make_page [
      match sessdat with
        | Eliom_state.Data name ->
          p [pcdata ("Hello "^name);
             br ();
             Eliom_output.Html5.a
               tsession_data_example_close
               [pcdata "close session"] ()]
        | Eliom_state.Data_session_expired
        | Eliom_state.No_data ->
          Eliom_output.Html5.post_form
            tsession_data_example_with_post_params
            (fun login ->
              [p [pcdata "login: ";
                  Eliom_output.Html5.string_input
                    ~input_type:`Text ~name:login ()]]) ()
    ])


(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_with_post_params"  *)
(* service with POST params:                                *)

let tsession_data_example_with_post_params_handler _ login =
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
  Eliom_state.set_volatile_data
    ~table:my_table login;
  return
    (make_page [p [pcdata ("Welcome " ^ login ^ ". You are now connected.");
        br ();
        Eliom_output.Html5.a tsession_data_example [pcdata "Try again"] ()
       ]])




(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_close" service:    *)

let tsession_data_example_close_handler () () =
  let sessdat = Eliom_state.get_volatile_data
    ~table:my_table () in
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
  return
    (make_page [
      (match sessdat with
        | Eliom_state.Data_session_expired -> p [pcdata "Your session has expired."]
        | Eliom_state.No_data -> p [pcdata "You were not connected."]
        | Eliom_state.Data _ -> p [pcdata "You have been disconnected."]);
      p [Eliom_output.Html5.a tsession_data_example [pcdata "Retry"] () ]])



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
    Eliom_output.Html5.post_form
      tsession_services_example_with_post_params
      (fun login ->
        [p [pcdata "login: ";
            string_input ~input_type:`Text ~name:login ()]]) ()
  in
  return (make_page [f])



(* ------------------------------------------------------------- *)
(* Handler for the "tsession_services_example_close" service:     *)

let tsession_services_example_close_handler () () =
  Eliom_state.discard
 (*zap* *) ~state_name (* *zap*) ~scope:`Client_process () >>= fun () ->
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
    let l3 = Eliom_output.Html5.post_form tcoservices_example_post
        (fun _ -> [p [Eliom_output.Html5.string_input
                        ~input_type:`Submit
                        ~value:"incr i (post)" ()]]) ()
    in
    let l4 = Eliom_output.Html5.get_form tcoservices_example_get
        (fun _ -> [p [Eliom_output.Html5.string_input
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
        Eliom_output.Html5.int_input ~input_type:`Text ~name:intname ();
        br ();
        Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Send" ()]]
  in
  let f = Eliom_output.Html5.get_form tcalc_i create_form in
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
      ~scope:`Client_process
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
  Eliom_output.Html5.post_form tdisconnect_action
    (fun _ -> [p [Eliom_output.Html5.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let tlogin_box () =
  Eliom_output.Html5.post_form tconnect_action
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Eliom_output.Html5.string_input
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
         tdisconnect_box "Close session"]
      | Eliom_state.Data_session_expired
      | Eliom_state.No_data -> [tlogin_box ()]
    ))



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
  Eliom_output.Html5.post_form tdisconnect_action
    (fun _ -> [p [Eliom_output.Html5.string_input
                    ~input_type:`Submit ~value:s ()]]) ()


let bad_user_key = Polytables.make_key ()
let get_bad_user table = 
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false


(* -------------------------------------------------------- *)
(* new login box:                                           *)

let tlogin_box session_expired action =
  Eliom_output.Html5.post_form action
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
      Eliom_output.Html5.register ~service:timeoutcoserv
        ~scope:`Client_process
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
  Eliom_output.Html5.post_form tdisconnect_action
    (fun _ -> [p [Eliom_output.Html5.string_input
                    ~input_type:`Submit ~value:s ()]]) ()



let bad_user_key = Polytables.make_key ()

let get_bad_user table = 
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false


(* -------------------------------------------------------- *)
(* new login box:                                           *)

let tlogin_box session_expired action =
  Eliom_output.Html5.post_form action
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
    let l3 = Eliom_output.Html5.post_form tcsrfsafe_example_post
        (fun _ -> [p [Eliom_output.Html5.string_input
                         ~input_type:`Submit
                         ~value:"Click" ()]]) ()
    in
    Lwt.return
      (make_page
	 [p [pcdata "A new coservice will be created each time this form is displayed"];
	  l3])
  in
  My_appl.register tcsrfsafe_example page;
  My_appl.register tcsrfsafe_example_post
    (fun () () ->
      Lwt.return (make_page [p [pcdata "This is a CSRF safe service"]]))



(***** User cookies *****)
let cookiename = "mycookie"


let tcookies = service ["tcookies"] unit ()


let _ = My_appl.register tcookies
  (fun () () ->
    Eliom_state.set_cookie
      ~cookie_scope:`Client_process
      ~name:cookiename ~value:(string_of_int (Random.int 100)) ();
    Lwt.return
      (make_page [p [pcdata (try
                    "cookie value: "^
                      (CookiesTable.find
                         cookiename
                         (Eliom_request_info.get_cookies
                            ~cookie_scope:`Client_process ()))
        with _ -> "<cookie not set>");
          br ();
          a tcookies [pcdata "send other cookie"] ()]]))






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
        (make_page
	   [p [a ~service:coucouaction
		  [ pcdata "Click to do an action outside the application"] () ];
           ]))



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
  Eliom_output.Html5.register persref
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
      Eliom_output.Html5.register_coservice'
        ~scope:`Client_process
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
                     {{let body = Dom_html.document##body in
                       ignore (Eliom_client.call_caml_service ~service:%serv () () >|=
                       List.iter
                         (fun i -> Dom.appendChild body
                           (Dom_html.document##createTextNode
                              (Js.string (string_of_int i)))))
                      }}
                  ]
            [pcdata "Click to call it and receive Ocaml data (service registered with Eliom_output.Caml)."];
          br ();
          pcdata "It works, because we send tab cookies with Eliom_client.call_service or Eliom_client.call_caml_service.";
          br ();
          Eliom_output.Html5.a ~service:serv2 [pcdata "Here a link to an client process service outside the application."] ();
          pcdata " For now it does not work, because we do not send tab cookies for non My_appl services ... How to solve this?";
          br ();
          pcdata "Add a test of link to another application."
         ]
      ])
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
  Eliom_output.Html5.post_form disconnect_action789
    (fun _ -> [p [Eliom_output.Html5.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let bad_user = Eliom_references.eref (*~state_name*) ~scope:`Request false

let user = Eliom_references.eref (*~state_name*) ~scope:`Session None

let login_box session_expired bad_u action =
  Eliom_output.Html5.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Eliom_output.Html5.string_input ~input_type:`Text ~name:loginname ()]
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
     Eliom_output.Html5.int_input ~input_type:`Text ~name:suff ();
     pcdata "Write a string: ";
     Eliom_output.Html5.string_input ~input_type:`Text ~name:endsuff ();
     pcdata "Write an int: ";
     Eliom_output.Html5.int_input ~input_type:`Text ~name:i ();
     Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Click" ()
    ]
}}

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
       (make_page  [p [
          a ~service:appl_redir1 [ pcdata "Link to a redirection inside the Eliom application"] ();
          br ();
          a ~service:appl_redir2 [ pcdata "Link to a redirection outside the Eliom application"] ();
         ];
         Eliom_output.Html5.get_form ~service:appl_redir1
            (fun () ->
              [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Form to a redirection inside the Eliom application" ()]
            );
         Eliom_output.Html5.get_form ~service:appl_redir2
            (fun () ->
              [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Form to a redirection outside the Eliom application" ()]
            )
        ]))


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
    (fun () s -> Lwt.return (make_page [p [pcdata "Yo man. ";
					   pcdata s]]))

module Another_appl =
  Eliom_output.Eliom_appl (
    struct
      let application_name = "eliom_testsuite3bis"
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
  module Another_appl = Eliom_output.Html5
}}

let otherappl =
  Another_appl.register_service
    ~path:["other"; "appl"]
    ~get_params:unit
    (fun () () -> Lwt.return (make_page_bis [p [pcdata "I am another application"] ]))


let formc = My_appl.register_service ["formc"] unit
  (fun () () -> 
    let div = unique (div [h3 [pcdata "Forms and links created on client side:"]]) in
    Eliom_services.onload
      {{ 

        let l =
          [
            h4 [pcdata "to outside the application:"];

            p [Eliom_output.Html5.a ~service:%Eliom_testsuite1.coucou
                 [pcdata "Link to a service outside the application."]
                 ()];
           
           Eliom_output.Html5.get_form ~service:%Eliom_testsuite1.coucou
             (fun () ->
               [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"GET form to a service outside the Eliom application" ()]
             );
           
           Eliom_output.Html5.post_form ~service:%Eliom_testsuite1.my_service_with_post_params
             (fun s ->
               [Eliom_output.Html5.string_input ~input_type:`Hidden ~name:s ~value:"plop" ();
                Eliom_output.Html5.string_input ~input_type:`Submit ~value:"POST form to a service outside the Eliom application" ()]
             )
             ();

            p [Eliom_output.Html5.a ~service:%otherappl
                  [pcdata "Link to another application."]
                  ();
              ];
           
            Eliom_output.Html5.get_form ~service:%otherappl
              (fun () ->
                [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"GET form to another application" ();];
              );
           
           
           h4 [pcdata "inside the application — must not stop the process! (same random number in the container)."];

           p [Eliom_output.Html5.a ~service:%eliomclient1
                 [pcdata "Link to a service inside the application."]
                 ()];
           
           Eliom_output.Html5.get_form ~service:%eliomclient1
             (fun () ->
               [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"GET form to a service inside the Eliom application" ()]
             );
           
           Eliom_output.Html5.post_form ~service:%postformc
             (fun s ->
               [Eliom_output.Html5.string_input ~input_type:`Submit ~name:s ~value:"POST form to a service inside the Eliom application" ()]
             )
             ();

           Eliom_output.Html5.get_form %isuffixc create_suffixformc;

           Eliom_output.Html5.post_form ~service:%applvoid_redir
             (fun () ->
               [pcdata "POST form towards action with void service redirection. This must not stop the application (same random number in the container but not in the here: ";
                pcdata (string_of_int (Random.int 1000));
                pcdata ") ";
                Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Click to send POST form to myself." ()]
             )
             ();

          ]
        in
        List.iter
	  (fun e -> Dom.appendChild
	    (Eliom_client.Html5.of_div %div)
	    (Eliom_client.Html5.of_element e)) l
       }};

    Lwt.return
      (make_page [

        h3 [pcdata "Forms and links created on server side:"];

        h4 [pcdata "to outside the application:"];

        p [Eliom_output.Html5.a ~service:Eliom_testsuite1.coucou
              [pcdata "Link to a service outside the application."]
              ()];

        Eliom_output.Html5.get_form ~service:Eliom_testsuite1.coucou
          (fun () ->
            [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"GET form to a service outside the Eliom application" ()]
          );
        
        Eliom_output.Html5.post_form ~service:Eliom_testsuite1.my_service_with_post_params
          (fun s ->
            [Eliom_output.Html5.string_input ~input_type:`Hidden ~name:s ~value:"plop" ();
             Eliom_output.Html5.string_input ~input_type:`Submit ~value:"POST form to a service outside the Eliom application" ()]
          )
          ();

        p [Eliom_output.Html5.a ~service:otherappl
              [pcdata "Link to another application."]
              ();
           pcdata " (The other appl won't work as it is not compiled)"];
        
        Eliom_output.Html5.get_form ~service:otherappl
          (fun () ->
            [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"GET form to another application" ();]
          );
           
        
        h4 [pcdata "inside the application — must not stop the process! (same random number in the container)."];

        p [Eliom_output.Html5.a ~service:eliomclient1
              [pcdata "Link to a service inside the application."]
              ()];

        Eliom_output.Html5.get_form ~service:eliomclient1
          (fun () ->
            [Eliom_output.Html5.string_input ~input_type:`Submit ~value:"GET form to a service inside the Eliom application" ()]
          );
        
        Eliom_output.Html5.post_form ~service:postformc
          (fun s ->
            [Eliom_output.Html5.string_input ~input_type:`Submit ~name:s ~value:"POST form to a service inside the Eliom application" ()]
          )
          ();
        
        Eliom_output.Html5.get_form isuffixc create_suffixformc;

        Eliom_output.Html5.post_form ~service:applvoid_redir
          (fun () ->
            [pcdata "POST form towards action with void service redirection. This must not stop the application (same random number in the container but not in the here: ";
             pcdata (string_of_int (Random.int 1000));
             pcdata ") ";
             Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Click to send POST form to myself." ()]
          )
          ();
        
        div;

      ]))

(*****************************************************************************)
(* Any with Eliom applications: *)

let any_service =
  Eliom_services.service
    ~path:["appl_any"]
    ~get_params:(Eliom_parameters.int "with_eliom_appl")
    ()

let any_service_fallback =
  My_appl.register_service
    ~path:["appl_any_"]
    ~get_params:Eliom_parameters.unit
    (fun () () -> Lwt.return (make_page [p [pcdata "any_appl_ fallback"; br (); pcdata "it should never be called"]]))

let any_service_post =
  Eliom_services.post_service
    ~fallback:any_service_fallback
    ~post_params:(Eliom_parameters.int "with_eliom_appl")
    ()

let () =
  let make_content name =
    let sp = Eliom_common.get_sp () in
    let appl_name = sp.Eliom_common.sp_client_appl_name in
    let links f =
      span [f "version generated by Eliom_output.Html5" 0;
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
	 links (fun text i -> Eliom_output.Html5.a ~service:any_service [pcdata text] i);
	 br ();
	 links (fun text i -> HTML5.M.a ~a:[a_onclick {{
	   ignore (Eliom_client.change_page ~service:( %any_service_post ) () %i) }}]
	   [pcdata text]);
	 br ();
	 pcdata "There are no post parameters so the request is cached: the first time it is correctly done,
    then the cache respond instead. Maybe add a non localised parameter to have different requests ?
    This works with FF, but not with chrome, althougth chrome have a normal beahviour.";
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
      Eliom_output.appl_self_redirect Eliom_output.Html5.send
	(html
	   (head (title (pcdata "html5 content")) [])
	   (body (make_content "Eliom_output.Html5.send")))
    | 1 ->
      Printf.printf "my appl case\n%!";
      My_appl.send (make_page (make_content "My_appl.send"))
    | 2 ->
      Printf.printf "another appl case\n%!";
      Eliom_output.appl_self_redirect Another_appl.send
	(make_page (make_content "Another_appl.send"))
    | _ ->
      Printf.printf "Files case\n%!";
      Eliom_output.appl_self_redirect Eliom_output.Files.send "/var/www/ocsigen/tutorial/ocsigen5.png"
  in
  Eliom_output.Any.register ~service:any_service
    (fun choice () -> make_any choice);
  Eliom_output.Any.register  ~service:any_service_post
    (fun () choice -> make_any choice)


(*****************************************************************************)
(* XHR form with files: *)

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

let block_form_fallback = Eliom_output.Blocks5.register_service
  ~path:["resultblocks"]
  ~get_params:unit
  (fun () () -> return [pcdata "nothing"])

let block_form_result = Eliom_output.Blocks5.register_post_service
  ~post_params:((((((bool "case" ** (radio int "radio"))
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
    let form = post_form block_form_result make_xhr_form () in
    let subpage = div [] in
    let launch = p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
      a_onclick {{
	let uri = Eliom_uri.make_string_uri ~service:%block_form_result () in
	ignore (Eliom_request.send_post_form (Eliom_client.Html5.of_form %form) uri >|=
	    (fun contents -> ( Eliom_client.Html5.of_div %subpage )##innerHTML <- (Js.string contents)))
      }}]
      [pcdata "send form with an xhr"]
    in
    Lwt.return
      (make_page
	 [
	   pcdata "this test need upload: add <uploaddir>/tmp/upload</uploaddir> to the configuration file";
	   form; launch; subpage]))
