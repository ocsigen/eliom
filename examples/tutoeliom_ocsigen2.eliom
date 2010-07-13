(*zap* *)
{shared{
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
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
Eliom allows to write the client and server parts of a Web application
fully in Objectice Caml.
Running OCaml in the client's browser is acheived by compiling OCaml bytecode
into Javascript. Check the [[wiki(30):/|js_of_ocaml]] project for news and
information.


===@@id="p4basics"@@

====Your first client-server application
        %<div class="onecol"|

First, I need to create my Eliom application, by applying the functor
{{{Eliom_predefmod.Eliom_appl}}}. You can define here what will be
the default title for pages belonging to this application, the
default container for pages, the default stylesheets you want for your
whole application.

*wiki*)
(****** open on both side *******)
{shared{
open XHTML.M
}}
(****** server only *******)
{server{ (* note that {server{ ... }} is optionnal. *)
open Eliom_parameters
open Eliom_predefmod.Xhtmlcompact
open Eliom_services
}}

(* for client side only, one can use : {client{ ... }} *)

(* This is server only because there are no delimiters. *)
module Eliom_appl =
  Eliom_predefmod.Eliom_appl (
    struct
      let application_name = "tutoeliom_ocsigen2_client"
      let params =
        {Eliom_predefmod.default_appl_params with
           Eliom_predefmod.ap_title = "Eliom application example";
           Eliom_predefmod.ap_headers =
            [XHTML.M.style ~contenttype:"text/css"
               [XHTML.M.pcdata ".clickable {color: #111188; cursor: pointer;}"]];
           Eliom_predefmod.ap_container =
            Some (None,
                  fun div -> [h1 [pcdata "Eliom application"];
                              p [pcdata "Random value in the container: ";
                                 pcdata (string_of_int (Random.int 1000))];
                              div ])
        }
    end)
(*wiki* Now I can define my first service belonging to that application: *wiki*)

let eliomclient1 =
  Eliom_appl.register_new_service
    ~path:["eliomclient1"]
    ~get_params:unit
    (fun sp () () ->
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
let eliomclient2 = new_service ~path:["eliomclient2"] ~get_params:unit ()

let myblockservice =
  Eliom_predefmod.Blocks.register_new_post_coservice
    ~fallback:eliomclient2
    ~post_params:unit
    (fun _ () () ->
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
  Eliom_appl.register
    eliomclient2
    (fun sp () () ->
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
                    ~sp:\sp(sp) (* Here [sp] is sent by the server *)
                    ~service:\magic(Tutoeliom.coucou) (* just as [coucou] *)
                    () ()
                }}
            ]
            [pcdata "Click here to go to another page."];

(*wiki*
To use server values inside client code one should use the syntax {{{ \k(e) }}}
where {{{k}}} is the wrapper keyword and {{{e}}} the sent expression. Note that
{{{e}}} is evaluated by the server and the resulting value is send to the
client at loading time.
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
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (myblockservice : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.http) Eliom_services.service) ->
                      let sp = Eliommod_client.unwrap_sp sp in
                      let body = Dom_html.document##body in
                      (*Js_old.get_element_by_id "bodyid"*)
                      Eliom_client.call_service
                        ~sp ~service:myblockservice () () >>= fun s ->
                      (try
                         let l = Js_old.Node.children (Js_old.dom_of_xml s) in
                         List.iter (Js_old.Node.append body) l
                       with e -> Js_old.alert (Printexc.to_string e));
(* does not work with chrome. A solution is probably to use set "innerHTML". *)
                       Lwt.return ()
                 ) (Eliom_client.wrap_sp sp) myblockservice)
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
                  ~sp:\sp(sp)
                  ~service:\magic(Tutoeliom.coucou)
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
                    ~sp:\sp(sp)
                    ~service:\magic(eliomclient1)
                    () ()
                }}
            ]
            [pcdata "Click here to change the page without stopping the program."];

(*wiki* Actually the usual {{{a}}} function to create link will
  use {{{change_page}}} if you do a link inside the same application.
  The latter example is equivalent to the following. *wiki*)
          p [a (*zap* *) ~a:[a_class ["clickable"]](* *zap*)
               ~sp
               ~service:eliomclient1
               [pcdata "Click here to change the page without stopping the program (with ";
                code [pcdata "a"];
                pcdata ")."]
               ()];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick{{
                Eliom_client.change_page ~sp:\sp(sp) ~service:\magic(Tutoeliom.coucou)
                  () ()
              }}
            ]
            [pcdata "Click here to go to a page outside the application, using ";
             code [pcdata "change_page"];
             pcdata "."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.exit_to ~sp:\sp(sp) ~service:\magic(eliomclient2) () ()
              }}
            ]
            [pcdata "Click here to relaunch the program by reloading the page."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_page ~sp:\sp(sp) ~service:\magic(eliomclient1)
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
                Eliom_client.get_subpage ~sp:\sp(sp) ~service:\magic(eliomclient1)
                  () () >|= fun blocks ->
                List.iter
                  (Dom.appendChild Dom_html.document##body)
                  (XHTML.M.toeltl blocks)
              }}
            ]
            [pcdata "Click here to get a subpage from server."];


(*wiki*
====Refering to parts of the page in client side code
*wiki*)

          (let container = ul (item ()) [ item () ; item ()] in
           div [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                               a_onclick {{
                                 Dom.appendChild
                                   \node(container) (* node is the wrapper keyword for XHTML.M nodes. *)
                                   (XHTML.M.toelt (item ()))
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
                     (Js.string (string_of_float \(my_value:float))) ;
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
                (Dom.appendChild
                   (Dom_html.document##body)
                   (XHTML.M.toelt
                      (p [Eliom_predefmod.Xhtml.a
                            ~sp:\sp(sp) ~service:\magic(Tutoeliom.coucou)
                            [pcdata "An external link generated client side"]
                            ();
                          pcdata " and ";
                          Eliom_predefmod.Xhtml.a
                            (*zap* *)~a:[a_class ["clickable"]](* *zap*)
                            ~sp:\sp(sp) ~service:\magic(eliomclient1)
                            [pcdata "another, inside the application."]
                            ()
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
let eliomclient3' =
  Eliom_appl.register_new_post_coservice'
    ~post_params:(caml "isb")
    (fun sp () (i, s, l) ->
      Lwt.return
        [p (pcdata (Printf.sprintf "i = %d, s = %s" i s)::
              List.map (fun a -> pcdata a) l
           )])



let eliomclient3 =
  Eliom_appl.register_new_service
    ~path:["eliomclient3"]
    ~get_params:unit
    (fun sp () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
                {{ Eliom_client.change_page
                     ~sp:\sp(sp) ~service:\magic(eliomclient3')
                     () (22, "oo", ["a";"b";"c"])
                }}
              ]
           [pcdata "Click to send Ocaml data"]
        ])
(*wiki*
====Sending OCaml values using services
It is possible to do services that send any caml value. For example:
*wiki*)
let eliomclient4' =
  Eliom_predefmod.Caml.register_new_post_coservice'
    ~post_params:unit
    (fun sp () () -> Lwt.return [1; 2; 3])

let eliomclient4 =
  Eliom_appl.register_new_service
    ~path:["eliomclient4"]
    ~get_params:unit
    (fun sp () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick
                 {{let body = Dom_html.document##body in
                   Eliom_client.call_caml_service
                     ~sp:\sp(sp) ~service:\magic(eliomclient4')
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
  Eliom_services.new_service
    ~path:["withoutclient"]
    ~get_params:unit
    ()

let gotowithoutclient =
  Eliom_services.new_service
    ~path:["gotowithoutclient"]
    ~get_params:unit
    ()


let _ =
  Eliom_appl.register
    ~options:true
    ~service:withoutclient
    (fun sp () () ->
       Lwt.return
         [p [pcdata "If the application was not launched before coming here (or if you reload), this page will not launch it. But if it was launched before, it is still running."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_page
                  ~sp:\sp(sp) ~service:\magic(gotowithoutclient)
                  () ()
              }}
            ]
            [pcdata "Click here to go to a page that launches the application every time (this link does not work if the appl is not launched)."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~sp ~service:gotowithoutclient
               [pcdata "Same link with ";
                code [pcdata "a"]; pcdata "."] ()];
         ]);
  Eliom_appl.register
    ~service:gotowithoutclient
    (fun sp () () ->
       Lwt.return
         [p [pcdata "The application is launched."];
          p
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_page
                  ~sp:\sp(sp) ~service:\magic(withoutclient)
                  () ()
              }}
            ]
            [pcdata "Click here to see the page that does not launch the application."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~sp ~service:withoutclient
               [pcdata "Same link with ";
                code [pcdata "a"]; pcdata "."] ()];
         ])



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

(* create a communication channel. Because it is public, we give an explicit
* name for it. *)
let (c1, write_c1) =
  let (e, push_e) = React.E.create () in
  (Eliom_comet.Channels.create ~name:"comet1_public_channel" e, push_e)

(* randomly write on the channel *)
let rec rand_tick () =
  Lwt_unix.sleep (float_of_int (5 + (Random.int 5))) >>= fun () ->
  write_c1 (Random.int 99) ; rand_tick ()
let _ = rand_tick ()


let comet1 =
  Eliom_appl.register_new_service
    ~path:["comet1"]
    ~get_params:unit
    (fun sp () () ->
       let (c2_pre, write_c2) = React.E.create () in
       let c2 = Eliom_comet.Dlisted_channels.create
                  ~max_size:6
                  ~timer:16.
                  c2_pre
       in
       let t2 = ref 0 in
       let rec tick_2 () =
         Lwt_unix.sleep (float_of_int (6 + (Random.int 6))) >>= fun () ->
         write_c2 !t2 ; incr t2 ; Lwt.pause () >>= fun () ->
         write_c2 !t2 ; incr t2 ; write_c2 !t2 ; incr t2 ; tick_2 ()
       in
       let t = tick_2 () in
       let `R _ = React.E.retain c2_pre (fun () -> ignore t; ignore c2) in
       Lwt.return
         [
           div
             [pcdata "To fully understand the meaning of the public channel, \
                      use a couple browsers on this page."] ;
           div
             ~a:[a_onclick{{
                   Eliom_client_comet.Channels.register \channel(c1)
                     (fun i ->
                        Dom.appendChild (Dom_html.document##body)
                          (Dom_html.document##createTextNode
                             (Js.string ("public: "^ string_of_int i ^";  "))) ;
                        Lwt.return ()
                     )
                }} ]
             [pcdata "Click here to start public channel listening"] ;
           div
             ~a:[a_onclick{{
                   Eliom_client_comet.Dlisted_channels.register \buffchan(c2)
                     (fun i ->
                        Dom.appendChild (Dom_html.document##body)
                          (Dom_html.document##createTextNode
                             (Js.string ("private: "^ string_of_int i ^"; "))) ;
                        Lwt.return ()
                     )
                }} ]
             [pcdata "Click here to start private buffered channel listening"] ;
         ]
    )

(*wiki*
This second example involves client-to-server and server to client event
propagation. There is no manual handling of channel, only events are used.
 *wiki*)


let comet2 =
  Eliom_appl.register_new_service
    ~path:["comet2"]
    ~get_params:unit
    (fun sp () () ->
       (* First create a server-readable client-writable event AKA up event AKA
          client-to-server asynchronous edge *)
       let e_up = Eliom_event.Up.create ~sp (string "letter") in
       let e_up_real = Eliom_event.Up.react_event_of_up_event e_up in
       let e_down = React.E.map
                      (function "A" -> "alpha" | "B" -> "beta" | _ -> "what ?")
                      e_up_real
       in
       let `R _ = React.E.retain e_up_real (fun () -> ignore e_down) in

       (* We can send the page *)
       Lwt.return [
         h2 [pcdata "Dual events"] ;
         div (* There's a start "button" right now, but it's gonna change *)
           ~a:[a_onclick {{
                React.E.map
                  (fun s -> Dom_html.window##alert (Js.string s))
                  \down_event(e_down)
           }}
              ]
           [pcdata "START"] ;
         div (* This div is for pushing "A" to the server side event *)
           (*TODO: fix client side sp and simplify up_event unwrapping *)
           ~a:[a_onclick {{ let sp = \sp(sp) in \up_event(e_up) "A" }} ]
           [pcdata "Push A"] ;
         div (* This one is for pushing "B" *)
           ~a:[a_onclick {{ let sp = \sp(sp) in \up_event(e_up) "B" }} ]
           [pcdata "Push B"] ;
       ]
    )

(*wiki*
 This third example demonstrates the capacity for simultaneous server push.
 *wiki*)


let comet3 =
  Eliom_appl.register_new_service
    ~path:["comet3"]
    ~get_params:unit
    (fun sp () () ->
       (* First create a server-readable client-writable event AKA up event AKA
          client-to-server asynchronous edge *)
       let e_up = Eliom_event.Up.create ~sp (string "double") in
       let e_up_real = Eliom_event.Up.react_event_of_up_event e_up in
       let e_down_1 =
         Lwt_event.limit (*TODO: integrate throttling to events*)
           (fun () -> Lwt_unix.sleep 3.)
           (React.E.map
              (let i = ref 0 in fun _ -> incr i ; !i)
              e_up_real
           )
       in
       let e_down_2 = React.E.map (fun _ -> "haha") e_up_real in
       let `R _ = React.E.retain e_up_real
                    (fun () -> ignore e_down_1 ; ignore e_down_2)
       in

       (* We can send the page *)
       Lwt.return [
         h2 [pcdata "Simultaneous events"] ;
         div (* There's a start "button" right now, but it's gonna change *)
           ~a:[a_onclick {{
                React.E.map
                  (fun s -> Dom_html.window##alert (Js.string s))
                  (React.E.merge
                     (^) ""
                     [ React.E.map string_of_int \down_event(e_down_1) ;
                       \down_event(e_down_2) ;
                     ]
                  )
           }}
              ]
           [pcdata "START"] ;
         div (*TODO: fix client side sp and simplify up_event unwrapping *)
           ~a:[
             (*zap* *)a_class ["clickable"];(* *zap*)
             a_onclick {{ let sp = \sp(sp) in \up_event(e_up) "" }} ]
           [pcdata "Send me two values from different events !"] ;
         div [pcdata "Note that one of the two events has a greater rate limit \
                      (using throttle control). Hence you might receive only \
                      one if you click with high frequency."] ;
       ]
    )


(*wiki*
 Here is the code for a small minimalist message board.
 *wiki*)

(* First is the event on the server corresponding to a new message. *)
let message_up = Eliom_event.Up.create (string "content")

(* Then is the page hosting the board *)
let comet_message_board =
  Eliom_appl.register_new_service
    ~path:["message_board"]
    ~get_params:unit
    (fun sp () () ->
       let message_down =
         React.E.map (fun x -> x)
         (Eliom_event.Up.react_event_of_up_event message_up)
       in

       Lwt.return (
         let container = ul (li [pcdata "This is the message board"]) [] in
         let field = input ~a:[a_id "msg"; a_input_type `Text; a_name "message"] () in
         let go =
           div
             ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                  a_onclick {{
                    let sp = \sp(sp) in
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
                    \up_event(message_up) v
                  }}
             ]
             [pcdata "send"]
         in

         [ h2 [pcdata "Message board"];
           div
             ~a:[ (*zap* *)a_class ["clickable"];(* *zap*)
                  a_onclick {{
                    ignore (
                      React.E.map
                        (fun msg ->
                            Dom.appendChild \node(container)
                              (XHTML.M.toelt (li [pcdata msg]))
                        )
                        \down_event(message_down)
                    ) ;
                    Eliom_client_comet.Engine.start ()
                  }}
                ]
             [pcdata "Go online"];
           div
             ~a:[ (*zap* *)a_class ["clickable"];(* *zap*)
                  a_onclick {{ Eliom_client_comet.Engine.stop () }}
                ]
             [pcdata "Go offline"];
           form (uri_of_string "") (div [field; go]) [];
           container;
         ])
    )

(*wiki*

===Tab sessions

  *wiki*)


open Lwt

(************************************************************)
(************ Connection of users, version 1 ****************)
(************************************************************)

(*zap* *)
let session_name = "tsession_data"
(* *zap*)

(* "my_table" will be the structure used to store
   the session data (namely the login name): *)

let my_table = Eliom_sessions.create_volatile_table ()


(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let tsession_data_example =
  Eliom_services.new_service
    ~path:["tsessdata"]
    ~get_params:Eliom_parameters.unit
    ()

let tsession_data_example_with_post_params =
  Eliom_services.new_post_service
    ~fallback:tsession_data_example
    ~post_params:(Eliom_parameters.string "login")
    ()

let tsession_data_example_close =
  Eliom_services.new_service
    ~path:["tclose"]
    ~get_params:Eliom_parameters.unit
    ()



(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example" service:          *)

let tsession_data_example_handler sp _ _  =
  let sessdat = 
    Eliom_sessions.get_volatile_session_data
 (*zap* *) ~session_name (* *zap*) ~cookie_type:Eliom_common.CTab ~table:my_table ~sp () in
  return
    [
      match sessdat with
        | Eliom_sessions.Data name ->
          p [pcdata ("Hello "^name);
             br ();
             Eliom_predefmod.Xhtml.a
               tsession_data_example_close
               sp [pcdata "close session"] ()]
        | Eliom_sessions.Data_session_expired
        | Eliom_sessions.No_data ->
          Eliom_predefmod.Xhtml.post_form
            tsession_data_example_with_post_params
            sp
            (fun login ->
              [p [pcdata "login: ";
                  Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Text ~name:login ()]]) ()
    ]

(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_with_post_params"  *)
(* service with POST params:                                *)

let tsession_data_example_with_post_params_handler sp _ login =
  Eliom_sessions.close_session
 (*zap* *) ~session_name (* *zap*) ~cookie_type:Eliom_common.CTab ~sp () >>= fun () ->
  Eliom_sessions.set_volatile_session_data
 (*zap* *) ~session_name (* *zap*) ~cookie_type:Eliom_common.CTab ~table:my_table ~sp login;
  return
    [p [pcdata ("Welcome " ^ login ^ ". You are now connected.");
        br ();
        Eliom_predefmod.Xhtml.a tsession_data_example sp [pcdata "Try again"] ()
       ]]



(* -------------------------------------------------------- *)
(* Handler for the "tsession_data_example_close" service:    *)

let tsession_data_example_close_handler sp () () =
  let sessdat = Eliom_sessions.get_volatile_session_data
 (*zap* *) ~session_name (* *zap*) ~cookie_type:Eliom_common.CTab ~table:my_table ~sp () in
  Eliom_sessions.close_session
 (*zap* *) ~session_name (* *zap*) ~cookie_type:Eliom_common.CTab ~sp () >>= fun () ->
  return
    [
      (match sessdat with
        | Eliom_sessions.Data_session_expired -> p [pcdata "Your session has expired."]
        | Eliom_sessions.No_data -> p [pcdata "You were not connected."]
        | Eliom_sessions.Data _ -> p [pcdata "You have been disconnected."]);
      p [Eliom_predefmod.Xhtml.a tsession_data_example sp [pcdata "Retry"] () ]]


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_appl.register
    tsession_data_example_close tsession_data_example_close_handler;
  Eliom_appl.register
    tsession_data_example tsession_data_example_handler;
  Eliom_appl.register
    tsession_data_example_with_post_params
    tsession_data_example_with_post_params_handler


(*$$$$$$$$$$$$$$$$$$$$$$$$$

(************************************************************)
(************ Connection of users, version 2 ****************)
(************************************************************)

(*zap* *)
let session_name = "session_services"
(* *zap*)
(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let session_services_example =
  Eliom_services.new_service
    ~path:["sessionservices"]
    ~get_params:Eliom_parameters.unit
    ()

let session_services_example_with_post_params =
  Eliom_services.new_post_service
    ~fallback:session_services_example
    ~post_params:(Eliom_parameters.string "login")
    ()

let session_services_example_close =
  Eliom_services.new_service
    ~path:["close2"]
    ~get_params:Eliom_parameters.unit
    ()


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example" service:           *)
(* It displays the main page of our site, with a login form.     *)

let session_services_example_handler sp () () =
  let f =
    Eliom_predefmod.Xhtml.post_form
      session_services_example_with_post_params
      sp
      (fun login ->
        [p [pcdata "login: ";
            string_input ~input_type:`Text ~name:login ()]]) ()
  in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_close" service:     *)

let session_services_example_close_handler sp () () =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  return
    (html
       (head (title (pcdata "Disconnect")) [])
       (body [p [pcdata "You have been disconnected. ";
                 a session_services_example
                   sp [pcdata "Retry"] ()
               ]]))

(*wiki*

          
When the page is called with login parameters,
       it runs the function %<span class="code"|launch_session>%
       that replaces some services already defined by new ones:
    
          

*wiki*)
(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_with_post_params"   *)
(* service:                                                      *)

let launch_session sp () login =

  (* New handler for the main page: *)
  let new_main_page sp () () =
    return
      (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Welcome ";
                 pcdata login;
                 pcdata "!"; br ();
                 a coucou sp [pcdata "coucou"] (); br ();
                 a hello sp [pcdata "hello"] (); br ();
                 a links sp [pcdata "links"] (); br ();
                 a session_services_example_close
                   sp [pcdata "close session"] ()]]))
  in

  (* If a session was opened, we close it first! *)
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->

  (* Now we register new versions of main services in the
     session service table: *)
  Eliom_predefmod.Xhtml.register_for_session (*zap* *) ~session_name (* *zap*)
    ~sp
    ~service:session_services_example
    (* service is any public service already registered,
       here the main page of our site *)
    new_main_page;

  Eliom_predefmod.Xhtml.register_for_session (*zap* *) ~session_name (* *zap*)
    ~sp
    ~service:coucou
    (fun _ () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Coucou ";
                   pcdata login;
                   pcdata "!"]])));

  Eliom_predefmod.Xhtml.register_for_session (*zap* *) ~session_name (* *zap*)
    ~sp
    ~service:hello
    (fun _ () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Ciao ";
                   pcdata login;
                   pcdata "!"]])));

  new_main_page sp () ()

(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register
    ~service:session_services_example
    session_services_example_handler;
  Eliom_predefmod.Xhtml.register
    ~service:session_services_example_close
    session_services_example_close_handler;
  Eliom_predefmod.Xhtml.register
    ~service:session_services_example_with_post_params
    launch_session
(*zap* Registering for session during initialisation is forbidden:
let _ = register_for_session
    ~path:coucou1
    %< <html>
         <head><title></title></head>
         <body><h1>humhum</h1></body>
       </html> >%
*zap*)
(*wiki*

          
[[site:tuto/sessionservices| See the result]].
          

          
Warning: As in the previous example,
       to implement such connection and disconnection forms, you
       get more flexibility by using //actions// instead of xhtml services
       (see below for the same example with actions).
      
          

          
Services registered in session tables are called
       //session// or //private// services.
       Services registered in the public table
       are called //public//.
      
          
    
        >%
        ===@@id="p2coservices"@@Coservices
        
        %<div class="onecol"|
          
   A coservice is a service that uses the same URL as
   a main service, but generates another page.
   They are distinguished from main services only by a special
   parameter, called //state// parameter.
   Coservices may use GET or POST parameters.
          

          
Most of the time, GET coservices are created dynamically with
   respect to previous interaction with the user and are registered
   in the session table. They allow to give a precise semantics to the
   "back" button of the browser (be sure that you will go back in the
   past) or bookmarks, or duplication of the browser's window.
   (See the [[manual/dev/2#p2calc|%<span class="code"|calc>%]] example below).
   
          

          
   Use POST coservices if you want to particularize a link or form,
   but not the URL it points to.
   More precisely, POST coservices are mainly used in two situations:
    
          

          
*For the same purpose as GET coservices (new services
   corresponding to precise points of the interaction with the user)
   but when you don't want this service to be bookmarkable.
*To create a button that leads to a service after having performed
   a side-effect. For example a disconnection button that leads to the main
   page of the site, but with the side effect of disconnecting the user.
               
          

          
   To create a coservice, use
   %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALnew_coservice"|%<span class="code"|Eliom_services.new_coservice>%>%>% and
   %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALnew_post_coservice"|%<span class="code"|Eliom_services.new_post_coservice>%>%>%.
   Like %<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALnew_post_service"|%<span class="code"|Eliom_services.new_post_service>%>%,
   they take a public service as parameter
   (labeled %<span class="code"|fallback>%)
   to be used as fallback when the user comes back without the state
   parameter (for example if it was a POST coservice and/or the coservice
   has expired).
          

          
The following example shows the difference between GET coservices
   (bookmarkable) and POST coservices:
          
*wiki*)
(************************************************************)
(************** Coservices. Basic examples ******************)
(************************************************************)

(* -------------------------------------------------------- *)
(* We create one main service and two coservices:           *)

let coservices_example =
  Eliom_services.new_service
    ~path:["coserv"]
    ~get_params:Eliom_parameters.unit
    ()

let coservices_example_post =
  Eliom_services.new_post_coservice
    ~fallback:coservices_example
    ~post_params:Eliom_parameters.unit
    ()

let coservices_example_get =
  Eliom_services.new_coservice
    ~fallback:coservices_example
    ~get_params:Eliom_parameters.unit
    ()


(* -------------------------------------------------------- *)
(* The three of them display the same page,                 *)
(* but the coservices change the counter.                   *)

let _ =
  let c = ref 0 in
  let page sp () () =
    let l3 = Eliom_predefmod.Xhtml.post_form coservices_example_post sp
        (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"incr i (post)" ()]]) ()
    in
    let l4 = Eliom_predefmod.Xhtml.get_form coservices_example_get sp
        (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"incr i (get)" ()]])
    in
    return
      (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "i is equal to ";
                 pcdata (string_of_int !c); br ();
                 a coservices_example sp [pcdata "reload"] (); br ();
                 a coservices_example_get sp [pcdata "incr i"] ()];
              l3;
              l4]))
  in
  Eliom_predefmod.Xhtml.register coservices_example page;
  let f sp () () = c := !c + 1; page sp () () in
  Eliom_predefmod.Xhtml.register coservices_example_post f;
  Eliom_predefmod.Xhtml.register coservices_example_get f
(*wiki*

          
Try [[site:tuto/coserv|%<span class="code"|coserv>%]].
          

          
Note that if the coservice does not exist (for example it
      has expired), the fallback is called.
          

          
In this example, coservices do not take any parameters
      (but the state parameter), but you can create coservices with
      parameters. Note that the fallback of a GET coservice cannot take
      parameters. Actually as coservices parameters have special
      names, it is possible to use a "pre-applied" service as fallback
      ([[manual/dev/2#p3preapplied|see later]]).
          


          
**Exercise:** Rewrite the example of Web site with
        connection (%<span class="code"|session_data_example>%, with session data)
        using a POST
        coservice without parameter to make the disconnection link go back
        to the main page of the site instead of a "disconnection" page.
        It is better for ergonomics, but it would be even better to stay
        on the same page~ ... How to do that with POST coservices?
        A much better solution will be seen in the
        [[manual/dev/2#p2actions|section
        about actions and non-attached coservices]].
      
          

          %<div class="encadre"|
            ====URLs
            
            
While designing a Web site, think carefully about the URLs you
          want to use. URLs are the entry points of your site. Think that
          they may be bookmarked. If you create a link, you want to go to
          another URL, and you want a page to be generated. That page may be
          the default page for the URL (the one you get when you go back
          to a bookmarked page), or another page, that depends on the precise
          link or form you used to go to that URL (link to a coservice,
          or page depending on post data).
          Sometimes, you want that clicking
          a link or submitting a form does something without changing the URL.
          You can do this using //non-attached coservices// (see below).
          
            
      
          >%
          %<div class="encadre"|
            ====Continuations
            
            
Eliom is using the concept of //continuation//.
        A continuation represents the future of a program (what to do after).
        When a user clicks on a link or a form, he chooses the future of the
        computation. When he uses the "back" button of the browser, he chooses
        to go back to an old continuation. Continuations for Web programming
        have been introduced by
        [[http://www-spi.lip6.fr/%7Equeinnec/PDF/www.pdf| Christian Queinnec]],
        and are a big step in
        the understanding of Web interaction.
            

            
        Some programming languages (Scheme...) allow to manipulate
        continuations using //control operators// (like
        %<span class="code"|call/cc>%). The style of programming used by Eliom
        is closer to //Continuation Passing Style// (CPS), and has the
        advantage that it does not need control operators, and fits
        very well Web programming.
        
            

            
Coservices allow to create dynamically
        new continuations that depend on previous interactions with users
        ([[manual/dev/2#p2calc|See the %<span class="code"|calc>% example below]]).
        Such a behaviour is difficult to simulate with traditional Web
        programming.
            

            
If you want continuations dedicated to a particular user
        register them in the session table.
            
      
          >%
          ====Non-attached coservices
          
          
       Non-attached coservices are coservices
       that are not attached to an URL path.
       When you point a link or a form towards such a service, the URL does not
       change. The name of the service is sent as a special parameter.
       
          

          
As for attached coservices, there are GET and POST versions.
          

          
       To create them, use
       %<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALnew_coservice'"|%<span class="code"|Eliom_services.new_coservice'>%>% or
       %<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALnew_post_coservice'"|%<span class="code"|Eliom_services.new_post_coservice'>%>%.
       POST non-attached coservices are really useful if you want a
       link or form to be present on every page but you don't want the
       URL to change. Very often, non-attached POST coservices are
       used with //actions// or //redirections//
       ([[manual/dev/2#p2actions|see more details and an example in the section about
          actions below]]).
       
          

          
Non-attached coservices are distinguished by there names
         (if the optional %<span class="code"|name>% parameter is present), or a number
         (automatically generated and every times different).
          
    
        >%
        ===@@id="p2coservicesinsessiontable"@@Coservices in session tables
        
        %<div id="p2calc" class="onecol"|
          
You can register coservices in session tables to create
       dynamically new services dedicated to an user.
       Here is an example of pages that add two integers.
       Once the first number is sent by the user, a coservice
       is created and registered in the session table. This service
       takes the second number as parameter and displays the result of
       the sum with the first one.
       Try to duplicate the pages and/or to use the back button of your
       navigator to verify that it has the expected behaviour.
          

          
*wiki*)(*zap* ------------------------------------------------------------------ *)
(* You can register coservices in session tables.
   Use this if you want a link or a form which depends precisely on an
   instance of the web page, for example to buy something on an internet shop.
   UPDATE: Actually it is not a good example, because what we want in a shop
   is the same shoping basket for all pages.
   SEE calc example instead.
*)
(* zap* *)
let session_name = "shop_example"
(* *zap *)
let shop_without_post_params =
  new_service
   ~path:["shop"]
   ~get_params:unit
    ()

let shop_with_post_params =
  new_post_service
    ~fallback:shop_without_post_params
    ~post_params:(string "article")
    ()

let write_shop shop url =
  (post_form shop url
     (fun article ->
        let sb = string_input ~input_type:`Text ~name:article () in
          <:xmllist< <p> What do you want to buy? $sb$ </p> >>) ())

let shop_public_main_page sp () () =
  let f = write_shop shop_with_post_params sp in
  return << <html><body>$f$</body></html> >>

let _ =
  register shop_without_post_params shop_public_main_page


let write_shopping_basket shopping_basket =
  let rec aux = function
      [] -> [ << <br/> >> ]
    | a::l -> let fol = aux l in <:xmllist< $str:a$ <br/> $list:fol$ >>
  in
  let ffol = aux shopping_basket in
    <:xmllist< Your shopping basket: <br/> $list:ffol$ >>

let rec page_for_shopping_basket sp shopping_basket =
  let coshop_with_post_params =
    new_post_coservice
      ~fallback:shop_without_post_params
      ~post_params:(string "article")
      ()
  and copay =
    new_post_coservice
      ~fallback:shop_without_post_params
      ~post_params:unit
      ()
  in
    register_for_session (* zap* *) ~session_name (* *zap *)
      ~sp
      ~service:coshop_with_post_params
      (fun sp () article ->
                 page_for_shopping_basket
                   sp (article::shopping_basket));
    register_for_session (* zap* *) ~session_name (* *zap *)
      ~sp
      ~service:copay
      (fun sp () () ->
        return
           << <html><body>
                <p>You are going to pay:
                  $list:write_shopping_basket shopping_basket$ </p>
              </body></html> >>);
       return << <html>
           <body>
             <div>$list:write_shopping_basket shopping_basket$</div>
             $write_shop coshop_with_post_params sp$
             $post_form copay sp
                    (fun _ -> [p [string_input
                                    ~input_type:`Submit ~value:"pay" ()]]) ()$
           </body>
         </html> >>

let _ = register
  ~service:shop_with_post_params
  (fun sp () article -> page_for_shopping_basket sp [article])
(* *zap*)(*zap* Queinnec example: *zap*)
(************************************************************)
(*************** calc: sum of two integers ******************)
(************************************************************)

(*zap* *)
let session_name = "calc_example"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create two main services on the same URL,             *)
(* one with a GET integer parameter:                        *)

let calc =
  new_service
    ~path:["calc"]
    ~get_params:unit
    ()

let calc_i =
  new_service
    ~path:["calc"]
    ~get_params:(int "i")
    ()


(* -------------------------------------------------------- *)
(* The handler for the service without parameter.           *)
(* It displays a form where you can write an integer value: *)

let calc_handler sp () () =
  let create_form intname =
    [p [pcdata "Write a number: ";
        Eliom_predefmod.Xhtml.int_input ~input_type:`Text ~name:intname ();
        br ();
        Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Send" ()]]
  in
  let f = Eliom_predefmod.Xhtml.get_form calc_i sp create_form in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* -------------------------------------------------------- *)
(* The handler for the service with parameter.              *)
(* It creates dynamically and registers a new coservice     *)
(* with one GET integer parameter.                          *)
(* This new coservice depends on the first value (i)        *)
(* entered by the user.                                     *)

let calc_i_handler sp i () =
  let create_form is =
    (fun entier ->
       [p [pcdata (is^" + ");
           int_input ~input_type:`Text ~name:entier ();
           br ();
           string_input ~input_type:`Submit ~value:"Sum" ()]])
  in
  let is = string_of_int i in
  let calc_result =
    register_new_coservice_for_session
      ~sp
      ~fallback:calc
      ~get_params:(int "j")
      (fun sp j () ->
        let js = string_of_int j in
        let ijs = string_of_int (i+j) in
        return
          (html
             (head (title (pcdata "")) [])
             (body
                [p [pcdata (is^" + "^js^" = "^ijs)]])))
  in
  let f = get_form calc_result sp (create_form is) in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register calc   calc_handler;
  Eliom_predefmod.Xhtml.register calc_i calc_i_handler
(*wiki*

          
[[site:tuto/calc| See the result]].
          
    
        >%





        ===@@id="p2actions"@@Actions
        
        %<div class="onecol"|
          
Actions are services that do not generate any page.
   Use them to perform an effect on the server (connection/disconnection
   of a user, adding something in a shopping basket, delete a message in
   a forum, etc.). The page you link to is redisplayed after the action.
   For ex, when you have the same form (or link) on several pages
   (for ex a connection form),
   instead of making a version with post params of all these pages,
   you can use only one action, registered on a non-attached coservice.
   To register actions, just use the module %<ocsigendoc version="dev" file="Eliom_predefmod.Action.html"|%<span class="code"|Eliom_predefmod.Action>%>%
   instead of %<ocsigendoc version="dev" file="Eliom_predefmod.Xhtml.html"|%<span class="code"|Eliom_predefmod.Xhtml>%>% (or %<ocsigendoc version="dev" file="Eliom_duce.Xhtml.html"|%<span class="code"|Eliom_duce.Xhtml>%>%, etc.).
   For example
     %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_predefmod.Action.html" fragment="VALregister"|%<span class="code"|Eliom_predefmod.Action.register>%>%>%,
     %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_predefmod.Action.html" fragment="VALregister_new_service"|%<span class="code"|Eliom_predefmod.Action.register_new_service>%>%>%,
     %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_mkreg.ELIOMREGSIG1.html" fragment="VALregister_for_session"|%<span class="code"|Eliom_predefmod.Action.register_for_session>%>%>%.            \\
      
          

          
Here is one simple example. Suppose you wrote a function
        %<span class="code"|remove>% to remove one piece of data from a database
        (taking an identifier of the data).
        If you want to put a link on your page to call this function
        and redisplay the page, just create an action like this:
      
          

          
%<code language="ocaml"|
let remove_action =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~post_params:(Eliom_parameters.int "id")
    (fun sp () id -> remove id)
>%

          
Then wherever you want to add a button to do that action
         (on data %<span class="code"|id>%),
      create a form like:
          

          
%<code language="ocaml"|
Eliom_predefmod.Xhtml.post_form remove_action sp
  (fun id_name ->
     Eliom_predefmod.Xhtml.int_input
       ~input_type:`Hidden ~name:id_name ~value:id ();
     Eliom_predefmod.Xhtml.string_input
       ~input_type:`Submit ~value:("remove "^string_of_int id) ())

>%

          
Here we rewrite the example %<span class="code"|session_data_example>%
      using actions
      and named non-attached coservices
      (note the POST coservice for disconnection, much better than the
      previous solution that was using another URL).
          

          
*wiki*)
(************************************************************)
(************ Connection of users, version 3 ****************)
(************************************************************)

(*zap* *)
let session_name = "connect_example3"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example3 =
  Eliom_services.new_service
    ~path:["action"]
    ~get_params:Eliom_parameters.unit
    ()

let connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect3"
    ~post_params:(Eliom_parameters.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let disconnect_action =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"disconnect3"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp ())


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box sp =
  Eliom_predefmod.Xhtml.post_form connect_action sp
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Eliom_predefmod.Xhtml.string_input
                     ~input_type:`Text ~name:loginname ()]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "connect_example3" service (main page):    *)

let connect_example3_handler sp () () =
  let sessdat = Eliom_sessions.get_volatile_session_data (*zap* *) ~session_name (* *zap*) ~table:my_table ~sp () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired
          | Eliom_sessions.No_data -> [login_box sp]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  Eliom_sessions.set_volatile_session_data (*zap* *) ~session_name (* *zap*) ~table:my_table ~sp login;
  return ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register ~service:connect_example3 connect_example3_handler;
  Eliom_predefmod.Action.register ~service:connect_action connect_action_handler
(*wiki*

          
[[site:tuto/action| See these pages]].
          


          
      Note that actions return %<span class="code"|()>%.
      [[manual/dev/3#p3infofallbacks|See later for more advanced use]]
     
          



          
       That version of the site with connection solves the main problems of
       [[manual/dev/2#p2sessiondata|%<span class="code"|sessdata>%]]:
     
          

          
*         Connection and disconnection stay on the same page,
       
*         If you want a connection/disconnection form on each page, no need
         to create a version with POST parameters of each service.
       
                 
          


          
       We'll see later
      [[manual/dev/3#p3infofallbacks|how to display an error message]]
       if the connection goes wrong, and
      [[manual/dev/3#p3persistenceofsessions|how to have persistent sessions]]
      (that stay opened even if the server is re-launched).
     
          



    
        >%

        ===@@id="p2detailsonserviceregistration"@@Details on service registration
        
        %<div class="encadre sanstitre"|
          
*All services created during initialisation must be registered
        in the public table during the initialisation phase of your module.
        If not, the server will not start (with an error message in the logs).
        Thus, there will always be a service to answer when somebody clicks on
        a link or a form.
        
*Services
         may be registered in the public table after initialisation with
         %<span class="code"|register>% only if you add the %<span class="code"|~sp>%
           parameter.              \\
    If you use that for main services,
    you will dynamically create new URLs!
    This may be dangerous as they will disappear if you stop the server.
    Be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks will be broken!              \\
    The use of that feature is discouraged for coservices
    without timeout, as such coservices will be available only until the end
    of the server process (and it is not possible to re-create them with the
    same key).
        
*Do not register twice the same service in the public table,
          and do not replace a service
          by a directory (or vice versa). If this happens during the
          initialisation phase, the server won't start.
          If this happens after, it will be ignored (with a warning in the
          logs).
        
*All services (not coservices) must be created in
        a module loaded inside a %<span class="code"|<site>%> tag of the
        config file (because they will be attached to a directory).
        Not possible for modules loaded inside %<span class="code"|<extension>%>
        or %<span class="code"|<library>%>.
        
*GET coservices (whithout POST parameters) can be registered
        only with a main service without GET/POST parameters as fallback.
        But it may be a
      [[manual/dev/3#p3preapplied|//preapplied//]]
        service (see below).
        
*Services with POST parameters (main service or coservice)
        can be registered with a (main or co) service without POST
        parameters as fallback.
*The registration of (main) services must be completed before
          the end of the loading of the module. It is not possible to launch
          a (Lwt) thread that will register a service later, as
          registering a service needs access to config file
          information (for example the directory of the site).
          If you do this, the server will raise
          %<ocsigendoc version="dev" file="Eliom_common.html" fragment="EXCEPTIONEliom_function_forbidden_outside_site_loading"|%<span class="code"|Eliom_common.Eliom_function_forbidden_outside_site_loading >%>%
          most of the time,
          but you may also get unexpected results (if the thread is executed
          while another site is loaded).
          If you use threads in the initialization phase of your module
          (for example if you need information from a database),
          use %<ocsigendoc version="dev" file="Lwt_unix.html" fragment="VALrun"|%<span class="code"|Lwt_unix.run>%>% to wait the end of the thread.
        
                  
          
    
        >%
      >%      

%<||3>%
%<div class='leftcol'|%<leftcoldoc version="dev">%>%
      %<div class="colprincipale"|
        ==3. More details on services and page generation
        
        %<div class="onecol"|
          
       You now know all Eliom's main concepts. In that part, we'll give
       more details on some aspects that have been seen before:
     
          

          
*The different types of output for services
*Timeouts and error handling
*Persistence of sessions
*Advanced forms
                 
          
    
        >%


        ===@@id="p3staticparts"@@Static parts
        
        %<div class="onecol"|
          ====Fully static pages
          
          
The %<span class="code"|staticmod>% extension allows to associate
         to your site a static directory
         where you can put all the static (non generated) parts of your
         web-site (for examples images and stylesheets).
         See the default config file %<span class="code"|ocsigen.conf>% to
         learn how to do that.
         A predefined service can be used to make links to static files.
         Get it using
         %<span class="code"|(static_dir ~sp)>%.
         That service takes as string parameter the name of the file.
                            \\
                For example
          

          
%<code language="ocaml"|Eliom.a
  (static_dir ~sp)
  sp
  [pcdata "download image"]
  "ocsigen8-100x30.png"
>%

          
creates this link:
         [[site:ocsigen8-100x30.png|download image]]
      
          

          
It is now also possible to handle static pages with Eliom, using
      %<span class="code"|Eliom_predefmod.Files>% ([[manual/dev/3#p3eliomfiles|see later]]).
      
          
      %<|h4>Static parts of a page</h4>      <em>To be available soon</em>%
    
        >%



        ===@@id="p3otherkindsofpages"@@Other kinds of pages
        
        %<div class="onecol"|
          ====Sending portions of pages
          
          
     The %<ocsigendoc version="dev" file="Eliom_predefmod.Blocks.html"|%<span class="code"|Eliom_predefmod.Blocks>%>% module allows to register services that
     send portions of pages, of any type that may be contained directly in
     a %<span class="code"|<body>%> tag (blocks of xhtml DTD).
     It is useful to create AJAX pages
     (i.e. pages using the %<span class="code"|XMLHttpRequest>% Javascript object).
     Note that the service returns a list of blocks.
          

*wiki*)
let divpage =
  Eliom_predefmod.Blocks.register_new_service
    ~path:["div"]
    ~get_params:unit
    (fun sp () () ->
      return
        [div [h2 [pcdata "Hallo"];
              p [pcdata "Blablablabla"] ]])
(*wiki*

          
     The %<ocsigendoc version="dev" file="Eliom_predefmod.SubXhtml.html"|%<span class="code"|Eliom_predefmod.SubXhtml>%>% module allows to create other modules for
     registering portions of pages of other types.
     For example, %<ocsigendoc version="dev" file="Eliom_predefmod.Blocks.html"|%<span class="code"|Eliom_predefmod.Blocks>%>%
     is defined by:
          

          
%<code language="ocaml"|
module Blocks = SubXhtml(struct
  type content = Xhtmltypes.body_content
end)

>%


          ====Redirections
          
          
     The %<ocsigendoc version="dev" file="Eliom_predefmod.Redirection.html"|%<span class="code"|Eliom_predefmod.Redirection>%>% module allows to register HTTP redirections.            \\  **[New in 1.1.0. For 1.0.0, please see module %<span class="code"|Eliom_predefmod.Redirections>%.]**            \\
     If a request is done towards such a service, the server asks the browser
     to retry with another URL. 
    
          

          
     Such services return a GET service without parameter at all.
     Example:
    
          
*wiki*)
let redir1 = Eliom_predefmod.Redirection.register_new_service
    ~options:`Temporary
    ~path:["redir"]
    ~get_params:Eliom_parameters.unit
   (fun sp () () -> Lwt.return coucou)
(*wiki*

          
     If you want to give parameters to such services, use
     %<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALpreapply"|%<span class="code"|Eliom_services.preapply>%>% (see also 
     [[manual/dev/3#p3preapplied|later in the tutorial]]).
     Example:
    
          
 *wiki*)
let redir = Eliom_predefmod.Redirection.register_new_service
    ~options:`Temporary
    ~path:["redir"]
    ~get_params:(int "o")
   (fun sp o () ->
      Lwt.return
        (Eliom_services.preapply coucou_params (o,(22,"ee"))))
(*wiki*

          
The %<span class="code"|options>% parameter may be either
      %<span class="code"|`Temporary>% or %<span class="code"|`Permanent>%.
          

          
[[site:tuto/redir?o=11| Try it]].
          


          
Note that the cost of a redirection is one more query and
      one more answer.
      
          



          ====@@id="p3eliomfiles"@@Sending files
          
          
You may want to register a service that will send files.
      To do that, use the %<ocsigendoc version="dev" file="Eliom_predefmod.Files.html"|%<span class="code"|Eliom_predefmod.Files>%>% module. Example:
      
          

          
%<code language="ocaml"|
let sendfile =
  Files.register_new_service
    ~path:["sendfile"]
    ~get_params:unit
    (fun _ () () -> return "filename")

>%

          
Other example, with suffix URL:
      
          

          
%<code language="ocaml"|
let sendfile2 =
  Files.register_new_service
    ~path:["files"]
    ~get_params:(suffix (all_suffix "filename"))
    (fun _ s () -> return ("//path//"^(Ocsigen_lib.string_of_url_path ~encode:false s)))

>%

          
The extension %<span class="code"|Staticmod>% is another way to
       handle static files (see the default
       configuration file for more information).
      
          


          ====Registering services that decide what they want to send
          
          
You may want to register a service that will send, for instance,
      sometimes
      an xhtml page, sometimes a file, sometimes something else.
      To do that, use the %<ocsigendoc version="dev" file="Eliom_predefmod.Any.html"|%<span class="code"|Eliom_predefmod.Any>%>% module, together
      with the %<span class="code"|send>% function of the module you want
      to use. Example:
      
          
*wiki*)
let send_any =
  Eliom_predefmod.Any.register_new_service
    ~path:["sendany"]
    ~get_params:(string "type")
   (fun sp s () ->
     if s = "valid"
     then
       Eliom_predefmod.Xhtml.send sp
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata
                        "This page has been statically typechecked.
                         If you change the parameter in the URL you will get an unchecked text page"]]))
     else
       Eliom_predefmod.HtmlText.send sp
         "<html><body><p>It is not a valid page. Put type=\"valid\" in the URL to get a typechecked page.</p></body></html>"
   )
(*wiki*

          
      See [[site:tuto/sendany?type=valid| a valid page]],
      and [[site:tuto/sendany?type=non+valid| a non valid page]].
      
          

          
You may also use %<ocsigendoc version="dev" file="Eliom_predefmod.Any.html"|%<span class="code"|Eliom_predefmod.Any>%>% to send cookies or to choose a
         different charset than the default
        (default charset is set in configuration file)
         for the page you send. To do that use the optional parameters
          %<span class="code"|?cookies>% and %<span class="code"|?charset>% of the
         %<span class="code"|send>% function.
      
          

          ====Cookies
          
          
      A simplest way to set your own cookies on the client is to use
      functions like
      %<span class="code"|Eliom_predefmod.Xhtml.Cookies.register>% instead of
      %<span class="code"|Eliom_predefmod.Xhtml.register>%.
      The function you register returns a pair containing the page (as usual)
      and a list of cookies, of type %<span class="code"|Eliom_services.cookie>%
      defined by:
      
          

          
%<code language="ocaml"|
type cookie =
  | Set of Eliom_common.cookie_type * 
  string list option * float option * string * string * bool
  | Unset of Eliom_common.cookie_type * string list option * string

>%

{{{Eliom_common.cookie_type}}} is {{{Eliom_common.CBrowser}}}
for regular browser cookies, or {{{Eliom_common.CTab}}} for tab cookies
(available only if you have a client side Eliom program).
          
**[New in 1.1.0]** For version 1.0.0, the type 
%<span class="code"|cookie>% was slightly different (no secure cookies).
          

          
     The %<span class="code"|string list option>% is a the path for which you want
     to set/unset the cookie (relative to the main directory of your site,
   defined
     in the configuration file). %<span class="code"|None>% means for all your site.
     
          

          
     The %<span class="code"|float option>% is a the expiration date
     (Unix timestamp, in seconds since the epoch).
     %<span class="code"|None>% means that the cookie will expire when the browser
     will be closed.
     
          

          
     If the %<span class="code"|bool>% is true and the protocol is https, 
     the server will ask the browser to send the cookie only through
     secure connections.
     
          

          
      You can access the cookies sent by the browser using
      %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_cookies"|%<span class="code"|Eliom_sessions.get_cookies sp>%>%.
     
          

          
      Example:
     
*wiki*)
let cookiename = "mycookie"

let cookies = new_service ["cookies"] unit ()

let _ = Cookies.register cookies
    (fun sp () () ->
      return
       ((html
         (head (title (pcdata "")) [])
         (body [p [pcdata (try
                             "cookie value: "^
                             (Ocsigen_lib.String_Table.find
                                cookiename (Eliom_sessions.get_cookies sp))
                           with _ -> "<cookie not set>");
                   br ();
                   a cookies sp [pcdata "send other cookie"] ()]])),
        [Eliom_services.Set (Eliom_common.CBrowser,
                             None, None,
                             cookiename,
                             string_of_int (Random.int 100),
                             false)]))
(*wiki*

          
[[site:tuto/cookies| Try it]].
          
    
        >%


        ===@@id="p3persistenceofsessions"@@Persistence of sessions
        
        %<div class="onecol"|
          
Tables of sessions (for data or services) are kept in memory,
        and thus will disappear if you close the server process.
        To solve this problem, Ocsigen allows to reload the modules of
        your configuration file without shutting down the server.
        Another solution provided by Eliom is to save session data on hard drive.
      
          


          ====Updating sites without shutting down the server
          
          
To reload the modules of the configuration file without
       stoping the server, use %<span class="code"|/etc/init.d/ocsigen reload>%
       for most of the distributions, or do it manually using:
          

          
%<div class="pre"|echo reload > /var/run/ocsigen_command >%


          
       Only modules loaded inside %<span class="code"|<site>%> or
       %<span class="code"|<library>%> will be reloaded.
       Module loaded using %<span class="code"|<extension>%> will not.
      
          

          
        Have a look at the logs to see if all went well during the reload.
        If something went wrong, old services may still be reachable.
      
          

          
        Note that coservices created with the old modules or
        URLs that have not been masked by new ones
        will still reachable after the update.
      
          

          
        During the reload, some information of the configuration file
        will not be re-read (for example port numbers, user and group, etc.).
      
          



          ====Persistent data
          
          
        Eliom allows to use more persistent data, using the module
        %<ocsigendoc version="dev" file="Ocsipersist.html"|%<span class="code"|Ocsipersist>%>%. (%<span class="code"|Ocsipersist>% is needed in
        %<span class="code"|eliom.cma>%, thus you need to dynlink it in the
        configuration file before %<span class="code"|Eliom>%).
        There are currently two implementations of %<span class="code"|Ocsipersist>%:
        %<span class="code"|ocsipersist-dbm.cma>% (uses the DBM database) and
        %<span class="code"|ocsipersist-sqlite.cma>% (uses the SQLite database,
        and depends on %<span class="code"|sqlite3.cma>%).
      
          

          
These modules allow to:
      
          

          
*Create persistent references
          (still present after restarting the server),
*Create persistent association tables,
*Set persistent session data (using
        %<span class="code"|set_persistent_data>%, see below).
                  
          

          
Note that persistent data are serialized on hard drive using
        OCaml's %<span class="code"|Marshal>% module:
      
          

          %<div class="importantwarning"|
            
*It is not possible to serialize closures or services
         (as we are using dynamic linking).
* If you ever change the type of serialised data, don't
 forget to delete the database file!
 Or if you really want to keep it, and
 you know what you are doing, you can use the sqlite client to manually
 update the table or a program to create a new sqlite or dbm table
 for the new type.
        
                    
            
   
          >%
          
   Suppose for example that you use %<span class="code"|get/set_persistent_data>%
   (see below) to store a (int, string)
 tuple with the user's login credentials.  At this point you stop the
 server, and change the code such that get/set_persistent_data now to store
 a (int, string, string).  Now recompile and restart the server.  If by any
 chance a client with an old cookie reconnects, you get a segfault on the
 server, because of the type change in the data stored in the DB backend ...
   
It is possible to customize the location of the database on the 
file system. For example, with sqlite:
%<div class="pre"|
    <extension findlib-package="ocsigen.ext.ocsipersist-sqlite">
      <database file="_DATADIR_/ocsidb"/>
    </extension>
 >%
And with DBM, you can customize the location of the database and the
name of the {{{ocsidbm}}} process you want to use:
%<div class="pre"|
    <extension findlib-package="ocsigen.ext.ocsipersist-dbm">
      <store dir="_DATADIR_"/>
      <ocsidbm name="_EXTRALIBDIR_/ocsidbm"/>
    </extension>
 >%
          

          ====Persistent references
          
          
%<span class="code"|Ocsipersist>% allows to create persistent references.
       Here is an example of page with a persistent counter:
      
          

*wiki*)
let mystore = Ocsipersist.open_store "eliomexamplestore2"

let count2 =
  let next =
    let cthr = Ocsipersist.make_persistent mystore "countpage" 0 in
    let mutex = Lwt_mutex.create () in
    (fun () ->
      cthr >>=
      (fun c ->
        Lwt_mutex.lock mutex >>= fun () ->
        Ocsipersist.get c >>=
        (fun oldc ->
          let newc = oldc + 1 in
          Ocsipersist.set c newc >>=
          (fun () ->
            Lwt_mutex.unlock mutex;
            return newc))))
  in
  register_new_service
    ~path:["count2"]
    ~get_params:unit
    (fun _ () () ->
      next () >>=
      (fun n ->
        return
         (html
          (head (title (pcdata "counter")) [])
          (body [p [pcdata (string_of_int n)]]))))

(*wiki*

          
[[site:tuto/count2| See this example here]].
      
          

          ====Persistent tables
          
          
%<span class="code"|Ocsipersist>% also allows to create very basic
       persistent tables. Use them if you don't need complex requests
       on your tables. Otherwise use a database such as %<span class="code"|PostgreSQL>%
       or %<span class="code"|MySQL>%. Here are the interface you can use:
      
          

          
%<code language="ocaml"|
type 'value table

val open_table : string -> 'value table

val find : 'value table -> string -> 'value Lwt.t

val add : 'value table -> string -> 'value -> unit Lwt.t

val remove : 'value table -> string -> unit Lwt.t

>%


          
      As you can see, all these function are cooperative.
    
          


          ====Persistent session data
          
          
%<span class="code"|Eliom>% also implements persistent session tables.
       You can use them instead of memory tables if you don't need
       to register closures.
          

          
The following example is a new version of our site
       with users, with persistent connections.
       (%<span class="code"|login_box>%, %<span class="code"|disconnect_box>%
       and %<span class="code"|disconnect_action>%
       are the same as
      [[manual/dev/2#p2actions|before]]).
      
          

*wiki*)
(************************************************************)
(************ Connection of users, version 4 ****************)
(**************** (persistent sessions) *********************)
(************************************************************)

(*zap* *)
let session_name = "persistent_sessions"
(* *zap*)
let my_persistent_table =
  create_persistent_table "eliom_example_table"

(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let persist_session_example =
  Eliom_services.new_service
    ~path:["persist"]
    ~get_params:unit
    ()

let persist_session_connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect4"
    ~post_params:(string "login")
    ()

(* disconnect_action, login_box and disconnect_box have been
   defined in the section about actions *)

(*zap* *)

(* -------------------------------------------------------- *)
(* Actually, no. It's a lie because we don't use the
   same session name :-) *)
(* new disconnect action and box:                           *)

let disconnect_action =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"disconnect4"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session ~session_name ~sp ())

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let bad_user_key = Polytables.make_key ()
let get_bad_user table = 
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box sp session_expired action =
  Eliom_predefmod.Xhtml.post_form action sp
    (fun loginname ->
      let l =
        [pcdata "login: ";
         string_input ~input_type:`Text ~name:loginname ()]
      in
      [p (if get_bad_user (Eliom_sessions.get_request_cache sp)
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

let persist_session_example_handler sp () () =
  Eliom_sessions.get_persistent_session_data (*zap* *) ~session_name (* *zap*)
    ~table:my_persistent_table ~sp () >>= fun sessdat ->
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired ->
              [login_box sp true persist_session_connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | Eliom_sessions.No_data ->
              [login_box sp false persist_session_connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))


(* ----------------------------------------------------------- *)
(* Handler for persist_session_connect_action (user logs in):  *)

let persist_session_connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  if login = "toto" (* Check user and password :-) *)
  then
    Eliom_sessions.set_persistent_session_data (*zap* *) ~session_name (* *zap*) ~table:my_persistent_table ~sp login
  else ((*zap* *)Polytables.set (Eliom_sessions.get_request_cache sp) bad_user_key true;(* *zap*)return ())


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register
    ~service:persist_session_example
    persist_session_example_handler;
  Eliom_predefmod.Action.register
    ~service:persist_session_connect_action
    persist_session_connect_action_handler
(*wiki*

          
[[site:tuto/persist| See this example here]].
      
          


          
        As it is not possible to serialize closures, there is no persistent
        session service table. Be very carefull if you use both persistent
        session data tables and service session tables,
        as your session may become inconsistent (use the session service
        table only for volatile services, like coservices with timeouts).
      
          

    
        >%



===@@id="p3otherconcepts"@@Other concepts
        
        %<div class="onecol"|
====@@id="p3preapplied"@@Pre-applied services
          
          
Services or coservices with GET parameters can be preapplied
     to obtain a service without parameters. Example:
    
          

          
%<code language="ocaml"|
let preappl = Eliom_services.preapply coucou_params (3,(4,"cinq"))
    
>%

          
     It is not possible to register something on a preapplied service,
     but you can use them in links or as fallbacks for coservices.
    
          


          ====@@id="p3preapplied"@@Void coservices **[New in 1.1.0]**
          
          
%<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALvoid_coservice'"|%<span class="code"|Eliom_services.void_coservice'>%>%
     is a special non-attached action, with special behaviour:
     it has no parameter at all, even non-attached parameters.
     Use it if you want to make a link to the current page
     without non-attached parameters.
     It is almost equivalent to a POST non-attached coservice without POST
     parameters, on which you register an action that does nothing,
     but you can use it with %<span class="code"|<a>%> links, not only forms.
     Example:

%<code language="ocaml"|
Eliom_duce.Xhtml.a
  ~service:Eliom_services.void_coservice'
  ~sp
  {{ "cancel" }}
  ()
>%

There is also 
%<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALhttps_void_coservice'"|%<span class="code"|Eliom_services.https_void_coservice'>%>%
(same, but forces use of HTTPS),
%<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALvoid_hidden_coservice'"|%<span class="code"|Eliom_services.void_hidden_coservice'>%>%, and
%<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALhttps_void_hidden_coservice'"|%<span class="code"|Eliom_services.https_void_hidden_coservice'>%>%. "Hidden" means that they keep GET non attached parameters.


====@@id="p3infofallbacks"@@Giving information to fallbacks
          

          
    The function
    %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_link_too_old"|%<span class="code"|Eliom_sessions.get_link_too_old>%>%
    returns %<span class="code"|true>% if the coservice called has not been found.
    In that case, the current service is the fallback.
  
          

          
    The function
    %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_expired_service_sessions"|%<span class="code"|Eliom_sessions.get_expired_service_sessions>%>%
    returns returns the list of names of service sessions expired 
    for the current request.
    
          

          
It is also possible to send other information to fallback,
    about what succeeded before they were called. 
    Put this information in the //request cache//.
    The request cache is a polymorphic table returned by
     %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_request_cache"|%<span class="code"|Eliom_sessions.get_request_cache sp>%>%.
     See the module
     %<ocsigendoc version="dev" file="Polytables.html"|%<span class="code"|Polytables>%>% to understand how to use it.
     You may also want to use this table to cache some data during the 
     duration of a request.
    
          

          
    Here is a new version of the
          [[manual/dev/2#p2actions|example of session with actions,]] using the polymorphic request data table:
    
*wiki*)
(************************************************************)
(************ Connection of users, version 6 ****************)
(************************************************************)
(*zap* *)
let session_name = "connect_example6"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example6 =
  Eliom_services.new_service
    ~path:["action2"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect6"
    ~post_params:(string "login")
    ()

(* new disconnect action and box:                           *)

let disconnect_action =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"disconnect6"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp ())

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()


let bad_user_key = Polytables.make_key ()
let get_bad_user table = 
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box sp session_expired action =
  Eliom_predefmod.Xhtml.post_form action sp
    (fun loginname ->
      let l =
        [pcdata "login: ";
         string_input ~input_type:`Text ~name:loginname ()]
      in
      [p (if get_bad_user (Eliom_sessions.get_request_cache sp)
      then (pcdata "Wrong user")::(br ())::l
      else
        if session_expired
        then (pcdata "Session expired")::(br ())::l
        else l)
     ])
    ()

(* -------------------------------------------------------- *)
(* Handler for the "connect_example6" service (main page):   *)

let connect_example6_handler sp () () =
  let group =
    Eliom_sessions.get_volatile_data_session_group (*zap* *) ~session_name (* *zap*) ~sp ()
  in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match group with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired ->
              [login_box sp true connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | Eliom_sessions.No_data ->
              [login_box sp false connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))

(* -------------------------------------------------------- *)
(* New handler for connect_action (user logs in):           *)

let connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_sessions.set_volatile_data_session_group ~set_max:4 (*zap* *) ~session_name (* *zap*) ~sp login;
    return ()
  end
  else begin
    Polytables.set (Eliom_sessions.get_request_cache sp) bad_user_key true;
    return ()
  end


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register ~service:connect_example6 connect_example6_handler;
  Eliom_predefmod.Action.register ~service:connect_action connect_action_handler

(*wiki*
          
[[site:tuto/action2| See this example here]].
      
          

          
        If the actions raises an exception (with %<ocsigendoc version="dev" file="Lwt.html" fragment="VALfail"|%<span class="code"|Lwt.fail>%>%),
        the server will send an error 500 (like for any other service).
        Think about catching the exceptions and put them in the list
        if they correspond to usual cases you want to handle while
        generating the page after the action.
      
          




====Disposable coservices
          
          
It is possible to set a limit to the number of uses of
      (attached or non-attached) coservices. Just give the maximum number
      of uses with the optional %<span class="code"|?max_use>% parameter while
      creating your coservices. Example
      
          

          
*wiki*)
let disposable = new_service ["disposable"] unit ()

let _ = register disposable
    (fun sp () () ->
      let disp_coservice =
        new_coservice ~max_use:2 ~fallback:disposable ~get_params:unit ()
      in
      register_for_session sp disp_coservice
        (fun sp () () ->
          return
            (html
              (head (title (pcdata "")) [])
              (body [p [pcdata "I am a disposable coservice";
                        br ();
                        a disp_coservice sp [pcdata "Try me once again"] ()]]))
        );
      return
        (html
          (head (title (pcdata "")) [])
          (body [p [(if Eliom_sessions.get_link_too_old sp
                    then pcdata "Your link was outdated. I am the fallback. I just created a new disposable coservice. You can use it only twice."
                    else
                    pcdata "I just created a disposable coservice. You can use it only twice.");
                    br ();
                    a disp_coservice sp [pcdata "Try it!"] ()]])))
(*wiki*

          
[[site:tuto/disposable| Try it]].
          

          ====Timeout for sessions
          
          
The default timeout for sessions in one hour. Sessions will be
       automatically closed after that amount of time of inactivity
       from the user.
       You can change that value for your whole site during initialisation
       using:
          

          
%<code language="ocaml"|
Eliom_sessions.set_global_volatile_timeout (Some 7200.)

>%

          
Here 7200 seconds. %<span class="code"|None>% means no timeout.
          

          
       You can change that value for your whole site after initialisation
       using:
          

          
%<code language="ocaml"|
Eliom_sessions.set_global_volatile_timeout ~sp (Some 7200.)

>%

          
       You can change that value for one user only using:
          

          
%<code language="ocaml"|
Eliom_sessions.set_volatile_session_timeout ~sp (Some 7200.)

>%

          
      Note that there is also a possibility to change the default value
      for Eliom in the configuration file like this:
          

          
%<code language="ocaml"|
    <extension findlib-package="ocsigen.ext.eliom">
      <volatiletimeout value="7200"/>
    </extension>

>%

          
%<span class="code"|value="infinity">% means no timeout.
          

          
Warning: that default may be overriden by each site using
        %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALset_global_volatile_timeout"|%<span class="code"|Eliom_sessions.set_global_volatile_timeout>%>% or
        %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALset_default_volatile_timeout"|%<span class="code"|Eliom_sessions.set_default_volatile_timeout>%>%.
        If you want your user to be able to set the default in the
        configuration file for your site (between %<span class="code"|<site>%>
        and %<span class="code"|</site>%>), you must parse the configuration
        (%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_config"|%<span class="code"|Eliom_sessions.get_config ()>%>% function, see below).
     
          




====Timeout for coservices
          
          
It is also possible to put timeouts on coservices using
      the optional parameter %<span class="code"|?timeout>% of functions
      %<span class="code"|new_coservice>%,
      %<span class="code"|new_coservice'>%, etc.
     Note that session coservices cannot survive after the end of the session.
     Use this if you don't want your coservice to be available during all the
     session duration. For example if your coservice is here to show the
     results of a search, you probably want it to be available only for
     a short time. The following example shows a coservice with timeout
     registered in the session table.
     
          
*wiki*)
let timeout = new_service ["timeout"] unit ()

let _ =
  let page sp () () =
    let timeoutcoserv =
      register_new_coservice_for_session
        ~sp ~fallback:timeout ~get_params:unit ~timeout:5.
        (fun _ _ _ ->
           return
             (html
               (head (title (pcdata "Coservices with timeouts")) [])
               (body [p
                 [pcdata "I am a coservice with timeout."; br ();
                  pcdata "Try to reload the page!"; br ();
                  pcdata "I will disappear after 5 seconds of inactivity." ];
                 ])))
    in
    return
      (html
        (head (title (pcdata "Coservices with timeouts")) [])
        (body [p
          [pcdata "I just created a coservice with 5 seconds timeout."; br ();
           a timeoutcoserv sp [pcdata "Try it"] (); ];
          ]))
  in
  register timeout page
(*wiki*
          
[[site:tuto/timeout| See this example here]].
      
          

====Registering coservices in public table during session
          
          
If you want to register coservices in the
     public table during a session, (that is, after the initialisation
     phase of your module), you must add the optional %<span class="code"|~sp>%
     parameter to the %<span class="code"|register>% function.
     Remember that using %<span class="code"|register>% without %<span class="code"|~sp>%
     is possible only during initialisation!
     
          

          
     We recommend to put a timeout on such coservices, otherwise, they
     will be available until the end of the server process, and it will not be
     possible to re-create them when the server is relaunched.
     
          

          
     The following example is a translation of the previous one using
     the public table:
     
*wiki*)
let publiccoduringsess = new_service ["publiccoduringsess"] unit ()

let _ =
  let page sp () () =
    let timeoutcoserv =
      register_new_coservice
        ~sp ~fallback:publiccoduringsess ~get_params:unit ~timeout:5.
        (fun _ _ _ ->
           return
             (html
               (head (title (pcdata "Coservices with timeouts")) [])
               (body [p
                 [pcdata "I am a public coservice with timeout."; br ();
                  pcdata "I will disappear after 5 seconds of inactivity." ];
                 ])))
    in
    return
      (html
        (head (title (pcdata "Public coservices with timeouts")) [])
        (body [p
          [pcdata "I just created a public coservice with 5 seconds timeout."; br ();
           a timeoutcoserv sp [pcdata "Try it"] (); ];
          ]))
  in
  register publiccoduringsess page
(*wiki*
          
[[site:tuto/publiccoduringsess| See this example here]].
      
          

====Defining an exception handler for the whole site
          
          
When an exception is raised during the generation of a page,
     or when the page has not been found or has wrong parameters,
     an HTTP error 500 or 404 is sent to the client. You may want to
     catch these exceptions to print your own error page.
     Do this using %<ocsigendoc version="dev" file="Eliom_services.html" fragment="VALset_exn_handler"|%<span class="code"|Eliom_services.set_exn_handler>%>%.
     Here is the handler used by this tutorial:
     
          
*wiki*)
let _ = Eliom_services.set_exn_handler
   (fun sp e -> match e with
    | Eliom_common.Eliom_404 ->
        Eliom_predefmod.Xhtml.send ~code:404 ~sp
          (html
             (head (title (pcdata "")) [])
             (body [h1 [pcdata "Eliom tutorial"];
                    p [pcdata "Page not found"]]))
    | Eliom_common.Eliom_Wrong_parameter ->
        Eliom_predefmod.Xhtml.send ~sp
          (html
             (head (title (pcdata "")) [])
             (body [h1 [pcdata "Eliom tutorial"];
                    p [pcdata "Wrong parameters"]]))
    | e -> fail e)
(*wiki*

====Giving configuration options to your sites
          
          
You can add your own options in the configuration
       file for your Web site. For example:
          

          
%<code language="ocaml"|
    <eliom module="//path_to///yourmodule.cmo">
      <youroptions> ...
    </eliom>

>%

          
       Use %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_config"|%<span class="code"|Eliom_sessions.get_config ()>%>% during the initialization
       of your module to get the data between
       %<span class="code"|<eliom>%> and %<span class="code"|</eliom>%>.
       Warning: parsing these data is very basic for now.
       That feature will be improved in the future.
      
          

====More about sessions - session names
          
          
By default, Eliom is using three cookies :
          

          
*One for session services,
*one for volatile session data,
*one for persistent session data.
                  
          

          
They correspond to three different sessions (opened only if needed).
   %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALclose_session"|%<span class="code"|Eliom_sessions.close_session>%>%>%
       closes all three sessions, but you may want to desynchronize
       the three sessions by using
   %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALclose_persistent_session"|%<span class="code"|Eliom_sessions.close_persistent_session>%>%>% (persistent session),
   %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALclose_service_session"|%<span class="code"|Eliom_sessions.close_service_session>%>%>% (session services), or
   %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALclose_data_session"|%<span class="code"|Eliom_sessions.close_data_session>%>%>% (volatile data session).
     There is also
   %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALclose_volatile_session"|%<span class="code"|Eliom_sessions.close_volatile_session>%>%>% for both volatile data session and session services.
       The module %<ocsigendoc version="dev" file="Eliom_sessions.html"|%<span class="code"|Eliom_sessions>%>% also contains functions for setting timeouts or expiration dates for cookies for each kind of session.
      
          

          
If you need more sessions (for example several different data sessions)
         for the same site, you can give a name to your sessions by giving
         the optional parameter %<span class="code"|?session_name>% to functions like
     %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALclose_data_session"|%<span class="code"|Eliom_sessions.close_data_session>%>%>%,
     %<span class="Cem"|%<ocsigendoc version="dev" file="Eliom_mkreg.ELIOMREGSIG1.html" fragment="VALregister_for_session"|%<span class="code"|register_for_session>%>%>%, or
      %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_volatile_session_data"|%<span class="code"|Eliom_sessions.get_volatile_session_data>%>%.
       Note that this tutorial has been implemented using this feature,
       even if it has been hidden for the sake of simplicity.
       That's how the different examples of sessions in this tutorial are
       independant.
      
          

          ====Secure services **[New in 1.1.0]**
          
          
You may want to impose HTTPS for some of your services.
       To do that, use the optional parameter %<span class="code"|~https:true>%
       while creating your service.

It is also possible to require http or https while creating a link or
      a form (using the optional parameter %<span class="code"|~https:true>%).
      But it is never possible to make an http link towards an https service,
      even if you request it.

Warning: if the protocol needs to be changed (from http to https 
       or vice versa), Eliom will generate absolute URLs.
       The host name and port numbers are guessed from the IP and the 
       configuration by default, but it is recommended to specify them
       in the configuration file. For example:

          %<div class="pre"|<host hostfilter="*.org" defaulthostname="www.mywebsite.org" defaulthttpport="8080" defaulthttpsport="4433"> ... </host>
>%

====Secure sessions **[New in 1.1.0]**

For security reasons, Eliom does not use the same cookies in
        https and http. Secure sessions are using secure cookies
        (i.e. Ocsigen will ask the browsers to send the cookie only if
        the protocol is HTTPS). Thus it is not possible to access
        secure session if the user is using http. If the user is using
        https, Eliom will save data and services in secure session. But
        it is possible to access unsecure session data and to register
        unsecure session services using the optional parameter
        %<span class="code"|~secure:false>% when calling functions like
        %<span class="code"|Eliom_sessions.set_volatile_session_data>%,
        %<span class="code"|Eliom_sessions.get_persistent_session_data>%,
        %<span class="code"|Eliom_predefmod.Xhtml.register_for_session>%, etc.

====Non localized parameters**[New in 1.3.0]**

Non localized parameters are GET or POST parameters that are not
        taken into account by Eliom for choosing the service.
        They have a special prefix.
        Use this if you want some information to be available or not, through
        parameters, for all of your services.

  *wiki*)
let my_nl_params = 
  Eliom_parameters.make_non_localized_parameters
    ~prefix:"tutoeliom"
    ~name:"mynlparams"
    (Eliom_parameters.int "a" ** Eliom_parameters.string "s")

let nlparams = register_new_service
    ~path:["nlparams"]
    ~get_params:(int "i")
    (fun sp i () ->
       Lwt.return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata "i = ";
                      strong [pcdata (string_of_int i)]];
                   (match Eliom_parameters.get_non_localized_get_parameters
                      sp my_nl_params 
                    with
                      | None -> 
                          p [pcdata "I do not have my non localized parameters"]
                      | Some (a, s) -> 
                          p [pcdata "I have my non localized parameters, ";
                             pcdata ("with values a = "^string_of_int a^
                                       " and s = "^s^".")]
                   )]))

    )
  (*wiki*

          
        To create a link or a form with non-localized parameters,
        use the optional parameter %<span class="code"|nl_params>% of functions
    %<ocsigendoc version="dev" file="Eliom_predefmod.XHTMLFORMSSIG.html" fragment="VALa"|%<span class="code"|a>%>%,
    %<ocsigendoc version="dev" file="Eliom_predefmod.XHTMLFORMSSIG.html" fragment="VALget_form"|%<span class="code"|get_form>%>% or
    %<ocsigendoc version="dev" file="Eliom_predefmod.XHTMLFORMSSIG.html" fragment="VALpost_form"|%<span class="code"|post_form>%>%. Example:
    
    *wiki*)

let tonlparams = register_new_service
    ~path:["nlparams"]
    ~get_params:unit
    (fun sp i () ->
       Lwt.return
         (html
            (head (title (pcdata "")) [])
            (body
               [p [a ~service:nlparams ~sp [pcdata "without nl params"] 4];
                p [a ~service:nlparams ~sp 
                     ~nl_params:(Eliom_parameters.add_nl_parameter
                                   Eliom_parameters.empty_nl_params_set
                                   my_nl_params
                                   (22, "oh")
                                )
                     [pcdata "with nl params"] 
                     5];
                get_form
                  ~service:nlparams ~sp 
                  ~nl_params:(Eliom_parameters.add_nl_parameter
                                Eliom_parameters.empty_nl_params_set
                                my_nl_params
                                (22, "oh")
                             )
                  (fun iname ->
                     [p [pcdata "form with hidden nl params";
                         Eliom_predefmod.Xhtml.int_input 
                           ~input_type:`Text ~name:iname ();
                         Eliom_predefmod.Xhtml.string_input
                           ~input_type:`Submit ~value:"Send" ()]]);
                get_form
                  ~service:nlparams ~sp 
                  (fun iname ->
                     let (aname, sname) = 
                       Eliom_parameters.get_nl_params_names my_nl_params
                     in
                     [p [pcdata "form with nl params fiels";
                         Eliom_predefmod.Xhtml.int_input 
                           ~input_type:`Text ~name:iname ();
                         Eliom_predefmod.Xhtml.int_input 
                           ~input_type:`Text ~name:aname ();
                         Eliom_predefmod.Xhtml.string_input 
                           ~input_type:`Text ~name:sname ();
                         Eliom_predefmod.Xhtml.string_input
                           ~input_type:`Submit ~value:"Send" ()]]);
               ]))
    )
    

  (*wiki*

          
    It is also possible to 
    create a new service by adding the non localized parameters
        to an existing service:
      
  *wiki*)
let nlparams_with_nlp =
  Eliom_services.add_non_localized_get_parameters my_nl_params nlparams
  (*wiki*
          
Then create your link as usual, for example:
      %<span class="code"|a nlparams_with_nlp
             sp [pcdata "Try it"] (22, (11, "aa"))>%.
    [[site:tuto/nlparams?i=22&__nl_n_tutoeliom-mynlparams.s=aa&__nl_n_tutoeliom-mynlparams.a=11|Try it]].
          
    
        >%



===@@id="p3sessiongroups"@@[New in 0.99.5] Session groups
        
        %<div class="onecol"|
          
The idea is complementary to that of
the "session name".  While the
optional %<span class="code"|session_name>% parameter allows for a single session to have
multiple buckets of data associated with it, a session_group parameter
(also optional) allow multiple sessions to be referenced together.
For most uses, the session group is the user name.
It allows to implement features like "close all sessions" for one user
(even those opened on other browsers), or to limit the number of sessions
one user may open at the same time.
    
          

          
Session groups have been suggested by Dario Teixeira and
    introduced in Eliom 0.99.5. Dario explains:
    //Consider the following scenario: a user logs in from home using
  a "Remember me on this computer" feature, which sets a (almost)
  no-expiration cookie on his browser and session timeouts of infinity
  on the server.  The user goes on vacation, and while logging from
  a cyber-café, she also sets the "Remember me" option.  Back home
  she realises her mistake, and wishes to do a "global logout", ie,
  closing all existing sessions associated with her user name.
  //

**It is highly recommended to use session groups!
If you do not use them, the number of session is limitated by IP address,
which can be a problem for example if the server is behind a reverse proxy.**
    
  *wiki*)
(************************************************************)
(************ Connection of users, version 5 ****************)
(************************************************************)

(*zap* *)
let session_name = "connect_example5"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example5 =
  Eliom_services.new_service
    ~path:["groups"]
    ~get_params:Eliom_parameters.unit
    ()

let connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect5"
    ~post_params:(Eliom_parameters.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let disconnect_action =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"disconnect5"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp ())


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box sp =
  Eliom_predefmod.Xhtml.post_form connect_action sp
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Eliom_predefmod.Xhtml.string_input
                     ~input_type:`Text ~name:loginname ()]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "connect_example5" service (main page):    *)

let connect_example5_handler sp () () =
  let sessdat = Eliom_sessions.get_volatile_data_session_group (*zap* *) ~session_name (* *zap*) ~sp () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired
          | Eliom_sessions.No_data -> [login_box sp]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  Eliom_sessions.set_volatile_data_session_group ~set_max:4 (*zap* *) ~session_name (* *zap*) ~sp login;
  return ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register ~service:connect_example5 connect_example5_handler;
  Eliom_predefmod.Action.register ~service:connect_action connect_action_handler
(*wiki*
      Note that in this case, we do not need a session table any more,
      because our session table was containing only the user name,
      and the user name is now the session group.
      (But if we need to save more data, we still need a session table).

      As we will see later, there are three kinds of sessions
      (services, volatile data and persistent data).
      It is highly recommended to set a group for each of them!
    
          


    
        >%




===@@id="p3csrf"@@[New in 1.3.0]CSRF-safe services

Eliom implements a protection against CSRF attacks.

====What is CSRF?====

CSRF means //Cross Site Request Forgery//.
Here is an explanation from Wikipedia:

For example, one user, Bob, might be browsing a chat forum where another user, 
Mallory, has posted a message. Suppose that Mallory has crafted an HTML image 
element that references a script on Bob's bank's website (rather than an image 
file), e.g.,
{{{<img src="http://bank.example/withdraw?account=bob&amount=1000000&for=mallory">}}}
If Bob's bank keeps his authentication information in a cookie, and if the 
cookie hasn't expired, then the attempt by Bob's browser to load the image 
will submit the withdrawal form with his cookie, thus authorizing a 
transaction without Bob's approval.

====Solution with Eliom <= 1.2====

There is an easy way to protect a service from such attacks with Eliom 1.2:
just create a new anonymous coservice with timeout each time you display the 
form. Thus, a new token will be created for each form and no service will
answer if you do not send it (or more precisely the fallback will do).


====CSRF-safe services====

In order to simplify this, Eliom add this possibility:
*When creating a new coservice, you can give the optional ~csrf_safe 
parameter
*If this parameter is true, actual registration of the service will be 
delayed and performed each time a form is created towards this coservice

Example:
    *wiki*)

let csrfsafe_example =
  Eliom_services.new_service
    ~path:["csrf"]
    ~get_params:Eliom_parameters.unit
    ()

let csrfsafe_example_post =
  Eliom_services.new_post_coservice
    ~csrf_safe:true
    ~csrf_session_name:"csrf"
    ~csrf_secure_session:true
    ~timeout:10.
    ~max_use:1
    ~https:true
    ~fallback:csrfsafe_example
    ~post_params:Eliom_parameters.unit
    ()

let _ =
  let page sp () () =
    let l3 = Eliom_predefmod.Xhtml.post_form csrfsafe_example_post sp
        (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"Click" ()]]) ()
    in
    return
      (html
       (head (title (pcdata "CSRF safe service example")) [])
       (body [p [pcdata "A new coservice will be created each time this form is displayed"];
              l3]))
  in
  Eliom_predefmod.Xhtml.register csrfsafe_example page;
  Eliom_predefmod.Xhtml.register csrfsafe_example_post
    (fun sp () () ->
       Lwt.return
         (html
            (head (title (pcdata "CSRF safe service")) [])
            (body [p [pcdata "This is a CSRF safe service"]])))

(*wiki*


     If you register in the global service table, 
     the CSRF safe service will be available for everybody.
     But the actual (delayed) registration will take place in a session table,
     described by {{{?csrf_session_name}}} and {{{?csrf_secure_session}}}
     (corresponding to {{{?session_name}}} and {{{?secure}}}).

     If you use {{{register_for_session}}}, 
     the coservice will be available only for one session.
     The actual registration will take place in the same session table,
     described by {{{?csrf_session_name}}} and {{{?csrf_secure_session}}}.
     In that case, the parameters 
     {{?session_name}}} and {{{?secure}}} of {{{register_for_session}}}
     must be exactly the same.



===@@id="p3advancedformsandparameters"@@Advanced forms and parameters
        

        %<div class="onecol"|
          
This section shows more advanced use of page parameters and
      corresponding forms.
          

          ====Parsing parameters using regular expressions
          
          
        Eliom_parameters.regexp allows to parse page parameters using (Perl-compatible)
        regular expressions. We use the module %<span class="code"|Netstring_pcre>%,
        from //OCamlnet//. See the documentation about OCamlnet
        for more information.
        The following example shows a service that accepts only parameters
        values enclosed between %<span class="code"|[>% and %<span class="code"|]>%:
      
          

          
%<code language="ocaml"|
let r = Netstring_pcre.regexp "\\\\[(.*)\\\\]"

let regexp =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["regexp"]
    ~get_params:(regexp r "$1" "myparam")
    (fun _ g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata g]])))

>%

*wiki*)
(*zap* *)
let myregexp = Netstring_pcre.regexp "\\[(.*)\\]"

let regexpserv =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["regexp"]
    ~get_params:(regexp myregexp "$1" (fun s -> s) "myparam")
    (fun _ g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata g]])))
(* *zap*)
(*wiki*
[[site:tuto/regexp?myparam=%5Btoto%5D| Try it]].
          


          ====Boolean checkboxes
          
          
Page may take parameter of type %<span class="code"|bool>%.
         A possible use of this type is in a form
         with //boolean checkboxes//, as in the example below:
      
*wiki*)
(* Form with bool checkbox: *)
let bool_params = register_new_service
    ~path:["bool"]
    ~get_params:(bool "case")
  (fun _ case () ->
    return
    << <html>
         <head><title></title></head>
         <body>
         <p>
           $pcdata (if case then "checked" else "not checked")$
         </p>
         </body>
       </html> >>)

let create_form_bool casename =
    <:xmllist< <p>check? $bool_checkbox ~name:casename ()$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let form_bool = register_new_service ["formbool"] unit
  (fun sp () () ->
     let f = get_form bool_params sp create_form_bool in
     return
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)


(*wiki*
          
[[site:tuto/formbool| Try it]].
          


          
//Important warning://
        As you can see, browsers do not send any value
        for unchecked boxes! An unchecked box is equivalent to no parameter
        at all! Thus it is not possible to distinguish between a service
        taking a boolean and a service taking no parameter at all
        (if they share the same URL).
        In Eliom //services are tried in order of registration!//
        The first matching service will answer.
      
          


          
Other types similar to bool:
          

          
*%<ocsigendoc version="dev" file="Eliom_parameters.html" fragment="VALopt"|%<span class="code"|Eliom_parameters.opt>%>% (page taking an optional parameter),
*%<ocsigendoc version="dev" file="Eliom_parameters.html" fragment="VALsum"|%<span class="code"|Eliom_parameters.sum>%>% (either a parameter or another).
                  
          

          
        See the interface
        %<ocsigendoc version="dev" file="Eliom_parameters.html"|here>%.
      
          


          ====Type %<span class="code"|set>%
          
          
Page may take several parameters of the same name.
      It is useful when you want to create a form with a variable number
      of fields.
      To do that with Eliom, use the type %<ocsigendoc version="dev" file="Eliom_parameters.html" fragment="VALset"|%<span class="code"|Eliom_parameters.set>%>%.
      For example %<span class="code"|set int "val">% means that the page will take
      zero, one or several parameters of name %<span class="code"|"val">%,
      all of type %<span class="code"|int>%.
      The function you register will receive the parameters in a list.
      Example:
      
*wiki*)

let set = register_new_service
    ~path:["set"]
    ~get_params:(set string "s")
  (fun _ l () ->
    let ll =
      List.map
        (fun s -> << <strong>$str:s$ </strong> >>) l
    in
    return
    << <html>
         <head><title></title></head>
         <body>
         <p>
           You sent:
           $list:ll$
         </p>
         </body>
       </html> >>)
(*wiki*
          
These parameters may come from several kinds of widgets in forms.
   Here is an example of a form with several checkboxes, all sharing the same
   name, but with different values:
   
*wiki*)

(* form to set *)
let setform = register_new_service
    ~path:["setform"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Set Form"];
                  get_form set sp
                    (fun n ->
                      [p [pcdata "Form to set: ";
                          string_checkbox ~name:n ~value:"box1" ();
                          string_checkbox
                            ~name:n ~value:"box2" ~checked:true ();
                          string_checkbox ~name:n ~value:"box3" ();
                          string_checkbox ~name:n ~value:"box4" ();
                          string_input ~input_type:`Submit ~value:"Click" ()]])
                ])))
(*wiki*
          
[[site:tuto/setform| Try it]].
          


          
Once again, note that there is no difference between an empty
      set or no parameter at all. If you register a service without parameters
      and a service with a set of parameters on the same URL, the firstly
      registered service that matches will answer.
      
          

          ====Select
          
          
Here is an example of a select box.
          
*wiki*)
let select_example_result = register_new_service
    ~path:["select"]
    ~get_params:(string "s")
    (fun sp g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata "You selected: ";
                     strong [pcdata g]]])))

let create_select_form =
  (fun select_name ->
    [p [pcdata "Select something: ";
        Eliom_predefmod.Xhtml.string_select ~name:select_name
          (Eliom_predefmod.Xhtml.Option ([] (* attributes *),
                                        "Bob" (* value *),
                                        None (* Content, if different from value *),
                                        false (* not selected *))) (* first line *)
          [Eliom_predefmod.Xhtml.Option ([], "Marc", None, false);
          (Eliom_predefmod.Xhtml.Optgroup
          ([],
           "Girls",
           ([], "Karin", None, false),
           [([a_disabled `Disabled], "Juliette", None, false);
            ([], "Alice", None, true);
            ([], "Germaine", Some (pcdata "Bob's mother"), false)]))]
          ;
        Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Send" ()]])

let select_example = register_new_service ["select"] unit
  (fun sp () () ->
     let f =
       Eliom_predefmod.Xhtml.get_form
         select_example_result sp create_select_form
     in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))
(*wiki*
          
[[site:tuto/select| Try it]].
          

          
To do "multiple" select boxes, use functions like
   %<ocsigendoc version="dev" file="Eliom_predefmod.XHTMLFORMSSIG.html" fragment="VALstring_multiple_select"|%<span class="code"|Eliom_predefmod.Xhtml.string_multiple_select>%>%.
   As you can see in the type, the service must be declared with parameters
   of type %<ocsigendoc version="dev" file="Eliom_parameters.html" fragment="VALset"|%<span class="code"|set>%>%.
     
          





          ====Clickable images
          
          
Here is an example of clickable image.
      You receive the coordinates the user clicked on.
      
*wiki*)
let coord = register_new_service
    ~path:["coord"]
    ~get_params:(coordinates "coord")
  (fun _ c () ->
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You clicked on coordinates:
         ($str:(string_of_int c.abscissa)$, $str:(string_of_int c.ordinate)$)
       </p>
       </body>
     </html> >>)

(* form to image *)
let imageform = register_new_service
    ~path:["imageform"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Image Form"];
                  get_form coord sp
                    (fun n ->
                      [p [image_input
                            ~src:(make_uri ~service:(static_dir sp) ~sp ["ocsigen5.png"])
                            ~name:n
                            ()]])
                ])))
(*wiki*
          
[[site:tuto/imageform| Try it]].
          

          
You may also send a value with the coordinates:
          
*wiki*)
let coord2 = register_new_service
    ~path:["coord2"]
    ~get_params:(int_coordinates "coord")
  (fun _ (i, c) () ->
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You clicked on coordinates:
         ($str:(string_of_int c.abscissa)$, $str:(string_of_int c.ordinate)$)
       </p>
       </body>
     </html> >>)

(* form to image *)
let imageform2 = register_new_service
    ~path:["imageform2"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Image Form"];
                  get_form coord2 sp
                    (fun n ->
                      [p [int_image_input
                            ~src:(make_uri ~service:(static_dir sp) ~sp ["ocsigen5.png"])
                            ~name:n
                            ~value:3
                            ()]])
                ])))

(*wiki*
          
[[site:tuto/imageform2| Try it]].
          



          ====Type %<span class="code"|list>%
          
          
Another way (than %<ocsigendoc version="dev" file="Eliom_parameters.html" fragment="VALset"|%<span class="code"|Eliom_parameters.set>%>%) to do variable length forms
        is to use indexed lists (using %<ocsigendoc version="dev" file="Eliom_parameters.html" fragment="VALlist"|%<span class="code"|Eliom_parameters.list>%>%).
        The use of that feature is a bit more complex than %<span class="code"|set>%
        and still experimental.
        Here is an example of service taking an indexed list as parameter:
        
*wiki*)

(* lists *)
let coucou_list = register_new_service
    ~path:["coucou"]
    ~get_params:(list "a" (string "str"))
  (fun _ l () ->
    let ll =
      List.map (fun s -> << <strong>$str:s$</strong> >>) l in
      return
        << <html>
             <head><title></title></head>
             <body>
             <p>
               You sent:
               $list:ll$
             </p>
             </body>
           </html> >>)
(*wiki*
          
   Here is an example of link towards this service:
   [[site:tuto/coucou?a.str%5B1%5D=titi&a.str%5B0%5D=toto|coucou?a.str[0]=toto&a.str[1]=titi]].
      
          

          
//Warning://
   As for sets or bools,
   if a request has no parameter, it will be considered as the empty list.
   Services are tried in order of registration.
   
          


          
   As you see, the names of each list element is built from the name
   of the list, the name of the list element, and an index.
   To spare you creating yourself these names, Eliom provides you an iterator
   to create them.
   
*wiki*)
(*zap* Note:
   Actually almost all services will be overwritten by new versions,
   but not those with user_type parameters for example
   (because the type description contains functions)
 *zap*)

(* Form with list: *)
let create_listform f =
  (* Here, f.it is an iterator like List.map,
     but it must be applied to a function taking 2 arguments
     (unlike 1 in map), the first one being the name of the parameter,
     and the second one the element of list.
     The last parameter of f.it is the code that must be appended at the
     end of the list created
   *)
  f.it (fun stringname v init ->
    <:xmllist< <p>Write the value for $str:v$:
      $string_input ~input_type:`Text ~name:stringname ()$ </p> >>@init)
    ["one";"two";"three";"four"]
    <:xmllist< <p>$string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let listform = register_new_service ["listform"] unit
  (fun sp () () ->
     let f = get_form coucou_list sp create_listform in
     return
      << <html>
           <head><title></title></head>
           <body> $f$ </body>
         </html> >>)

(*wiki*
          
[[site:tuto/listform| Try it]].
          


          
//Important warning://
      As we have seen in the section about boolean (or optional)
      parameters, it is not possible to distinguish between a boolean
      with value "false", and no parameter at all.
      This causes problems if you create a list of boolean or optional
      values, as it is not possible to know the length of the list.
      In that case, Eliom always takes the shortest possible list.
      
          


          ====Forms and suffixes
          

          
Service with "suffix" URLs have an equivalent version with
      usual parameters, allowing to create forms towards such services.
      Example:
      
*wiki*)
(* Form for service with suffix: *)
let create_suffixform ((suff, endsuff),i) =
    <:xmllist< <p>Write the suffix:
      $int_input ~input_type:`Text ~name:suff ()$ <br/>
      Write a string: $user_type_input
      (Ocsigen_lib.string_of_url_path ~encode:false)
         ~input_type:`Text ~name:endsuff ()
         $ <br/>
      Write an int: $int_input ~input_type:`Text ~name:i ()$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let suffixform = register_new_service ["suffixform"] unit
  (fun sp () () ->
     let f = get_form isuffix sp create_suffixform in
     return
      << <html>
           <head><title></title></head>
           <body> $f$ </body>
         </html> >>)

(*wiki*
          
[[site:tuto/suffixform| Try it]].
          


          ====Uploading files
          

          
The %<ocsigendoc version="dev" file="Eliom_parameters.html" fragment="VALfile"|%<span class="code"|Eliom_parameters.file>%>% parameter type allows to send files in your
       request. The service gets something of type
       %<ocsigendoc version="dev" file="Ocsigen_extensions.html" fragment="TYPEfile_info"|%<span class="code"|Ocsigen_extensions.file_info>%>%. You can extract information
       using this using these functions (from %<ocsigendoc version="dev" file="Eliom_sessions.html"|%<span class="code"|Eliom_sessions>%>%):
      
          

          
%<code language="ocaml"|
val get_tmp_filename : Ocsigen_extensions.file_info -> string
val get_filesize : Ocsigen_extensions.file_info -> int64
val get_original_filename : Ocsigen_extensions.file_info -> string

>%

          
%<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_tmp_filename"|%<span class="code"|Eliom_sessions.get_tmp_filename>%>% allows to know the actual name
       of the uploaded file on the hard drive.
        %<ocsigendoc version="dev" file="Eliom_sessions.html" fragment="VALget_original_filename"|%<span class="code"|Eliom_sessions.get_original_filename>%>% gives the original filename.
          

          
To make possible the upload of files, you must configure a
      directory for uploaded files in Ocsigen's configuration file.
      For example:
      
          

          
%<div class="pre"|
  <uploaddir>/tmp</uploaddir>
>%

          
Files are kept in this directory only during the request.
       Then they are automatically cancelled.
       Thus your services must copy them
       somewhere else themselves if they want to keep them.
       In the following example, we create a new hard link to the file
       to keep it (the destination must be on the same partition of the disk).
      
          
*wiki*)
let upload = new_service
    ~path:["upload"]
    ~get_params:unit
    ()

let upload2 = register_new_post_service
   ~fallback:upload
   ~post_params:(file "file")
    (fun _ () file ->
      let to_display =
        let newname = "/tmp/thefile" in
        (try
          Unix.unlink newname;
        with _ -> ());
        Ocsigen_messages.console2 (Eliom_sessions.get_tmp_filename file);
        Unix.link (Eliom_sessions.get_tmp_filename file) newname;
        let fd_in = open_in newname in
        try
          let line = input_line fd_in in close_in fd_in; line (*end*)
        with End_of_file -> close_in fd_in; "vide"
      in
      return
        (html
           (head (title (pcdata "Upload")) [])
           (body [h1 [pcdata to_display]])))


let uploadform = register upload
    (fun sp () () ->
      let f =
        (post_form upload2 sp
           (fun file ->
             [p [file_input ~name:file ();
                 br ();
                 string_input ~input_type:`Submit ~value:"Send" ()
               ]]) ()) in
      return
        (html
           (head (title (pcdata "form")) [])
           (body [f])))






$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ *)




















(*zap* *)
open Tutoeliom

(* Main page for this example *)
let main = new_service [] unit ()

let _ = Eliom_predefmod.Xhtmlcompact.register main
  (fun sp () () ->
    Lwt.return
     (html
       (head
          (title (pcdata "Eliom examples"))
          [css_link (make_uri ~service:(static_dir sp) ~sp ["style.css"]) ()])
       (body
          [
            h1 [img ~alt:"Ocsigen" ~src:(Eliom_predefmod.Xhtml.make_uri ~service:(static_dir sp) ~sp ["ocsigen5.png"]) ()];

            h3 [pcdata "Eliom examples"];
            h4 [pcdata "Simple pages"];
            p [
              pcdata "A simple page: ";
              a coucou sp [code [pcdata "coucou"]] ();
              br ();

              pcdata "A page with a counter: ";
              a count sp [code [pcdata "count"]] ();
              br ();


              pcdata "A page in a directory: ";
              a hello sp [code [pcdata "dir/hello"]] ();
              br ();


              pcdata "Default page of a directory: ";
              a default sp [code [pcdata "rep/"]] ()
            ];

            h4 [pcdata "Parameters"];
            p [
              pcdata "A page with GET parameters: ";
              a coucou_params sp [code [pcdata "coucou"]; pcdata " with params"] (45,(22,"krokodile"));
              pcdata "(what if the first parameter is not an integer?)";
              br ();

              pcdata "A page with \"suffix\" URL that knows the IP and user-agent of the client: ";
              a uasuffix sp [code [pcdata "uasuffix"]] (2007,6);
              br ();


              pcdata "A page with \"suffix\" URL and GET parameters: ";
              a isuffix sp [code [pcdata "isuffix"]] ((111, ["OO";"II";"OO"]), 333);
              br ();

              pcdata "A page with constants in suffix: ";
              a constfix sp [pcdata "Page with constants in suffix"] ("aa", ((), "bb"));
              br ();

              pcdata "Form towards page with suffix: ";
              a suffixform sp [pcdata "formsuffix"] ();
              br ();

              pcdata "A page with a parameter of user-defined type : ";
              a mytype sp [code [pcdata "mytype"]] A;
            ];

            h4 [pcdata "Links and Forms"];
            p [
              pcdata "A page with links: ";
              a links sp [code [pcdata "links"]]  ();
              br ();


              pcdata "A page with a link towards itself: ";
              a linkrec sp [code [pcdata "linkrec"]] ();
              br ();


              pcdata "The ";
              a main sp [pcdata "default page"] ();
              pcdata "of this directory (myself)";
              br ();

              pcdata "A page with a GET form that leads to the \"coucou\" page with parameters: ";
              a form sp [code [pcdata "form"]] ();
              br ();


              pcdata "A POST form towards the \"post\" page: ";
              a form2 sp [code [pcdata "form2"]] ();
              br ();


              pcdata "The \"post\" page, when it does not receive parameters: ";
              a no_post_param_service sp [code [pcdata "post"]; pcdata " without post_params"] ();
              br ();


              pcdata "A POST form towards a service with GET parameters: ";
              a form3 sp [code [pcdata "form3"]] ();
              br ();


              pcdata "A POST form towards an external page: ";
              a form4 sp [code [pcdata "form4"]] ();
            ];

            h4 [pcdata "Sessions"];
            p [
              pcdata "Coservices: ";
              a coservices_example sp [code [pcdata "coservice"]] ();
              br ();


              pcdata "A session based on cookies, implemented with session data: ";
              a session_data_example sp [code [pcdata "sessdata"]] ();
              br ();


              pcdata "A session based on cookies, implemented with actions: ";
              a connect_example3 sp [code [pcdata "actions"]] ();
              br ();


              pcdata "A session based on cookies, with session services: ";
              a session_services_example sp [code [pcdata "sessionservices"]] ();
              br ();

              pcdata "A session based on cookies, implemented with actions, with session groups: ";
              a connect_example5 sp [code [pcdata "groups"]] ();
              br ();


              pcdata "The same with wrong user if not \"toto\": ";
              a connect_example6 sp [code [pcdata "actions2"]] ();
              br ();


              pcdata "Coservices in the session table: ";
              a calc sp [code [pcdata "calc"]] ();
              br ();


              pcdata "Persistent sessions: ";
              a persist_session_example sp [code [pcdata "persist"]] ();
              br ();
            ];

            h4 [pcdata "Other"];
            p [
              pcdata "A page that is very slow, implemented in cooperative way: ";
              a looong sp [code [pcdata "looong"]] ();
              br ();


              pcdata "A page that is very slow, using preemptive threads: ";
              a looong sp [code [pcdata "looong2"]] ();
              br ();


              pcdata "Catching errors: ";
              a catch sp [code [pcdata "catch"]] 22;
              pcdata "(change the value in the URL)";
              br ();

              pcdata "Redirection: ";
              a redir sp [code [pcdata "redir"]] 11;
              br ();

              pcdata "Cookies: ";
              a cookies sp [code [pcdata "cookies"]] ();
              br ();


              pcdata "Disposable coservices: ";
              a disposable sp [code [pcdata "disposable"]] ();
              br ();

              pcdata "Coservice with timeout: ";
              a timeout sp [code [pcdata "timeout"]] ();
              br ();

              pcdata "Public coservice created after initialization (with timeout): ";
              a publiccoduringsess sp [code [pcdata "publiccoduringsess"]] ();
              br ();


              pcdata "The following URL send either a statically checked page, or a text page: ";
              a send_any sp [code [pcdata "send_any"]] "valid";
              br ();


              pcdata "A page with a persistent counter: ";
              a count2 sp [code [pcdata "count2"]] ();
              br ();

              a hier1 sp [pcdata "Hierarchical menu"] ();
              br ();

              a divpage sp [code [pcdata "a link sending a &lt;div&gt; page"]] ();
              br ();

              a tonlparams sp [pcdata "Non localized parameters"] ();
              br ();

              a nlparams sp [pcdata "Non localized parameters (absent)"] 4;
              br ();

              a nlparams_with_nlp sp [pcdata "Non localized parameters (present)"] (22, (11, "aa"));
              br ();

              a csrfsafe_example sp [pcdata "CSRF safe services"] ();
              br ();
            ];

            h4 [pcdata "Advanced forms"];
            p [
              pcdata "A page that parses a parameter using a regular expression: ";
              a regexpserv sp [code [pcdata "regexpserv"]] "[toto]";
              br ();

              pcdata "A form with a checkbox: ";
              a form_bool sp [pcdata "Try it"] ();
              br ();

              pcdata "A page that takes a set of parameters: ";
              a set sp [code [pcdata "set"]] ["Ciao";"bello";"ciao"];
              br ();

              pcdata "A form to the previous one: ";
              a setform sp [code [pcdata "setform"]] ();
              br ();

              pcdata "A page that takes any parameter: ";
              a raw_serv sp [code [pcdata "raw_serv"]] [("a","hello"); ("b","ciao")];
              br ();

              pcdata "A form to the previous one: ";
              a raw_form sp [code [pcdata "raw_form"]] ();
              br ();

              pcdata "A form for a list of parameters: ";
              a listform sp [pcdata "Try it"] ();
              br ();
            ];

            h3 [pcdata "Eliom Client"];
            p [
              a eliomclient1 sp [pcdata "Simple example of client side code"] ();
              br ();

              a eliomclient2 sp [pcdata "Using Eliom services in client side code"] ();
            br ();
              a eliomclient3 sp [pcdata "Caml values in service parameters"] ();
            br ();
              a eliomclient4 sp [pcdata "A service sending a Caml value"] ();
            br ();
              a gotowithoutclient sp [pcdata "A page that links to a service that belongs to the application but do not launch the application if it is already launched"] ();
            br ();
              a comet1 sp [pcdata "A really simple comet example"] ();
            br ();
              a comet2 sp [pcdata "A comet example with server to client and client to server asynchronous events"] ();
            br ();
              a comet3 sp [pcdata "Server simultaneous events, transmitted together"] ();
            br ();
              a comet_message_board sp [pcdata "Minimalistic message board"] ();
            br ();
          ];

            h4 [pcdata "Tab sessions"];
            p [
(*              pcdata "Coservices: ";
              a coservices_example sp [code [pcdata "coservice"]] ();
              br ();
*)

              pcdata "A session based on cookies, implemented with session data: ";
              a tsession_data_example sp [code [pcdata "tsessdata"]] ();
              br ();
(*

              pcdata "A session based on cookies, implemented with actions: ";
              a connect_example3 sp [code [pcdata "actions"]] ();
              br ();


              pcdata "A session based on cookies, with session services: ";
              a session_services_example sp [code [pcdata "sessionservices"]] ();
              br ();

              pcdata "A session based on cookies, implemented with actions, with session groups: ";
              a connect_example5 sp [code [pcdata "groups"]] ();
              br ();


              pcdata "The same with wrong user if not \"toto\": ";
              a connect_example6 sp [code [pcdata "actions2"]] ();
              br ();


              pcdata "Coservices in the session table: ";
              a calc sp [code [pcdata "calc"]] ();
              br ();


              pcdata "Persistent sessions: ";
              a persist_session_example sp [code [pcdata "persist"]] ();
              br ();
*)
            ];

          ]
       )))
;;

(* *zap*)

