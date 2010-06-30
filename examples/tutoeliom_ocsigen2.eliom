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
                        (* with {{ expr }}, the expression is executed by the
                         * client. *)
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
}}

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
                    ~sp:\sp:sp (* Here [sp] is sent by the server *)
                    ~service:\w:Tutoeliom.coucou (* just as [coucou] *)
                    () ()
                }}
            ]
            [pcdata "Click here to go to another page."];

(*wiki*
To use server values inside client code one should use the syntax {{{\w:e}}}
where {{{w}}} is the wrapper keyword and {{{e}}} the sent expression. Note that
{{{e}}} is evaluated by the server and the resulting value is send to the
client.
*wiki*)

(*zap* 
(*wiki*
  The following examples shows how to do a request to a service,
  and use the content:
*wiki*)
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
 *zap*)
             
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
                  ~sp:\sp:sp
                  ~service:\w:Tutoeliom.coucou
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
                    ~sp:\sp:sp
                    ~service:\w:eliomclient1
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
                Eliom_client.change_page ~sp:\sp:sp ~service:\w:Tutoeliom.coucou
                  () ()
              }}
            ]
            [pcdata "Click here to go to a page outside the application, using ";
             code [pcdata "change_page"];
             pcdata "."];
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.exit_to ~sp:\sp:sp ~service:\w:eliomclient2 () ()
              }}
            ]
            [pcdata "Click here to relaunch the program by reloading the page."];
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick {{
                Eliom_client.change_page ~sp:\sp:sp ~service:\w:eliomclient1
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
                Eliom_client.get_subpage
                  ~sp:\sp:sp
                  ~service:\w:eliomclient1
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
                                   \node:container (* node is the wrapper keyword for XHTML.M nodes. *)
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
                     (Js.string (string_of_float \w:my_value)) ;
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
                            ~sp:\sp:sp ~service:\w:Tutoeliom.coucou
                            [pcdata "An external link generated client side"]
                            ();
                          pcdata " and ";
                          Eliom_predefmod.Xhtml.a
                            (*zap* *)~a:[a_class ["clickable"]](* *zap*)
                            ~sp:\sp:sp ~service:\w:eliomclient1
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
                     ~sp:\sp:sp ~service:\w:eliomclient3'
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
                     ~sp:\sp:sp ~service:\w:eliomclient4'
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
                  ~sp:\sp:sp ~service:\w:gotowithoutclient
                  () ()
              }}
            ]
            [pcdata "Click here to go to a page that launches the application every time."];
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
                  ~sp:\sp:sp ~service:\w:withoutclient
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

(* create a communication channel *)
let (c1, write_c1) =
  let (e, push_e) = React.E.create () in
  (Eliom_comet.Channels.create e, push_e)

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
       let (c2, write_c2) =
         let (e, push_e) = React.E.create () in
           (Eliom_comet.Buffered_channels.create ~max_size:20 e, push_e)
       in
       let t2 = ref 0 in
       let rec tick_2 () =
         Lwt_unix.sleep (float_of_int (6 + (Random.int 6))) >>= fun () ->
         write_c2 !t2 ; incr t2 ; Lwt.pause () >>= fun () ->
         write_c2 !t2 ; incr t2 ; write_c2 !t2 ; incr t2 ; tick_2 ()
       in
       let _ = tick_2 () in
       Lwt.return
         [
           div
             [pcdata "To fully understand the meaning of the public channel, \
                      use a couple browsers on this page."] ;
           div
             ~a:[a_onclick{{
                   Eliom_client_comet.Channels.register \channel:c1
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
                   Eliom_client_comet.Buffered_channels.register \buffchan:c2
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
                  \down_event:e_down
           }}
              ]
           [pcdata "START"] ;
         div (* This div is for pushing "A" to the server side event *)
           (*TODO: fix client side sp and simplify up_event unwrapping *)
           ~a:[a_onclick {{ let sp = \sp:sp in \up_event:e_up "A" }} ]
           [pcdata "Push A"] ;
         div (* This one is for pushing "B" *)
           ~a:[a_onclick {{ let sp = \sp:sp in \up_event:e_up "B" }} ]
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
                     [ React.E.map string_of_int \down_event:e_down_1 ;
                       \down_event:e_down_2 ;
                     ]
                  )
           }}
              ]
           [pcdata "START"] ;
         div (*TODO: fix client side sp and simplify up_event unwrapping *)
           ~a:[
             (*zap* *)a_class ["clickable"];(* *zap*)
             a_onclick {{ let sp = \sp:sp in \up_event:e_up "" }} ]
           [pcdata "Send me two values from different events !"] ;
         div [pcdata "Note that one of the two events has a greater rate limit \
                      (using throttle control). Hence you might receive only \
                      one if you click with high frequency."] ;
       ]
    )



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
          ]
          ]
       )))
;;

(* *zap*)

