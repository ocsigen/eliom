(*zap* *)
let (>>=) = Lwt.bind


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
For now it is using O'Browser to run OCaml programs in the browser.
But Eliom will probably be available for other platforms soon.


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
open XHTML.M
(****** server only *******)
open.server Eliom_parameters
open.server Eliom_predefmod.Xhtmlcompact
open.server Eliom_services

(* for client side only : open.client *)

module.server Eliom_appl =
  Eliom_predefmod.Eliom_appl (
    struct
      let.server application_name = "tutoeliom_ocsigen2_client"
      let.server params =
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

let.server eliomobrowser1 =
  Eliom_appl.register_new_service
    ~path:["eliomobrowser1"]
    ~get_params:unit
    (fun sp () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick 
                 ((fun.client (() : unit) -> Js.alert "clicked!") ())]
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

The code will look like:
%<code language="ocaml"|
p ~onclick:{{Eliom_client.post_request ~sp ~service:myblockservice ()
                   >>= Node.append bodynode}}
        [pcdata "Click here to add content from the server."];
>%

For now, the syntax extension has not been implemented, thus the syntax
is somewhat more complicated. Here are some examples of what you can do:
*wiki*)
let.server eliomobrowser2 = new_service ~path:["eliomobrowser2"] ~get_params:unit ()

let.server myblockservice =
  Eliom_predefmod.Blocks.register_new_post_coservice
    ~fallback:eliomobrowser2
    ~post_params:unit
    (fun _ () () -> 
       Lwt.return
         [p [pcdata ("I come from a distant service! Here is a random value: "^
                       string_of_int (Random.int 100))]])

let item () = li [pcdata Sys.ocaml_version]

let.server _ =
  Eliom_appl.register
    eliomobrowser2
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
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.http) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.exit_to ~sp ~service () ()
                 ) (Eliom_client.wrap_sp sp) Tutoeliom.coucou)
            ]
            [pcdata "Click here to go to another page."];
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
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      let body = JSOO.eval "document.body" in
                      (*Js.get_element_by_id "bodyid"*)
                      Eliom_client.call_service
                        ~sp ~service:myblockservice () () >>= fun s ->
                      (try
                         let l = Js.Node.children (Js.dom_of_xml s) in
                         List.iter (Js.Node.append body) l
                       with e -> Js.alert (Printexc.to_string e));
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
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, 'ret) Eliom_services.service) ->
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.change_url ~sp ~service () ()
                 ) (Eliom_client.wrap_sp sp) Tutoeliom.coucou)
            ]
            [pcdata "Click here to change the URL."];
          
(*wiki*
  The following examples shows how to change the current page,
  without stopping the client side program.
*wiki*)
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.appl_service) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.change_page ~sp ~service () ()
                 ) (Eliom_client.wrap_sp sp) eliomobrowser1)
            ]
            [pcdata "Click here to change the page without stopping the program."];

(*wiki* Actually the usual {{{a}}} function to create link will
  use {{{change_page}}} if you do a link inside the same application.
  The latter example is equivalent to the following. *wiki*)
          p [a (*zap* *) ~a:[a_class ["clickable"]](* *zap*)
               ~sp
               ~service:eliomobrowser1
               [pcdata "Click here to change the page without stopping the program (with ";
                code [pcdata "a"];
                pcdata ")."]
               ()];
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.http) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.change_page ~sp ~service () ()
                 ) (Eliom_client.wrap_sp sp) Tutoeliom.coucou)
            ]
            [pcdata "Click here to go to a page outside the application, using ";
             code [pcdata "change_page"];
             pcdata "."];
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.appl_service) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.exit_to ~sp ~service () ()
                 ) (Eliom_client.wrap_sp sp) eliomobrowser2)
            ]
            [pcdata "Click here to relaunch the program the program by reloading the page."];
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick 
                ((fun.client
                    (absolute : 'ze12)
                    (absolute_path : 'ze11)
                    (https : 'ze10)
                    (service : 'ze8)
                    (sp : 'ze7)
                    (hostname : 'ze6)
                    (port : 'ze5)
                    (fragment : 'ze4)
                    (keep_nl_params : 'ze3)
                    (nl_params : 'ze2)
                    (getparams : 'ze1) ->
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.change_page ~sp ~service () ()
                 ) 
                   None
                   None
                   None
                   eliomobrowser1
                   (Eliom_client.wrap_sp sp) 
                   None
                   None
                   None
                   None
                   None
                    ())
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
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.appl_service) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.get_subpage ~sp ~service () () >>= fun blocks ->
                      List.iter (Js.Node.append Ocsigen_lib.body) (XHTML.M.toeltl blocks);
                      Lwt.return ()
                 ) (Eliom_client.wrap_sp sp) eliomobrowser1)
            ]
            [pcdata "Click here to get a subpage from server."];

             
(*wiki*
====Refering to parts of the page in client side code
*wiki*)

          (let container = ul (item ()) [ item () ; item ()] in
           div [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick 
                  ((fun.client (container : 'node Eliom_client_types.data_key) ->
                      let container = Eliom_obrowser.unwrap_node container in
                      let nl = XHTML.M.toelt (item ()) in
                      Js.Node.append container nl) 
                     (Eliom_client.wrap_node ~sp container))]
                  [pcdata "Click here to add an item below with the current version of OCaml."];
                container]);
          
(*wiki*
====Refering to server side data in client side code
  In the case you want to send some server side value with your page,
  just do:
*wiki*)

          (let my_value = 1.12345 in 
           p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick 
             ((fun.client (my_value : float Eliom_client_types.data_key) ->
                 Js.alert (string_of_float (Eliom_obrowser.unwrap my_value))) 
                (Eliom_client.wrap ~sp my_value))]
             [pcdata "Click here to see a server side value sent with the page."]);

(*wiki*
====Other tests
  *wiki*)

          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (s1 : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.http) Eliom_services.service)
                    (s2 : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.appl_service) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      (Js.Node.append
                         Ocsigen_lib.body
                         (XHTML.M.toelt (p [Eliom_predefmod.Xhtml.a ~sp ~service:s1
                                              [pcdata "An external link generated client side"] ();
                                            pcdata " and ";
                                            Eliom_predefmod.Xhtml.a (*zap* *)~a:[a_class ["clickable"]](* *zap*)~sp ~service:s2
                                              [pcdata "another, inside the application."] ()]))
                      );
                      Lwt.return ()
                 ) (Eliom_client.wrap_sp sp) Tutoeliom.coucou eliomobrowser1)
            ]
            [pcdata "Click here to add client side generated links."];


          
        ])
(*wiki*
====Using OCaml values as service parameters
It is now possible to send OCaml values to services.
To do that, use the {{{Eliom_parameters.caml}}} function:
*wiki*)
let.server eliomobrowser3' =
  Eliom_appl.register_new_post_coservice'
    ~post_params:(caml "isb")
    (fun sp () (i, s, l) ->
      Lwt.return
        [p (pcdata (Printf.sprintf "i = %d, s = %s" i s)::
              List.map (fun a -> pcdata a) l
           )])



let.server eliomobrowser3 =
  Eliom_appl.register_new_service
    ~path:["eliomobrowser3"]
    ~get_params:unit
    (fun sp () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick 
                 ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, (int * string * string list), 'c, 'd, 'e, 'f, 'g, Eliom_services.appl_service) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.change_page ~sp ~service
                       () (22, "oo", ["a";"b";"c"]))
                    (Eliom_client.wrap_sp sp) eliomobrowser3')]
           [pcdata "Click to send Ocaml data"]
        ])
(*wiki*
====Sending OCaml values using services
It is possible to do services that send any caml value. For example:
*wiki*)
let.server eliomobrowser4' =
  Eliom_predefmod.Caml.register_new_post_coservice'
    ~post_params:unit
    (fun sp () () -> Lwt.return [1; 2; 3])

let.server eliomobrowser4 =
  Eliom_appl.register_new_service
    ~path:["eliomobrowser4"]
    ~get_params:unit
    (fun sp () () ->
      Lwt.return
        [p ~a:[(*zap* *)a_class ["clickable"];(* *zap*)a_onclick 
                 ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, int list Eliom_parameters.caml) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      let body = JSOO.eval "document.body" in
                      Eliom_client.call_caml_service ~sp ~service () ()
                      >>= fun l ->
                      List.iter 
                        (fun i -> Js.Node.append body 
                           (Js.Node.text (string_of_int i)))
                        l;
                      Lwt.return ()
                  )
                    (Eliom_client.wrap_sp sp) eliomobrowser4')]
           [pcdata "Click to receive Ocaml data"]
        ])
(*wiki*
====Other tests:
*wiki*)
let.server withoutobrowser =
  Eliom_services.new_service
    ~path:["withoutobrowser"]
    ~get_params:unit
    ()

let.server gotowithoutobrowser =
  Eliom_services.new_service
    ~path:["gotowithoutobrowser"]
    ~get_params:unit
    ()


let.server _ =
  Eliom_appl.register
    ~options:true
    ~service:withoutobrowser
    (fun sp () () ->
       Lwt.return
         [p [pcdata "If the application was not launched before coming here (or if you reload), this page will not launch it. But if it was launched before, it is still running."];
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.appl_service) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.change_page ~sp ~service () ()
                 ) (Eliom_client.wrap_sp sp) gotowithoutobrowser)
            ]
            [pcdata "Click here to go to a page that launches the application every time."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~sp ~service:gotowithoutobrowser
               [pcdata "Same link with "; 
                code [pcdata "a"]; pcdata "."] ()];
         ]);
  Eliom_appl.register
    ~service:gotowithoutobrowser
    (fun sp () () ->
       Lwt.return
         [p [pcdata "The application is launched."];
          p 
            ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
              a_onclick 
                ((fun.client
                    (sp : Eliom_client_types.server_params Eliom_client_types.data_key)
                    (service : (unit, unit, 'c, 'd, 'e, 'f, 'g, Eliom_services.appl_service) Eliom_services.service) -> 
                      let sp = Eliom_obrowser.unwrap_sp sp in
                      Eliom_client.change_page ~sp ~service () ()
                 ) (Eliom_client.wrap_sp sp) withoutobrowser)
            ]
            [pcdata "Click here to see the page that does not launch the application."];
          p [a (*zap* *)~a:[a_class ["clickable"]](* *zap*) ~sp ~service:withoutobrowser
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
It's very low level right now but it should evolve...
 *wiki*)

(* client code : read the next value from a channel *)
let.client read_one_value_from_channel chan =
  Lwt.return (
    Js.http_post
      "./"
      "x-ocsigen-comet" (* content-type *)
      chan
  )

(* client code : read values from a channel *)
let.client rec read_again_and_again chan action =
  read_one_value_from_channel chan >>= fun (_,y) ->
  action y >>= fun () ->
  read_again_and_again  chan action

(* client code : what to do with server pushed messages *)
let.client channel_action = function
  | "" -> Lwt.return ()
  | s  -> Js.alert s ; Lwt.return ()

(* server code : create a communication channel *)
let.server channel1 = Comet.Channels.new_channel ()

(* server code : randomly write on the channel *)
let.server rec rand_tick () =
  Lwt_unix.sleep (float_of_int (5 + (Random.int 5))) >>= fun () ->
  Comet.Channels.write channel1 (string_of_int (Random.int 99)) ; rand_tick ()
let.server _ = rand_tick ()


let.server comet1 =
  Eliom_appl.register_new_service
    ~path:["comet1"]
    ~get_params:unit
    (fun _ () () ->
       Lwt.return
         [
           div
             [pcdata "To fully understand the meaning of this, use a \
                      couple browsers on this page. Note that channel \
                     dentifier is printed along with the value."] ;
           div
             ~a:[a_onclick
                   ((fun.client (i : string) ->
                       read_again_and_again i channel_action
                   ) (Comet.Channels.get_id channel1)
                   )
             ]
             [pcdata "Click here to start public channel listening"] ;
         ]
    )



(*zap* *)
open.server Tutoeliom

(* Main page for this example *)
let.server main = new_service [] unit ()

let.server _ = Eliom_predefmod.Xhtmlcompact.register main
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
              a eliomobrowser1 sp [pcdata "Simple example of client side code"] ();
              br ();

              a eliomobrowser2 sp [pcdata "Using Eliom services in client side code"] ();
            br ();
              a eliomobrowser3 sp [pcdata "Caml values in service parameters"] ();
            br ();
              a eliomobrowser4 sp [pcdata "A service sending a Caml value"] ();
            br ();
              a gotowithoutobrowser sp [pcdata "A page that links to a service that belongs to the application but do not launch the application if it is already launched"] ();
            br ();
              a comet1 sp [pcdata "A really simple comet example"] ();
            br ();
          ]
          ]
       )))


(* *zap*)

let.client _ = Lwt_obrowser.run (fst (Lwt.wait ()))
