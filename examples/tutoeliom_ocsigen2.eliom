(*zap* *)
begin.client
  let (>>=) = Lwt.bind
end
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
        
====Your first client-side function
        %<div class="onecol"|
          
*wiki*)
open XHTML.M
open Eliom_parameters
open Eliom_predefmod.Xhtml
open Eliom_services

let eliomobrowser1 =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["eliomobrowser1"]
    ~get_params:unit
    (fun sp () () ->
      Lwt.return
        (html
           (head
              (title (pcdata "Eliom + O'Browser"))
              [
                js_script 
                  ~uri:(make_uri ~service:(static_dir sp) ~sp ["vm.js"]) ();
                js_script 
                  ~uri:(make_uri ~service:(static_dir sp) ~sp ["eliom_obrowser.js"]) ();
                script ~contenttype:"text/javascript"
                  (cdata_script
      "window.onload = function () {
        main_vm = exec_caml (\"tutoeliom_ocsigen2_client.uue\") ;
      }")])
           (body [p ~a:[a_onclick 
                           ((fun.client (() : unit) -> Js.alert "clicked!") ())]
                    [pcdata "I am a clickable paragraph"];

                 ])))
(*wiki*
====Compiling
//soon (have a look at Ocsigen source for now -- //examples// directory)// 

====Using a distant Eliom service in client side code

The code will look like:
%<code language="ocaml"|
p ~onclick:{{Eliom_obrowser.post_request ~sp ~service:myblockservice ()
                   >>= Node.append bodynode}}
        [pcdata "Click here to add content from the server."];
>%

For now, you can do:
*wiki*)
let eliomobrowser2 = new_service ~path:["eliomobrowser2"] ~get_params:unit ()

let myblockservice =
  Eliom_predefmod.Blocks.register_new_post_coservice
    ~fallback:eliomobrowser2
    ~post_params:unit
    (fun _ () () -> 
       Lwt.return
         [p [pcdata ("I come from a distant service! Here is a random value: "^
                       string_of_int (Random.int 100))]])

let _ =
  Eliom_predefmod.Xhtml.register
    eliomobrowser2
    (fun sp () () ->
(*zap* *)
      Lwt.return
        (html
           (head
              (title (pcdata "Eliom + O'Browser"))
              [
                js_script 
                  ~uri:(make_uri ~service:(static_dir sp) ~sp ["vm.js"]) ();
                js_script 
                  ~uri:(make_uri ~service:(static_dir sp) ~sp ["eliom_obrowser.js"]) ();
                script ~contenttype:"text/javascript"
                  (cdata_script
      "window.onload = function () {
        main_vm = exec_caml (\"tutoeliom_ocsigen2_client.uue\") ;
      }"
                  )])
           (* *zap*)
(*wiki*
{{{...<same header>...}}}
*wiki*)
           (body
              [
(*wiki*
  The following examples shows how to go to another service,
  exactly like pressing a link:
*wiki*)
                p 
                  ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                    a_onclick 
                      ((fun.client
                          (sp : Eliom_client_types.server_params)
                          (service : ('a, 'b, 'c, 'd, 'e, 'f, 'g) Eliom_services.service) -> 
                            Eliom_client.exit_to ~sp ~service () ()
                       ) (Eliom_obrowser.client_sp sp) Tutoeliom.coucou)
                  ]
                  [pcdata "Click here to go to another page."];

(*wiki*
  The following examples shows how to do a request to a service,
  and use the content:
*wiki*)
                p 
                  ~a:[(*zap* *)a_class ["clickable"];(* *zap*)
                    a_onclick 
                      ((fun.client
                          (sp : Eliom_client_types.server_params)
                          (myblockservice : ('a, 'b, 'c, 'd, 'e, 'f, 'g) Eliom_services.service) -> 
                            let body = JSOO.eval "document.body" in
                            (*Js.get_element_by_id "bodyid"*)
                            Eliom_client.call_service
                              ~sp ~service:myblockservice () () >>= fun s ->
                                (try
                                   let l = Js.Node.children (Js.dom_of_xml s) in
                                   List.iter (Js.Node.append body) l
                                 with e -> Js.alert (Printexc.to_string e));
                                Lwt.return ()
                       ) (Eliom_obrowser.client_sp sp) myblockservice)
(*zap*
  Problème avec le type du service : il faut l'écrire en entier et exactement
  sinon on n'a pas de vérif de type côté client *)
(* je me suis fait avoir en mettant let bodyid = "body" in
   ... Js.get_element_by_id bodyid ...
   sans le passer en paramètre à la fun.client
   Il faut vraiment automatiser ça...
*zap*)
                  ]
                  [pcdata "Click here to add content from the server."];
             
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
                          (sp : Eliom_client_types.server_params)
                          (service : ('a, 'b, 'c, 'd, 'e, 'f, 'g) Eliom_services.service) -> 
                            Eliom_client.change_url ~sp ~service ()
                       ) (Eliom_obrowser.client_sp sp) Tutoeliom.coucou)
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
                          (sp : Eliom_client_types.server_params)
                          (service : ('a, 'b, 'c, 'd, 'e, 'f, 'g) Eliom_services.service) -> 
                            Eliom_client.change_page ~sp ~service () ()
                       ) (Eliom_obrowser.client_sp sp) myblockservice)
                  ]
                  [pcdata "Click here to change the page without stopping the program."];

             
              
              ])))
(*wiki*
====Implicit registration of services to implement distant function calls
*wiki*)
(*wiki*
====
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
              a eliomobrowser1 sp [pcdata "Simple example of client side code"] ();
              br ();

              a eliomobrowser2 sp [pcdata "Using Eliom services in client side code"] ();
            br ();
          ]
          ]
       )))


(* *zap*)

;;
begin.client
  let _ = Lwt_obrowser.run (fst (Lwt.wait ()))
end
