
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

open Eliom_lib
open Eliom_content
open Html5.D

open Eliom_testsuite1
open Eliom_testsuite2
open Eliom_testsuite3


let _ = Eliom_testsuite6.mainservice
let _ = Eliom_testsuite7.mainservice
(* Main page for the test suite *)
let _ = Eliom_registration.Html5.register Eliom_testsuite_base.main
  (fun () () ->
    Lwt.return
     (html
       (head
          (title (pcdata "Examples from the manual"))
          [Html5.D.css_link
              (Html5.D.make_uri
                 ~service:(Eliom_service.static_dir ()) ["style.css"]) ()])
       (body
          [
            h1 [img ~alt:"Ocsigen"
                   ~src:(Html5.D.make_uri
                           ~service:(Eliom_service.static_dir ()) ["ocsigen5.png"]) ()];

            Eliom_testsuite_base.testsuite ~name:"Test suite 4" Eliom_testsuite4.tests;
            Eliom_testsuite_base.testsuite ~name:"Test suite 5" Eliom_testsuite5.tests;
            h3 [pcdata "Eliom examples"];
            h4 [pcdata "Simple pages"];
            p [
              pcdata "A simple page: ";
              a coucou [code [pcdata "coucou"]] ();
              br ();

              pcdata "A page with a counter: ";
              a count [code [pcdata "count"]] ();
              br ();


              pcdata "A page in a directory: ";
              a hello [code [pcdata "dir/hello"]] ();
              br ();


              pcdata "Default page of a directory: ";
              a default [code [pcdata "rep/"]] ()
            ];

            h4 [pcdata "Distillery"];
            p [
              Raw.a ~a:[a_href (Raw.uri_of_string "distillery/basic")] [pcdata "Basic distillery"];
            ];

            h4 [pcdata "Parameters"];
            p [
              pcdata "A page with GET parameters: ";
              a coucou_params [code [pcdata "coucou"]; pcdata " with params"] (45,(22,"krokodile"));
              pcdata "(what if the first parameter is not an integer?)";
              br ();

              pcdata "A page with \"suffix\" URL that knows the IP and user-agent of the client: ";
              a uasuffix [code [pcdata "uasuffix"]] (2007,6);
              br ();


              pcdata "A page with \"suffix\" URL and GET parameters: ";
              a isuffix [code [pcdata "isuffix"]] ((111, ["OO";"II";"OO"]), 333);
              br ();

              pcdata "A page with constants in suffix: ";
              a constfix [pcdata "Page with constants in suffix"] ("aa", ((), "bb"));
              br ();

              pcdata "Form towards page with suffix: ";
              a suffixform [pcdata "formsuffix"] ();
              br ();

              pcdata "A page with a parameter of user-defined type : ";
              a mytype [code [pcdata "mytype"]] A;
            ];

            h4 [pcdata "Links and Forms"];
            p [
              pcdata "A page with links: ";
              a links [code [pcdata "links"]]  ();
              br ();


              pcdata "A page with a link towards itself: ";
              a linkrec [code [pcdata "linkrec"]] ();
              br ();


              pcdata "The ";
              a Eliom_testsuite_base.main [pcdata "default page"] ();
              pcdata "of this directory (myself)";
              br ();

              pcdata "A page with a GET form that leads to the \"coucou\" page with parameters: ";
              a form [code [pcdata "form"]] ();
              br ();


              pcdata "A POST form towards the \"post\" page: ";
              a form2 [code [pcdata "form2"]] ();
              br ();


              pcdata "The \"post\" page, when it does not receive parameters: ";
              a no_post_param_service [code [pcdata "post"]; pcdata " without post_params"] ();
              br ();


              pcdata "A POST form towards a service with GET parameters: ";
              a form3 [code [pcdata "form3"]] ();
              br ();


              pcdata "A POST form towards an external page: ";
              a form4 [code [pcdata "form4"]] ();
            ];

            h4 [pcdata "Sessions"];
            p [
              pcdata "Coservices: ";
              a coservices_example [code [pcdata "coservice"]] ();
              br ();


              pcdata "A session based on cookies, implemented with session data: ";
              a session_data_example [code [pcdata "sessdata"]] ();
              br ();


              pcdata "A session based on cookies, implemented with actions: ";
              a connect_example3 [code [pcdata "actions"]] ();
              br ();


              pcdata "A session based on cookies, with session services: ";
              a session_services_example [code [pcdata "sessionservices"]] ();
              br ();

              pcdata "A session based on cookies, implemented with actions, with session groups: ";
              a connect_example5 [code [pcdata "groups"]] ();
              br ();


              pcdata "The same with wrong user if not \"toto\": ";
              a connect_example6 [code [pcdata "actions2"]] ();
              br ();


              pcdata "A session based on cookies, implemented with actions, with session groups, and using a group table: ";
              a group_tables_example [code [pcdata "grouptables"]] ();
              br ();


              pcdata "A session based on cookies, implemented with actions, with session groups, and using a persistent group table: ";
              a pgroup_tables_example [code [pcdata "pgrouptables"]] ();
              br ();

              pcdata "Coservices in the session table: ";
              a calc [code [pcdata "calc"]] ();
              br ();


              pcdata "Persistent sessions: ";
              a persist_session_example [code [pcdata "persist"]] ();
              br ();

              pcdata "Volatile group data: ";
              a connect_example_gd [code [pcdata "sessgrpdata"]] ();
              br ();

              pcdata "Persistent group data: ";
              a connect_example_pgd [code [pcdata "psessgrpdata"]] ();
              br ();
            ];

            h4 [pcdata "Other"];
            p [
              pcdata "A page that is very slow, implemented in cooperative way: ";
              a looong [code [pcdata "looong"]] ();
              br ();


              pcdata "A page that is very slow, using preemptive threads: ";
              a looong2 [code [pcdata "looong2"]] ();
              br ();


              pcdata "Catching errors: ";
              a catch [code [pcdata "catch"]] 22;
              pcdata " (change the value in the URL)";
              br ();

              pcdata "Redirection: ";
              a redir [code [pcdata "redir"]] 11;
              br ();

              pcdata "Cookies: ";
              a cookies [code [pcdata "cookies"]] ();
              br ();


              pcdata "Disposable coservices: ";
              a disposable [code [pcdata "disposable"]] ();
              br ();

              pcdata "Coservice with timeout: ";
              a timeout [code [pcdata "timeout"]] ();
              br ();

              pcdata "Public coservice created after initialization (with timeout): ";
              a publiccoduringsess [code [pcdata "publiccoduringsess"]] ();
              br ();


              pcdata "The following URL send either a statically checked page, or a text page: ";
              a send_any [code [pcdata "send_any"]] "valid";
              br ();


              pcdata "A page with a persistent counter: ";
              a count2 [code [pcdata "count2"]] ();
              br ();

              pcdata "A page with a persistent counter with persitent Eliom ref: ";
              a persref [code [pcdata "persref"]] ();
              br ();

              a hier1 [pcdata "Hierarchical menu"] ();
              br ();

              a divpage [code [pcdata "a link sending a &lt;div&gt; page"]] ();
              br ();

              a tonlparams [pcdata "Non localized parameters"] ();
              br ();

              a nlparams [pcdata "Non localized parameters (absent)"] 4;
              br ();

              a nlparams_with_nlp [pcdata "Non localized parameters (present)"] (22, (11, "aa"));
              br ();

              a nlpost_entry [pcdata "Non localized parameters on post service"] ();
              br ();

              a csrfsafe_example [pcdata "CSRF safe services"] ();
              br ();

              a volatile_references [pcdata "Volatile references"] ();
              br ();

              a reference_from_fun [pcdata "References from fun"] ();
              br ();

              a Eliom_testsuite_site.reference_scope_site [pcdata "References of scope site"] ();
              br ();

            ];

            h4 [pcdata "Advanced forms"];
            p [
              pcdata "A page that parses a parameter using a regular expression: ";
              a regexpserv [code [pcdata "regexpserv"]] "[toto]";
              br ();

              pcdata "A form with a checkbox: ";
              a form_bool [pcdata "Try it"] ();
              br ();

              pcdata "A page that takes a set of parameters: ";
              a set [code [pcdata "set"]] ["Ciao";"bello";"ciao"];
              br ();

              pcdata "A form to the previous one: ";
              a setform [code [pcdata "setform"]] ();
              br ();

              pcdata "A page that takes any parameter: ";
              a raw_serv [code [pcdata "raw_serv"]] [("a","hello"); ("b","ciao")];
              br ();

              pcdata "A form to the previous one: ";
              a raw_form [code [pcdata "raw_form"]] ();
              br ();

              pcdata "A form for a list of parameters: ";
              a listform [pcdata "Try it"] ();
              br ();
            ];

            h3 [pcdata "js_of_ocaml events"];

            p [
              a event2_service [code [pcdata "With Lwt"]] ();
              br ();
            ];

            h3 [pcdata "Other tests"];
        p
        [
         a coucou [pcdata "coucou"] (); br ();
         a sumform [pcdata "alternative parameters"] (); br ();
         a sumform2 [pcdata "alternative parameters with POST"] (); br ();
         a optform [pcdata "Optional parameters"] (); br ();
         a main_neopt_service [pcdata "Non-empty optional parameters"] (); br ();
         a sfail [pcdata "Service raising an exception"] (); br ();
         a sraise [pcdata "Wrong use of exceptions during service"] (); br ();
         a getcoex [pcdata "GET coservice with preapplied fallback, etc"] (); br ();
         a postcoex [pcdata "POST service with coservice fallback"] (); br ();
         a su [pcdata "Suffix and other service at same URL"] (); br ();
         a suffixform_su2 [pcdata "Suffix and other service at same URL: a form towards the suffix service"] (); br ();
         a su4 [pcdata "Suffix service with constant part and priority"] ("aa", ((), "bb")); br ();
         a preappliedsuffix [pcdata "Preapplied suffix"] (); br ();
         a attnonatt_service [pcdata "Attachment of non-attached coservice"] (); br ();
         a constform [pcdata "Form towards suffix service with constants"] (); br ();
         a getact [pcdata "action on GET attached coservice, etc"] 127; br ();
         a noreload [pcdata "action with `NoReload option"] (); br ();
         a cookies2 [pcdata "Many cookies"] "le suffixe de l'URL"; br ();
         a headers [pcdata "Customizing HTTP headers"] (); br ();
         a sendfileex [pcdata "Send file"] (); br ();
         a sendfile2 [pcdata "Send file 2"] "style.css"; br ();
         a sendfileexception [pcdata "Do not send file"] (); br ();
         a sendfileregexp [pcdata "Send file with regexp"] (); br ();
         a suffixform2 [pcdata "Suffix 2"] (); br ();
         a suffixform3 [pcdata "Suffix 3"] (); br ();
         a suffixform4 [pcdata "Suffix 4"] (); br ();
         a nosuffix [pcdata "Page without suffix on the same URL of a page with suffix"] (); br ();
         a anypostform [pcdata "POST form to any parameters"] (); br ();
         a any2 [pcdata "int + any parameters"]
           (3, [("Ciao","bel"); ("ragazzo","!")]); br ();
         a any3 [pcdata "any parameters broken (s after any)"]
           (4, ([("Thierry","Richard");("Sébastien","Stéphane")], "s")); br ();
(* broken        a any4 [pcdata "Any in suffix"] [("bo","ba");("bi","bu")]; br (); *)
         a any5 [pcdata "Suffix + any parameters"]
           ("ee", [("bo","ba");("bi","bu")]); br ();
         a upload [pcdata "Upload"] (); br ();
         a uploadgetform [pcdata "Upload with GET"] (); br ();
         a sufli [pcdata "List in suffix"] [("bo", 4);("ba", 3);("bi", 2);("bu", 1)]; br ();
         a sufliform [pcdata "Form to list in suffix"] (); br ();
         a sufliopt [pcdata "List of optional values in suffix"] [None; Some "j"]; br ();
         a sufliopt2 [pcdata "List of optional pairs in suffix"] [None; Some ("j", "ee")]; br ();
         a sufset [pcdata "Set in suffix"] ["bo";"ba";"bi";"bu"]; br ();
(*         a sufli2 [pcdata "List not in the end of in suffix"] ([1; 2; 3], 4); br (); *)
         a boollistform [pcdata "Bool list"] (); br ();
         a lilists [pcdata "List of lists in parameters"] (); br ();
         a wlf_lists [pcdata "List of lists in parameters - 2nd example"] [[333]]; br ();
         a preappmenu [pcdata "Menu with pre-applied services"] (); br ();
         a exn_act_main [pcdata "Actions that raises an exception"] (); br ();
         a close_from_outside [pcdata "Closing sessions from outside"] (); br ();
         a set_timeout_form [pcdata "Setting timeouts from outside sessions"] (); br ();
         a
           ~fragment:"a--   ---++&é/@"
           ~service:url_encoding
           [pcdata "Urls with strange characters inside"]
           (["l/l%l      &l=l+l)l@";"m\\m\"m";"n?èn~n"],
            [("po?po&po~po/po+po", "lo?\"l     o#lo'lo lo=lo&l      o/lo+lo");
            ("bo=mo@co:ro", "zo^zo%zo$zo:zo?aaa")]); br ();
         a ~service:(Eliom_service.static_dir_with_params ~get_params:Eliom_parameter.any ())
           [pcdata "Static file with GET parameters"]
           (["ocsigen5.png"], [("aa", "lmk"); ("bb", "4")]); br ();

         a extreq [pcdata "External request"] (); br ();
         a servreq [pcdata "Server request"] (); br ();
         a servreqloop [pcdata "Looping server request"] (); br ();

         a nlparams2 [pcdata "nl params and suffix, on void coservice"] ((3, 5), 222); br ();
         a optsuf [pcdata "optional suffix"] None; br ();
         a optsuf [pcdata "optional suffix"] (Some ("<a to/to=3?4=2>", None)); br ();
         a optsuf [pcdata "optional suffix"] (Some ("toto", Some 2)); br ();
         a optsuf2 [pcdata "optional suffix 2"] (Some "un", Some 2); br ();
         a optsuf2 [pcdata "optional suffix 2"] (None, Some 2); br ();
         a optsuf2 [pcdata "optional suffix 2"] (Some "un", None); br ();
         a optsuf2 [pcdata "optional suffix 2"] (None, None); br ();

         a csrfsafe_get_example [pcdata "GET CSRF safe service"] (); br ();
         a csrfsafe_postget_example [pcdata "POST CSRF safe service on GET CSRF safe service"] (); br ();
         a csrfsafe_session_example [pcdata "POST non attached CSRF safe service in session table"] (); br ();
         a unregister_example [pcdata "Unregistering services"] (); br ();
         a raw_post_example [pcdata "Raw POST data"] (); br ();
        ];

            h3 [pcdata "Eliom Client"];
            h4 [pcdata "Interaction"];
            p [
              a eliomclient1 [pcdata "Simple example of client side code"] ();
              br ();
              a eliomclient2 [pcdata "Nodes with various onclick features"] ();
              br ();
              a eliomclient3 [pcdata "Caml values in service parameters"] ();
              br ();
              a eliomclient4 [pcdata "A service sending a Caml value"] ();
              br ();
              a uri_test [pcdata "Simple test of URL generation"] ();
              br ();
              a formc [pcdata "Links and forms"] ();
              br ();
              a ~fragment:"id40" ~service:long_page [pcdata "Fragment scrolling"] ();
              br ();
              a live1 [ pcdata "History handling and page load/unload events." ] ();
              br ();
              a relink_test [pcdata "Global elements"] ();
              br ();
              a unique1 [pcdata "Onload event on element and global element"] ();
              br ();
              a body_onload [pcdata "Onload event on the body element"] ();
              br ();
              (* a xhr_form_with_file [pcdata "xhr forms with file"] (); *)
              (* br (); *)
              a caml_service_cookies [pcdata "Client process cookies with caml service"] ();
              br ();
              a gotowithoutclient [pcdata "A page that links to a service that belongs to the application but do not launch the application if it is already launched"] ();
              br ();
              a default_no_appl [pcdata "Toggle the default value of no_appl"] ();
              br ();
              a wrapping1 [pcdata "wrapping test 1"] ();
              br ();
              a wrapping_big_values [pcdata "wrapping test: big values"] 200000;
              br ();
              a caml_service_wrapping [pcdata "wrapping for caml call service"] ();
              br ();
              a caml_service_with_onload [pcdata "onload with caml call service"] ();
              br ();
              a service_style1 [pcdata "test header modifications"] ();
              br ();
              a any_service [pcdata "Eliom_output.Any with Eliom_appl"] 1;
              br ();
              a domnodes_timings [pcdata "Speed test for TyXMl nodes with dom semantics (previously known as unique node of scope request)"] (2,10);
              br ();
              a shared_dom_nodes [pcdata "Multiple occurences of unique nodes."] ();
              br ();
              a nl_serv [pcdata "Non localised parameters and eliom appl"] ();
              br ();
              a nlpost_entry [pcdata "Non localised parameters with forms in eliom appl"] ();
              br ();
            ];
            h4 [pcdata "Templates"];
            p [
              a tmpl1_page1 [pcdata "Multiple template switching."] ();
              br ();
              a hist_page1 [pcdata "Browser history and templates."] ();
              br ();
            ];
            h4 [pcdata "Comet"];
            p [
              a comet1 [pcdata "A really simple comet example"] ();
              br ();
              a comet2 [pcdata "A comet example with server to client and client to server asynchronous events"] ();
              br ();
              a comet3 [pcdata "Server simultaneous events, transmitted together"] ();
              br ();
              a comet_wrapping [pcdata "sent wrapped values"] ();
              br ();
              a comet_signal [pcdata "Signal"] ();
              br ();
              a comet_message_board [pcdata "Minimalistic message board"] ();
              br ();
            ];
            h5 [pcdata "stateless"];
            p [
	      a comet_stateless [pcdata "simple stateless comet"] ();
              br ();
              a comet_signal_stateless [pcdata "Signal"] ();
              br ();
              a comet_message_board_stateless [pcdata "Minimalistic stateless message board"] ();
              br ();
              a bus_multiple_times [pcdata "Bus stream used multiple times"] ();
              br ();
            ];
            h5 [pcdata "external"];
            p [
	      a comet_stateless_external [pcdata "comet channel on another server"] ();
              a external_xhr [pcdata "request and external service with xhr"] ();
            ];
            h4 [pcdata "More tests"];
            p [
              a appl_redir [pcdata "Eliom applications and redirections"] ();
              br ();
              a noreload_appl [pcdata "Eliom applications and actions with `NoReload option"] ();
              br ();
              a nonapplprocessservice [pcdata "Client process service not registered with Eliom_appl"] ();
              br ();
              a gracefull_fail_with_file [ pcdata "link to a service hidden by a file" ] ();
              br ();
              a appl_with_redirect_service [ pcdata "link to a service hidden by a redirection" ] ();
              br ();
              a big_service [ pcdata "loading a big page" ] ();
              br ();
            ];

            h4 [pcdata "Process states"];
            p [
              a states_test [pcdata "Extensive test of Eliom references of different scopes, accessed from inside or outside the state itself"] ();
              br ();
              pcdata "Coservices: ";
              a tcoservices_example [code [pcdata "tcoservice"]] ();
              br ();

              pcdata "Coservice with timeout: ";
              a ttimeout [code [pcdata "timeout"]] ();
              br ();

              pcdata "A session based on cookies, implemented with session data: ";
              a tsession_data_example [code [pcdata "tsessdata"]] ();
              br ();


              pcdata "A session based on cookies, implemented with actions: ";
              a tconnect_example3 [code [pcdata "tactions"]] ();
              br ();


              pcdata "A session based on cookies, with session services: ";
              a tsession_services_example [code [pcdata "tsessionservices"]] ();
              br ();

              pcdata "A session based on cookies, implemented with actions, with session groups: ";
              a connect_example5 [code [pcdata "groups"]] ();
              br ();

              pcdata "Session and client process: ";
              a connect_example789 [code [pcdata "session_appl"]] ();
              br ();

(*
              pcdata "The same with wrong user if not \"toto\": ";
              a tconnect_example6 [code [pcdata "tactions2"]] ();
              br ();
*)


              pcdata "Coservices in the session table: ";
              a tcalc [code [pcdata "tcalc"]] ();
              br ();


              pcdata "Persistent sessions: ";
              a tpersist_session_example [code [pcdata "tpersist"]] ();
              br ();


              a tcsrfsafe_example [pcdata "CSRF safe services"] ();
              br ()
            ];
            h4 [ pcdata "Other" ];
            p
              [ pcdata "User tab cookies: ";
                a tcookies
                  [ code [ pcdata "tcookies" ] ] ();
                br ();
                pcdata "A link inside the application that ask for an action outside the application. Eliom will ask the client side program to do a redirection: ";
                a actionoutside [ code [ pcdata "actionoutside" ] ] ();
                br ();
              ]
          ])))

(* *zap*)
