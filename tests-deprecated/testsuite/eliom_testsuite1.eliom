(* Eliom test suite, part 1 *)
(* TODO: extract the tests from the manual or vice versa.
   Take the code in the manual, not here! (and remove duplicates here) *)
(* TODO: include some missing parts in the manual *)

module Eliom_service = Eliom_testsuite_base.Service
module Eliom_registration = Eliom_testsuite_base.Registration

{client{ module Eliom_registration = Eliom_registration }}
{client{ module Eliom_service = Eliom_service }}

open Eliom_content
open Lwt
open Html.F
open Ocsigen_cookies
open Eliom_service
open Eliom_parameter
open Eliom_state
open Eliom_registration.Html

let coucou =
  register_service
    ~path:["coucou"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo!"]])))

let coucou1 =
  register_service
    ~path:["coucou1"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      let module Html = Eliom_content.Html.F in
      return
        << <html>
             <head><title></title></head>
             <body><h1>Coucou</h1></body>
           </html> >>)

(*
let coucou_xhtml =
  let open XHTML.M in
  Eliom_output.Xhtml.register_service
    ~path:["coucou_xhtml"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo!"]])))
*)

(*
let coucou1_xthml =
  Eliom_output.Html.register_service
    ~path:["coucou1_xhtml"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      let module Html = Eliom_content.Html.F in
      return
        <:xhtml< <html>
             <head><title></title></head>
             <body><h1>Coucou</h1></body>
           </html> >>)

 *)

(*wiki*

        Page generation may have side-effects:



*wiki*)
let count =
  let next =
    let c = ref 0 in
      (fun () -> c := !c + 1; !c)
  in
  register_service
    ~path:["count"]
    ~get_params:unit
    (fun () () ->
      return
        (html
         (head (title (pcdata "counter")) [])
         (body [p [pcdata (string_of_int (next ()))]])))
(*wiki*
As usual in OCaml, you can forget labels when the application
          is total:

*wiki*)
let hello =
  register_service
    ["dir";"hello"]  (* the url dir/hello *)
    unit
    (fun () () ->
      return
        (html
         (head (title (pcdata "Hello")) [])
         (body [h1 [pcdata "Hello"]])))
(*wiki*

The following example shows how to define the default page for
a directory. (Note that %<span class="code"|["rep";""]>% means
the default page of the directory %<span class="code"|rep/>%)

*wiki*)
let default = register_service ["rep";""] unit
  (fun () () ->
    return
     (html
      (head (title (pcdata "")) [])
      (body [p [pcdata "default page. rep is redirected to rep/"]])))



let writeparams (i1, (i2, s1)) () =
  return
   (html
    (head (title (pcdata "")) [])
    (body [p [pcdata "You sent: ";
              strong [pcdata (string_of_int i1)];
              pcdata ", ";
              strong [pcdata (string_of_int i2)];
              pcdata " and ";
              strong [pcdata s1]]]))
(*zap* you can register twice the same service, with different parameter names
 *zap*)
let coucou_params = register_service
    ~path:["coucou"]
    ~get_params:(int "i" ** (int "ii" ** string "s"))
    writeparams
(*zap* If you register twice exactly the same URL, the server won't start
 *zap*)
(*wiki*



*wiki*)
let uasuffix =
  register_service
    ~path:["uasuffix"]
    ~get_params:(suffix (int "year" ** int "month"))
    (fun (year, month) () ->
      return
       (html
        (head (title (pcdata "")) [])
        (body
           [p [pcdata "The suffix of the url is ";
               strong [pcdata ((string_of_int year)^"/"
                               ^(string_of_int month))];
               pcdata ", your user-agent is ";
               strong [pcdata (Eliom_request_info.get_user_agent ())];
               pcdata ", your IP is ";
               strong [pcdata (Eliom_request_info.get_remote_ip ())]]])))
(*wiki*

*wiki*)
let isuffix =
  register_service
    ~path:["isuffix"]
    ~get_params:(suffix_prod (int "suff" ** all_suffix "endsuff") (int "i"))
    (fun ((suff, endsuff), i) () ->
      return
       (html
        (head (title (pcdata "")) [])
        (body
           [p [pcdata "The suffix of the url is ";
               strong [pcdata (string_of_int suff)];
               pcdata " followed by ";
               strong [pcdata (Ocsigen_lib.Url.string_of_url_path ~encode:false endsuff)];
               pcdata " and i is equal to ";
               strong [pcdata (string_of_int i)]]])))
(*wiki*


  *wiki*)
let constfix =
  register_service
    ~path:["constfix"]
    ~get_params:(suffix (string "s1" ** (Eliom_parameter.suffix_const "toto" ** string "s2")))
    (fun (s1, ((), s2))  () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1
                   [pcdata "Suffix with constants"];
                 p [pcdata ("Parameters are "^s1^" and "^s2)]])))
(*wiki*


*wiki*)
type mysum = A | B
let mysum_of_string = function
  | "A" -> A
  | "B" -> B
  | _ -> raise (Failure "mysum_of_string")
let string_of_mysum = function
  | A -> "A"
  | B -> "B"

let mytype =
  Eliom_registration.Html.register_service
    ~path:["mytype"]
    ~get_params:
      (Eliom_parameter.user_type mysum_of_string string_of_mysum "valeur")
    (fun x () ->
      let v = string_of_mysum x in
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata (v^" is valid. Now try with another value.")]])))
(*wiki*


*wiki*)
let raw_serv =
  register_service
    ~path:["any"]
    ~get_params:Eliom_parameter.any
  (fun l () ->
    let module Html = Eliom_content.Html.F in
    let ll =
      List.map
        (fun (a,s) -> <:html< <strong>($str:a$, $str:s$)</strong> >>) l
    in
    return
     <:html< <html>
          <head><title></title></head>
          <body>
          <p>
            You sent:
            $list:ll$
          </p>
          </body>
        </html> >>)
(*wiki*

*wiki*)

let catch = register_service
    ~path:["catch"]
    ~get_params:(int "i")
    ~error_handler:(fun l ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata ("i is not an integer.")]])))
    (fun i () ->
      let v = string_of_int i in
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata ("i is an integer: "^v)]])))
(*wiki*


*wiki*)
let links = register_service ["rep";"links"] unit
 (fun () () ->
   return
    (html
     (head (title (pcdata "Links")) [])
     (body
       [p
        [Html.D.a coucou [pcdata "coucou"] (); br ();
         Html.D.a hello [pcdata "hello"] (); br ();
         Html.D.a default
           [pcdata "default page of the dir"] (); br ();
         Html.D.a uasuffix
           [pcdata "uasuffix"] (2007,06); br ();
         Html.D.a coucou_params
           [pcdata "coucou_params"] (42,(22,"ciao")); br ();
         Html.D.a raw_serv
           [pcdata "raw_serv"] [("sun","yellow");("sea","blue and pink")]; br ();
         Html.D.a
           (Eliom_service.extern
              ~prefix:"http://fr.wikipedia.org"
              ~path:["wiki"; ""]
              ~meth:(Eliom_service.Get (suffix (all_suffix "suff")))
              ())
           [pcdata "OCaml on wikipedia"]
           ["OCaml"]; br ();
         Html.F.Raw.a
           ~a:[a_href (Xml.uri_of_string "http://en.wikipedia.org/wiki/OCaml")]
           [pcdata "OCaml on wikipedia"]
       ]])))
(*zap*
   Note that to create a link we need to know the current url, because:
   the link from toto/titi to toto/tata is "tata" and not "toto/tata"
*zap*)
(*wiki*



*wiki*)
let linkrec = Eliom_service.Http.service ["linkrec"] unit ()

let _ = Eliom_registration.Html.register linkrec
    (fun () () ->
      return
       (html
        (head (title (pcdata "")) [])
        (body [p [a linkrec [pcdata "click"] ()]])))
(*zap* If some url are not registered, the server will not start:
let essai =
  new_url
   ~path:["essai"]
   ~server_params:no_server_param
   ~get_params:no_get_param
   ()
*zap*)
(*zap* pour les reload : le serveur ne s'éteint pas mais ajoute un message sur les services non enregistrés dans son log *zap*)
(*wiki*


*wiki*)
let create_form =
  (fun (number_name, (number2_name, string_name)) ->
    [p [pcdata "Write an int: ";
        Html.D.Form.input ~input_type:`Text ~name:number_name
          Html.D.Form.int;
        pcdata "Write another int: ";
        Html.D.Form.input ~input_type:`Text ~name:number2_name
          Html.D.Form.int;
        pcdata "Write a string: ";
        Html.D.Form.input ~input_type:`Text ~name:string_name
          Html.D.Form.string;
        Html.D.Form.input ~input_type:`Submit ~value:"Click"
          Html.D.Form.string;
       ]])

let form = register_service ["form"] unit
  (fun () () ->
    let f = Html.D.Form.get_form coucou_params create_form in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))
(*wiki*

*wiki*)
let raw_form = register_service
    ~path:["anyform"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [h1 [pcdata "Any Form"];
               Html.D.Form.get_form raw_serv
                 (fun () ->
                   [p [pcdata "Form to raw_serv: ";
                       Html.D.(
                         input ~a:[a_input_type `Text; a_name "plop"] ());
                       Html.D.(
                         input ~a:[a_input_type `Text; a_name "plip"] ());
                       Html.D.(
                         input ~a:[a_input_type `Text; a_name "plap"] ());
                       Html.D.Form.input
                         ~input_type:`Submit ~value:"Click"
                         Html.D.Form.string]])
                ])))
(*wiki*



*wiki*)
let no_post_param_service =
  register_service
    ~path:["post"]
    ~get_params:unit
    (fun () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata
                      "Version of the page without POST parameters"]])))

let my_service_with_post_params =
  register_post_service
    ~fallback:no_post_param_service
    ~post_params:(string "value")
    (fun () value ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata value]])))
(*wiki*




Services may take both GET and POST parameters:


*wiki*)
let get_no_post_param_service =
  register_service
    ~path:["post2"]
    ~get_params:(int "i")
    (fun i () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "No POST parameter, i:";
                   em [pcdata (string_of_int i)]]])))

let my_service_with_get_and_post = register_post_service
  ~fallback:get_no_post_param_service
  ~post_params:(string "value")
  (fun i value ->
    return
      (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Value: ";
                   em [pcdata value];
                   pcdata ", i: ";
                   em [pcdata (string_of_int i)]]])))
(*wiki*

POST forms

*wiki*)
let form2 = register_service ["form2"] unit
  (fun () () ->
     let f =
       (Html.D.Form.post_form my_service_with_post_params
          (fun chaine ->
            [p [pcdata "Write a string: ";
                Form.input ~input_type:`Text ~name:chaine Form.string]]) ()) in
     return
       (html
         (head (title (pcdata "form")) [])
         (body [f])))

let form3 = register_service ["form3"] unit
  (fun () () ->
     let module Html = Eliom_content.Html.F in
     let f  =
       (Eliom_content.Html.D.Form.post_form my_service_with_get_and_post
          (fun chaine ->
            <:htmllist< <p> Write a string:
                    $Form.input ~input_type:`Text ~name:chaine Form.string$ </p> >>)
          222) in
     return
       <:html< <html>
            <head><title></title></head>
            <body>$f$</body></html> >>)

let form4 = register_service ["form4"] unit
  (fun () () ->
      let module Html = Eliom_content.Html.F in
     let f  =
       (Eliom_content.Html.D.Form.post_form
          (Eliom_service.extern
             ~prefix:"http://www.petizomverts.com"
             ~path:["zebulon"]
             ~meth:(Eliom_service.Post (int "i", string "chaine"))
             ())
          (fun chaine ->
            <:htmllist< <p> Write a string:
                     $Form.input ~input_type:`Text ~name:chaine Form.string$ </p> >>)
          222) in
     return
       (html
        (head (title (pcdata "form")) [])
        (body [f])))
(*wiki*

Lwt


%<code language="ocaml"|let looong =
  register_service
    ~path:["looong"]
    ~get_params:unit
    (fun () () ->
      Unix.sleep 5;
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Ok now, you can read the page."]])))
>%



*wiki*)
let looong =
  register_service
    ~path:["looong"]
    ~get_params:unit
    (fun () () ->
      Lwt_unix.sleep 5.0 >>= fun () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata
                   "Ok now, you can read the page."]])))
(*wiki*

*wiki*)
let looong2 =
  register_service
    ~path:["looong2"]
    ~get_params:unit
    (fun () () ->
      Lwt_preemptive.detach Unix.sleep 5 >>= fun () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata
                   "Ok now, you can read the page."]])))
(*wiki*


*wiki*)
(************************************************************)
(************ Connection of users, version 1 ****************)
(************************************************************)

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "session_data"
let session = `Session scope_hierarchy
(* *zap*)

(* "my_table" will be the structure used to store
   the session data (namely the login name): *)

let my_table = Eliom_state.create_volatile_table (*zap* *) ~scope:session (* *zap*) ()


(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let session_data_example =
  Eliom_service.Http.service
    ~path:["sessdata"]
    ~get_params:Eliom_parameter.unit
    ()

let session_data_example_with_post_params =
  Eliom_service.Http.post_service
    ~fallback:session_data_example
    ~post_params:(Eliom_parameter.string "login")
    ()

let session_data_example_close =
  Eliom_service.Http.service
    ~path:["close"]
    ~get_params:Eliom_parameter.unit
    ()



(* -------------------------------------------------------- *)
(* Handler for the "session_data_example" service:          *)

let session_data_example_handler _ _  =
  let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          [
           match sessdat with
           | Eliom_state.Data name ->
               p [pcdata ("Hello "^name);
                  br ();
                  Html.D.a
                    session_data_example_close
                    [pcdata "close session"] ()]
           | Eliom_state.Data_session_expired
           | Eliom_state.No_data ->
               Html.D.Form.post_form
                 session_data_example_with_post_params
                 (fun login ->
                   [p [pcdata "login: ";
                       Html.D.Form.input
                         ~input_type:`Text ~name:login
                         Html.D.Form.string]]) ()
         ]))

(* -------------------------------------------------------- *)
(* Handler for the "session_data_example_with_post_params"  *)
(* service with POST params:                                *)

let session_data_example_with_post_params_handler _ login =
  lwt () = Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () in
  Eliom_state.set_volatile_data ~table:my_table login;
  return
    (html
       (head (title (pcdata "")) [])
       (body
          [p [pcdata ("Welcome " ^ login ^ ". You are now connected.");
              br ();
              Html.D.a session_data_example
                [pcdata "Try again"] ()
            ]]))



(* -------------------------------------------------------- *)
(* Handler for the "session_data_example_close" service:    *)

let session_data_example_close_handler () () =
  let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
  lwt () = Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () in
  return
    (html
       (head (title (pcdata "Disconnect")) [])
       (body [
        (match sessdat with
        | Eliom_state.Data_session_expired -> p [pcdata "Your session has expired."]
        | Eliom_state.No_data -> p [pcdata "You were not connected."]
        | Eliom_state.Data _ -> p [pcdata "You have been disconnected."]);
        p [Html.D.a session_data_example [pcdata "Retry"] () ]]))


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register
    session_data_example_close session_data_example_close_handler;
  Eliom_registration.Html.register
    session_data_example session_data_example_handler;
  Eliom_registration.Html.register
    session_data_example_with_post_params
    session_data_example_with_post_params_handler






(*zap* *)
let () = set_default_global_service_state_timeout
  ~cookie_level:`Session (Some 600.)
let () = set_default_global_persistent_data_state_timeout
  ~cookie_level:`Session (Some 3600.)
(* *zap*)
(************************************************************)
(************ Connection of users, version 2 ****************)
(************************************************************)

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "session_services"
let session = `Session scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let session_services_example =
  Eliom_service.Http.service
    ~path:["sessionservices"]
    ~get_params:Eliom_parameter.unit
    ()

let session_services_example_with_post_params =
  Eliom_service.Http.post_service
    ~fallback:session_services_example
    ~post_params:(Eliom_parameter.string "login")
    ()

let session_services_example_close =
  Eliom_service.Http.service
    ~path:["close2"]
    ~get_params:Eliom_parameter.unit
    ()


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example" service:           *)
(* It displays the main page of our site, with a login form.     *)

let session_services_example_handler () () =
  let f =
    Html.D.Form.post_form
      session_services_example_with_post_params
      (fun login ->
        [p [pcdata "login: ";
            Form.input ~input_type:`Text ~name:login Form.string]]) ()
  in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_close" service:     *)

let session_services_example_close_handler () () =
  lwt () = Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () in
  return
    (html
       (head (title (pcdata "Disconnect")) [])
       (body [p [pcdata "You have been disconnected. ";
                 a session_services_example
                   [pcdata "Retry"] ()
               ]]))

(*wiki*


When the page is called with login parameters,
       it runs the function %<span class="code"|launch_session>%
       that replaces some services already defined by new ones:



*wiki*)
(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_with_post_params"   *)
(* service:                                                      *)

let launch_session () login =

  (* New handler for the main page: *)
  let new_main_page () () =
    return
      (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Welcome ";
                 pcdata login;
                 pcdata "!"; br ();
                 a coucou [pcdata "coucou"] (); br ();
                 a hello [pcdata "hello"] (); br ();
                 a links [pcdata "links"] (); br ();
                 a session_services_example_close
                   [pcdata "close session"] ()]]))
  in

  (* If a session was opened, we close it first! *)
  lwt () = Eliom_state.discard ~scope:session () in

  (* Now we register new versions of main services in the
     session service table: *)
  Eliom_registration.Html.register ~scope:session
    ~service:session_services_example
    (* service is any public service already registered,
       here the main page of our site *)
    new_main_page;

  Eliom_registration.Html.register ~scope:session
    ~service:coucou
    (fun () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Coucou ";
                   pcdata login;
                   pcdata "!"]])));

  Eliom_registration.Html.register ~scope:session
    ~service:hello
    (fun () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Ciao ";
                   pcdata login;
                   pcdata "!"]])));

  new_main_page () ()

(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register
    ~service:session_services_example
    session_services_example_handler;
  Eliom_registration.Html.register
    ~service:session_services_example_close
    session_services_example_close_handler;
  Eliom_registration.Html.register
    ~service:session_services_example_with_post_params
    launch_session
(*zap* Registering for session during initialisation is forbidden:
let _ = register ~scope:`Session
    ~path:coucou1
    %< <html>
         <head><title></title></head>
         <body><h1>humhum</h1></body>
       </html> >%
*zap*)
(*wiki*


*wiki*)
(************************************************************)
(************** Coservices. Basic examples ******************)
(************************************************************)

(* -------------------------------------------------------- *)
(* We create one main service and two coservices:           *)

let coservices_example =
  Eliom_service.Http.service
    ~path:["coserv"]
    ~get_params:Eliom_parameter.unit
    ()

let coservices_example_post =
  Eliom_service.Http.post_coservice
    ~fallback:coservices_example
    ~post_params:Eliom_parameter.unit
    ()

let coservices_example_get =
  Eliom_service.Http.coservice
    ~fallback:coservices_example
    ~get_params:Eliom_parameter.unit
    ()

(* -------------------------------------------------------- *)
(* The three of them display the same page,                 *)
(* but the coservices change the counter.                   *)

let _ =
  let c = ref 0 in
  let page () () =
    let l3 = Html.D.Form.post_form coservices_example_post
        (fun _ -> [p [Html.D.Form.input
                        ~input_type:`Submit
                        ~value:"incr i (post)"
                        Html.D.Form.string]]) ()
    in
    let l4 = Html.D.Form.get_form coservices_example_get
        (fun _ -> [p [Html.D.Form.input
                        ~input_type:`Submit
                        ~value:"incr i (get)"
                        Html.D.Form.string]])
    in
    return
      (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "i is equal to ";
                 pcdata (string_of_int !c); br ();
                 a coservices_example [pcdata "reload"] (); br ();
                 a coservices_example_get [pcdata "incr i"] ()];
              l3;
              l4]))
  in
  Eliom_registration.Html.register coservices_example page;
  let f () () = c := !c + 1; page () () in
  Eliom_registration.Html.register coservices_example_post f;
  Eliom_registration.Html.register coservices_example_get f
(*wiki*



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



          >%

*wiki*)
(*zap* Queinnec example: *zap*)
(************************************************************)
(*************** calc: sum of two integers ******************)
(************************************************************)

(*zap* *)
let calc_example_scope_hierarchy = Eliom_common.create_scope_hierarchy "calc_example"
let session = `Session calc_example_scope_hierarchy
let session_group = `Session_group calc_example_scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create two main services on the same URL,             *)
(* one with a GET integer parameter:                        *)

let calc =
  Http.service
    ~path:["calc"]
    ~get_params:unit
    ()

let calc_i =
  Http.service
    ~path:["calc"]
    ~get_params:(int "i")
    ()


(* -------------------------------------------------------- *)
(* The handler for the service without parameter.           *)
(* It displays a form where you can write an integer value: *)

let calc_handler () () =
  let create_form intname =
    [p [pcdata "Write a number: ";
        Html.D.Form.input ~input_type:`Text ~name:intname
          Html.D.Form.int;
        br ();
        Html.D.Form.input ~input_type:`Submit ~value:"Send"
          Html.D.Form.string]]
  in
  let f = Html.D.Form.get_form calc_i create_form in
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

let calc_i_handler i () =
  let create_form is =
    (fun entier ->
       [p [pcdata (is^" + ");
           Form.input ~input_type:`Text ~name:entier Form.int;
           br ();
           Form.input ~input_type:`Submit ~value:"Sum" Form.string]])
  in
  let is = string_of_int i in
  let calc_result =
    register_coservice ~scope:Eliom_common.default_session_scope
      ~fallback:calc
      ~get_params:(int "j")
      (fun j () ->
        let js = string_of_int j in
        let ijs = string_of_int (i+j) in
        return
          (html
             (head (title (pcdata "")) [])
             (body
                [p [pcdata (is^" + "^js^" = "^ijs)]])))
  in
  let f = Form.get_form calc_result (create_form is) in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register calc   calc_handler;
  Eliom_registration.Html.register calc_i calc_i_handler
(*wiki*


*wiki*)
(************************************************************)
(************ Connection of users, version 3 ****************)
(************************************************************)

(*zap* *)
let connect_example3_scope_hierarchy = Eliom_common.create_scope_hierarchy "connect_example3"
let session = `Session connect_example3_scope_hierarchy
let session_group = `Session_group connect_example3_scope_hierarchy
let my_table = Eliom_state.create_volatile_table (*zap* *) ~scope:session (* *zap*) ()
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example3 =
  Eliom_service.Http.service
    ~path:["action"]
    ~get_params:Eliom_parameter.unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connect3"
    ~post_params:(Eliom_parameter.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnect3"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard (*zap* *) ~scope:session (* *zap*) ())


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s
                    Html.D.Form.string]]) ()

let login_box () =
  Html.D.Form.post_form connect_action
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Html.D.Form.input
                     ~input_type:`Text ~name:loginname
                     Html.D.Form.string]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "connect_example3" service (main page):    *)

let connect_example3_handler () () =
  let sessdat = Eliom_state.get_volatile_data ~table:my_table () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_state.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box "Close session"]
          | Eliom_state.Data_session_expired
          | Eliom_state.No_data -> [login_box ()]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  lwt () = Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () in
  Eliom_state.set_volatile_data ~table:my_table login;
  return ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:connect_example3 connect_example3_handler;
  Eliom_registration.Action.register ~service:connect_action connect_action_handler
(*wiki*


*wiki*)
let divpage =
  Eliom_registration.Flow5.create
    ~path:(Eliom_service.Path ["div"])
    ~meth:(Eliom_service.Get unit)
    (fun () () ->
      return
        [div [h2 [pcdata "Hallo"];
              p [pcdata "Blablablabla"] ]])
(*wiki*


*wiki*)
let redir1 =
  Eliom_registration.Redirection.create
    ~path:(Eliom_service.Path ["redir"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    (fun () () -> Lwt.return (Eliom_registration.Redirection coucou))
(*wiki*

 *wiki*)
let redir =
  Eliom_registration.Redirection.create
    ~path:(Eliom_service.Path ["redir"])
    ~meth:(Eliom_service.Get (int "o"))
    (fun o () ->
       Lwt.return
         (Eliom_registration.Redirection
            (Eliom_service.preapply coucou_params (o,(22,"ee")))))
(*wiki*


*wiki*)
let send_any =
  Eliom_registration.Any.create
    ~path:(Eliom_service.Path ["sendany"])
    ~meth:(Eliom_service.Get (string "type"))
    (fun s () ->
       if s = "valid"
       then
         Eliom_registration.Html.send
           (html
              (head (title (pcdata "")) [])
              (body [p [pcdata
                          "This page has been statically typechecked.
                         If you change the parameter in the URL you will get an unchecked text page"]]))
       else
         Eliom_registration.Html_text.send
           "<html><body><p>It is not a valid page. Put type=\"valid\" in the URL to get a typechecked page.</p></body></html>"
    )
(*wiki*

Cookies

*wiki*)
let cookiename = "mycookie"

let cookies = Http.service ["cookies"] unit ()

let _ = Eliom_registration.Html.register cookies
  (fun () () ->
    Eliom_state.set_cookie
      ~name:cookiename ~value:(string_of_int (Random.int 100)) ();
    Lwt.return
      (html
         (head (title (pcdata "")) [])
         (body [p [pcdata (try
                             "cookie value: "^
                               (CookiesTable.find
                                  cookiename (Eliom_request_info.get_cookies ()))
                           with _ -> "<cookie not set>");
                   br ();
                   a cookies [pcdata "send other cookie"] ()]])))
(*wiki*




*wiki*)
let mystore = Ocsipersist.Store.open_store "eliomexamplestore2"

let count2 =
  let next =
    let cthr =
      mystore >>= fun store ->
      Ocsipersist.Store.make_persistent store "countpage" 0 in
    let mutex = Lwt_mutex.create () in
    (fun () ->
      cthr >>= fun c ->
      Lwt_mutex.lock mutex >>= fun () ->
      Ocsipersist.Store.get c >>= fun oldc ->
      let newc = oldc + 1 in
      Ocsipersist.Store.set c newc >>= fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return newc)
  in
  register_service
    ~path:["count2"]
    ~get_params:unit
    (fun () () ->
      next () >>=
      (fun n ->
        return
         (html
          (head (title (pcdata "counter")) [])
          (body [p [pcdata (string_of_int n)]]))))

(*wiki*


*wiki*)
(************************************************************)
(************ Connection of users, version 4 ****************)
(**************** (persistent sessions) *********************)
(************************************************************)

(*zap* *)
let persistent_sessions_scope_hierarchy = Eliom_common.create_scope_hierarchy "persistent_sessions"
let session = `Session persistent_sessions_scope_hierarchy
let session_group = `Session_group persistent_sessions_scope_hierarchy
let persistent_session_scope = session
(* *zap*)
let my_persistent_table =
  create_persistent_table (*zap* *) ~scope:session (* *zap*) "eliom_example_table"

(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let persist_session_example =
  Eliom_service.Http.service
    ~path:["persist"]
    ~get_params:unit
    ()

let persist_session_connect_action =
  Eliom_service.Http.post_coservice'
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
  Eliom_registration.Action.create
    ~name:"disconnect4"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard ~scope:session ())

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s
                    Html.D.Form.string]]) ()

let bad_user_key = Polytables.make_key ()
let get_bad_user table =
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired action =
  Html.D.Form.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Form.input ~input_type:`Text ~name:loginname Form.string]
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

let persist_session_example_handler () () =
  my_persistent_table >>= fun my_persistent_table ->
  Eliom_state.get_persistent_data
    ~table:my_persistent_table () >>= fun sessdat ->
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_state.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box "Close session"]
          | Eliom_state.Data_session_expired ->
              [login_box true persist_session_connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | Eliom_state.No_data ->
              [login_box false persist_session_connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))


(* ----------------------------------------------------------- *)
(* Handler for persist_session_connect_action (user logs in):  *)

let persist_session_connect_action_handler () login =
  lwt () = Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () in
  if login = "toto" (* Check user and password :-) *)
  then
    my_persistent_table >>= fun my_persistent_table ->
    Eliom_state.set_persistent_data ~table:my_persistent_table login
  else ((*zap* *)Polytables.set (Eliom_request_info.get_request_cache ()) bad_user_key true;(* *zap*)return ())


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register
    ~service:persist_session_example
    persist_session_example_handler;
  Eliom_registration.Action.register
    ~service:persist_session_connect_action
    persist_session_connect_action_handler
(*wiki*


*wiki*)
(************************************************************)
(************ Connection of users, version 6 ****************)
(************************************************************)
(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "connect_example6"
let session = `Session scope_hierarchy
let session_group = `Session_group scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example6 =
  Eliom_service.Http.service
    ~path:["action2"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connect6"
    ~post_params:(string "login")
    ()

(* new disconnect action and box:                           *)

let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnect6"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard (*zap* *) ~scope:session (* *zap*) ())

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s Form.string]]) ()


let bad_user_key = Polytables.make_key ()
let get_bad_user table =
  try Polytables.get ~table ~key:bad_user_key with Not_found -> false

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired action =
  Html.D.Form.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Form.input ~input_type:`Text ~name:loginname Form.string]
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

let connect_example6_handler () () =
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~scope:session (* *zap*) ()
  in
  let group =
    Eliom_state.get_volatile_data_session_group (*zap* *) ~scope:session (* *zap*) ()
  in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match group, status with
          | Some name, _ ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box "Close session"]
          | None, Eliom_state.Expired_state ->
              [login_box true connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | _ ->
              [login_box false connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))

(* -------------------------------------------------------- *)
(* New handler for connect_action (user logs in):           *)

let connect_action_handler () login =
  lwt () = Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () in
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_state.set_volatile_data_session_group ~set_max:4
    (*zap* *) ~scope:session (* *zap*) login;
    return ()
  end
  else begin
    Polytables.set (Eliom_request_info.get_request_cache ()) bad_user_key true;
    return ()
  end


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:connect_example6 connect_example6_handler;
  Eliom_registration.Action.register ~service:connect_action connect_action_handler

(*wiki*


*wiki*)
let disposable = Http.service ["disposable"] unit ()

let _ = register disposable
    (fun () () ->
      let disp_coservice =
        Http.coservice ~max_use:2 ~fallback:disposable ~get_params:unit ()
      in
      register ~scope:Eliom_common.default_session_scope ~service:disp_coservice
        (fun () () ->
          return
            (html
              (head (title (pcdata "")) [])
              (body [p [pcdata "I am a disposable coservice";
                        br ();
                        a disp_coservice [pcdata "Try me once again"] ()]]))
        );
      return
        (html
          (head (title (pcdata "")) [])
          (body [p [(if Eliom_request_info.get_link_too_old ()
                    then pcdata "Your link was outdated. I am the fallback. I just created a new disposable coservice. You can use it only twice."
                    else
                    pcdata "I just created a disposable coservice. You can use it only twice.");
                    br ();
                    a disp_coservice [pcdata "Try it!"] ()]])))
(*wiki*


*wiki*)
let timeout = Http.service ["timeout"] unit ()

let _ =
  let page () () =
    let timeoutcoserv =
      register_coservice ~scope:session
        ~fallback:timeout ~get_params:unit ~timeout:5.
        (fun _ _ ->
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
           a timeoutcoserv [pcdata "Try it"] (); ];
          ]))
  in
  register timeout page
(*wiki*


*wiki*)
let publiccoduringsess =
  Http.service ~path:["publiccoduringsess"] ~get_params:unit ()

let _ =
  let page () () =
    let timeoutcoserv =
      register_coservice
        ~fallback:publiccoduringsess ~get_params:unit ~timeout:5.
        (fun _ _ ->
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
           a timeoutcoserv [pcdata "Try it"] (); ];
          ]))
  in
  register publiccoduringsess page
(*wiki*


*wiki*)
let _ = Eliom_registration.set_exn_handler
   (fun e -> match e with
    | Eliom_common.Eliom_404 ->
        Eliom_registration.Html.send ~code:404
          (html
             (head (title (pcdata "")) [])
             (body [h1 [pcdata "Eliom tutorial"];
                    p [pcdata "Page not found"]]))
(*    | Eliom_common.Eliom_Wrong_parameter ->
        Eliom_registration.Html.send
          (html
             (head (title (pcdata "")) [])
             (body [h1 [pcdata "Eliom tutorial"];
                    p [pcdata "Wrong parameters"]])) *)
    | e -> fail e)
(*wiki*

  *wiki*)
let my_nl_params =
  Eliom_parameter.make_non_localized_parameters
    ~prefix:"tutoeliom"
    ~name:"mynlparams"
    (Eliom_parameter.int "a" ** Eliom_parameter.string "s")

let nlparams = Http.service ~path:["nlparams"] ~get_params:(int "i") ()

let make_body () =
  [p [a ~service:nlparams [pcdata "without nl params"] 4];
   p [a ~service:nlparams
         ~nl_params:(Eliom_parameter.add_nl_parameter
                       Eliom_parameter.empty_nl_params_set
                       my_nl_params
                       (22, "oh")
         )
         [pcdata "with nl params"]
         5];
   Form.get_form
     ~service:nlparams
     ~nl_params:(Eliom_parameter.add_nl_parameter
                   Eliom_parameter.empty_nl_params_set
                   my_nl_params
                   (22, "oh")
     )
     (fun iname ->
       [p [pcdata "form with hidden nl params";
           Html.D.Form.input
             ~input_type:`Text ~name:iname Html.D.Form.int;
           Html.D.Form.input
             ~input_type:`Submit ~value:"Send" Html.D.Form.string]]);
   Form.get_form
     ~service:nlparams
     (fun iname ->
       let (aname, sname) =
         Eliom_parameter.get_nl_params_names my_nl_params
       in
       [p [pcdata "form with nl params fields";
           Html.D.Form.input
             ~input_type:`Text ~name:iname Html.D.Form.int;
           Html.D.Form.input
             ~input_type:`Text ~name:aname Html.D.Form.int;
           Html.D.Form.input
             ~input_type:`Text ~name:sname Html.D.Form.string;
           Html.D.Form.input
             ~input_type:`Submit ~value:"Send" Html.D.Form.string]]);
  ]

let _ = register
  nlparams
  (fun i () ->
    Lwt.return
      (html
         (head (title (pcdata "")) [])
         (body ((p [pcdata "i = ";
                    strong [pcdata (string_of_int i)]])::
                   (match Eliom_parameter.get_non_localized_get_parameters
                       my_nl_params
                    with
                      | None ->
                        p [pcdata "I do not have my non localized parameters"]
                      | Some (a, s) ->
                        p [pcdata "I have my non localized parameters, ";
                           pcdata ("with values a = "^string_of_int a^
                                      " and s = "^s^".")]
                   )::make_body ())))

  )
  (*wiki*


    *wiki*)
let tonlparams = register_service
    ~path:["nlparams"]
    ~get_params:unit
    (fun () () ->
       Lwt.return
         (html
            (head (title (pcdata "")) [])
            (body (make_body ()))))


  (*wiki*


  *wiki*)
let nlparams_with_nlp =
  Eliom_service.add_non_localized_get_parameters my_nl_params nlparams
  (*wiki*

  *wiki*)
(************************************************************)
(************ Connection of users, version 5 ****************)
(************************************************************)

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "connect_example5"
let session = `Session scope_hierarchy
let session_group = `Session_group scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example5 =
  Eliom_service.Http.service
    ~path:["groups"]
    ~get_params:Eliom_parameter.unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connect5"
    ~post_params:(Eliom_parameter.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnect5"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard (*zap* *) ~scope:session (* *zap*) ())


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s
                    Html.D.Form.string]]) ()

let login_box () =
  Html.D.Form.post_form connect_action
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Html.D.Form.input
                     ~input_type:`Text ~name:loginname
                     Html.D.Form.string]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "connect_example5" service (main page):    *)

let connect_example5_handler () () =
  let sessdat = Eliom_state.get_volatile_data_session_group (*zap* *) ~scope:session (* *zap*) () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Some name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box "Close session"]
          | None -> [login_box ()]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () >>= fun () ->
  Eliom_state.set_volatile_data_session_group ~set_max:4 (*zap* *) ~scope:session (* *zap*) login;
  return ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:connect_example5 connect_example5_handler;
  Eliom_registration.Action.register ~service:connect_action connect_action_handler
(*wiki*

  *wiki*)
(************************************************************)
(********************* Group tables *************************)
(************************************************************)

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "group_tables"
let session = `Session scope_hierarchy
let session_group = `Session_group scope_hierarchy
(* *zap*)
let my_table =
  Eliom_state.create_volatile_table
    ~scope:session_group ()
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let group_tables_example =
  Eliom_service.Http.service
    ~path:["grouptables"]
    ~get_params:Eliom_parameter.unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connect7"
    ~post_params:(Eliom_parameter.string "login")
    ()

let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnectgt"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard ~scope:session ())

let disconnect_g_action =
  Eliom_registration.Action.create
    ~name:"disconnectgtg"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard ~scope:session_group ())


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box () =
  div [
    Html.D.Form.post_form disconnect_action
      (fun _ -> [p [Html.D.Form.input
                      ~input_type:`Submit ~value:"Close session"
                      Html.D.Form.string]]) ();
    Html.D.Form.post_form disconnect_g_action
      (fun _ -> [p [Html.D.Form.input
                      ~input_type:`Submit ~value:"Close group"
                      Html.D.Form.string]]) ()
  ]

let login_box () =
  Html.D.Form.post_form connect_action
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Html.D.Form.input
                     ~input_type:`Text ~name:loginname
                     Html.D.Form.string]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "group_tables_example" service (main page): *)

let group_tables_example_handler () () =
  let sessdat = Eliom_state.get_volatile_data_session_group (*zap* *) ~scope:session (* *zap*) () in
  let groupdata = Eliom_state.get_volatile_data
    ~table:my_table ()
  in
  let group_info name =
    match groupdata with
      | Eliom_state.Data_session_expired
      | Eliom_state.No_data ->
        let d = string_of_int (Random.int 1000) in
        Eliom_state.set_volatile_data ~table:my_table d;
        d
      | Eliom_state.Data d -> d
  in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Some name ->
              [p [pcdata ("Hello "^name); br ()];
               (let d = group_info name in
                p [pcdata "Your group data is: ";
                   pcdata d;
                   pcdata ". It is common to all the sessions for the same user ";
                   pcdata name;
                   pcdata ". Try with another browser!"
                  ]);
               p [pcdata "Check that all sessions with same user name share the value."];
               p [pcdata "Check that the value disappears when all sessions from the group are closed."];
               p [pcdata "Check that the all sessions are closed when clicking on \"close group\" button."];
               disconnect_box ()]
          | None -> [login_box ()]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  lwt () = Eliom_state.discard (*zap* *) ~scope:session (* *zap*) () in
  Eliom_state.set_volatile_data_session_group ~set_max:4 (*zap* *) ~scope:session (* *zap*) login;
  return ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:group_tables_example group_tables_example_handler;
  Eliom_registration.Action.register ~service:connect_action connect_action_handler






(*zap* *)

(************************************************************)
(**************** Persistent group tables *******************)
(************************************************************)

let scope_hierarchy = Eliom_common.create_scope_hierarchy "pgroup_tables"
let session = `Session scope_hierarchy
let session_group = `Session_group scope_hierarchy

(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)


let pgroup_tables_example =
  Eliom_service.Http.service
    ~path:["pgrouptables"]
    ~get_params:Eliom_parameter.unit
    ()


let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connect8"
    ~post_params:(Eliom_parameter.string "login")
    ()

let disconnect_action =
  Eliom_registration.Action.create
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () -> Eliom_state.discard ~scope:session ())

let disconnect_g_action =
  Eliom_registration.Action.create
    ~name:"pdisconnectgtg"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard ~scope:session_group ())



(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box () =
  div [
    Html.D.Form.post_form disconnect_action
      (fun _ -> [p [Html.D.Form.input
                      ~input_type:`Submit ~value:"Close session"
                      Html.D.Form.string]]) ();
    Html.D.Form.post_form disconnect_g_action
      (fun _ -> [p [Html.D.Form.input
                      ~input_type:`Submit ~value:"Close group"
                      Html.D.Form.string]]) ()
  ]

let login_box () =
  Html.D.Form.post_form connect_action
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Html.D.Form.input
                     ~input_type:`Text ~name:loginname
                     Html.D.Form.string]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "group_tables_example" service (main page): *)

let my_table =
  Eliom_state.create_persistent_table
    ~scope:session_group "pgroup_table"

let group_tables_example_handler () () =
  my_table >>= fun my_table ->
  Eliom_state.get_persistent_data_session_group ~scope:session ()
  >>= fun sessdat ->
  Eliom_state.get_persistent_data ~table:my_table ()
  >>= fun groupdata ->
  let group_info name =
    match groupdata with
      | Eliom_state.Data_session_expired
      | Eliom_state.No_data ->
        let d = string_of_int (Random.int 1000) in
        Eliom_state.set_persistent_data ~table:my_table d
        >>= fun r -> Lwt.return d
      | Eliom_state.Data d -> Lwt.return d
  in
  (match sessdat with
    | Some name ->
      (group_info name >>= fun d ->
       Lwt.return
         [p [pcdata ("Hello "^name); br ()];
          (p [pcdata "Your persistent group data is: ";
              pcdata d;
              pcdata ". It is common to all the sessions for the same user ";
              pcdata name;
              pcdata ". Try with another browser!"
             ]);
          p [pcdata "Check that all sessions with same user name share the value."];
          p [pcdata "Check that the value disappears when all sessions from the group are closed."];
          p [pcdata "Check that the all sessions are closed when clicking on \"close group\" button."];
          p [pcdata "Check that the value is preserved after relaunching the server."];
          disconnect_box ()])
    | None -> Lwt.return [login_box ()]) >>= fun l ->
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body l))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  lwt () = Eliom_state.discard ~scope:session () in
  Eliom_state.set_persistent_data_session_group
    ~set_max:(Some 4) ~scope:session login


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:pgroup_tables_example group_tables_example_handler;
  Eliom_registration.Action.register ~service:connect_action connect_action_handler

(* *zap*)
(*wiki*

  *wiki*)

let csrf_scope_hierarchy = Eliom_common.create_scope_hierarchy "csrf"
let csrf_scope = `Session csrf_scope_hierarchy

let csrfsafe_example =
  Eliom_service.Http.service
    ~path:["csrf"]
    ~get_params:Eliom_parameter.unit
    ()

let csrfsafe_example_post =
  Eliom_service.Http.post_coservice
    ~csrf_safe:true
    ~csrf_scope
    ~csrf_secure:true
    ~timeout:10.
    ~max_use:1
    ~https:true
    ~fallback:csrfsafe_example
    ~post_params:Eliom_parameter.unit
    ()

let _ =
  let page () () =
    let l3 = Html.D.Form.post_form csrfsafe_example_post
        (fun _ -> [p [Html.D.Form.input
                        ~input_type:`Submit
                        ~value:"Click" Html.D.Form.string]]) ()
    in
    return
      (html
       (head (title (pcdata "CSRF safe service example")) [])
       (body [p [pcdata "A new coservice will be created each time this form is displayed"];
              l3]))
  in
  Eliom_registration.Html.register csrfsafe_example page;
  Eliom_registration.Html.register csrfsafe_example_post
    (fun () () ->
       Lwt.return
         (html
            (head (title (pcdata "CSRF safe service")) [])
            (body [p [pcdata "This is a CSRF safe service"]])))

(*wiki*



%<code language="ocaml"|
let r = Pcre.regexp "\\\\[(.*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@)\\\\]"

let regexp =
  Eliom_registration.Html.register_service
    ~path:["regexp"]
    ~get_params:(regexp r "$1" "myparam")
    (fun g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata g]])))

>%

*wiki*)
(*zap* *)
let myregexp = Pcre.regexp "\\[(.*)\\]"

let regexpserv =
  register_service
    ~path:["regexp"]
    ~get_params:(regexp myregexp "\\1" (fun s -> s) "myparam")
    (fun g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata g]])))
(* *zap*)
(*wiki*

*wiki*)
(* Form with bool checkbox: *)
let bool_params = register_service
    ~path:["bool"]
    ~get_params:(bool "case")
  (fun case () ->
    let module Html = Eliom_content.Html.F in
    return
    <:html< <html>
         <head><title></title></head>
         <body>
         <p>
           $pcdata (if case then "checked" else "not checked")$
         </p>
         </body>
       </html> >>)

let create_form_bool casename =
    let module Html = Eliom_content.Html.F in
    <:htmllist< <p>check? $Form.bool_checkbox_one ~name:casename ()$ <br/>
      $Form.input ~input_type:`Submit ~value:"Click" Form.string$</p> >>

let form_bool = register_service ["formbool"] unit
  (fun () () ->
    let module Html = Eliom_content.Html.F in
     let f = Form.get_form bool_params create_form_bool in
     return
     <:html< <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)


(*wiki*


*wiki*)

let set = register_service
    ~path:["set"]
    ~get_params:(set string "s")
  (fun l () ->
    let module Html = Eliom_content.Html.F in
    let ll =
      List.map
        (fun s -> <:html< <strong>$str:s$ </strong> >>) l
    in
    let module Html = Eliom_content.Html.F in
    return
    <:html< <html>
         <head><title></title></head>
         <body>
         <p>
           You sent:
           $list:ll$
         </p>
         </body>
       </html> >>)
(*wiki*


*wiki*)

(* form to set *)
let setform = register_service
    ~path:["setform"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Set Form"];
                  Form.get_form set
                    (fun n ->
                      [p [pcdata "Form to set: ";
                          Form.checkbox ~name:n ~value:"box1" Form.string;
                          Form.checkbox
                            ~name:n ~value:"box2" ~checked:true Form.string;
                          Form.checkbox ~name:n ~value:"box3" Form.string;
                          Form.checkbox ~name:n ~value:"box4" Form.string;
                          Form.input ~input_type:`Submit ~value:"Click"
                            Form.string]])
                ])))
(*wiki*


*wiki*)
let select_example_result = register_service
    ~path:["select"]
    ~get_params:(string "s")
    (fun g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata "You selected: ";
                     strong [pcdata g]]])))

let create_select_form =
  (fun select_name ->
    [p [pcdata "Select something: ";
        Html.D.Form.select ~name:select_name
          Html.D.Form.string
          (Html.D.Form.Option ([] (* attributes *),
                                        "Bob" (* value *),
                                        None (* Content, if different from value *),
                                        false (* not selected *))) (* first line *)
          [Html.D.Form.Option ([], "Marc", None, false);
          (Html.D.Form.Optgroup
          ([],
           "Girls",
           ([], "Karin", None, false),
           [([a_disabled ()], "Juliette", None, false);
            ([], "Alice", None, true);
            ([], "Germaine", Some (pcdata "Bob's mother"), false)]))]
          ;
          Html.D.Form.input
            ~input_type:`Submit ~value:"Send"
            Html.D.Form.string]])

let select_example = register_service ["select"] unit
  (fun () () ->
     let f =
       Html.D.Form.get_form
         select_example_result create_select_form
     in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))
(*wiki*


*wiki*)
let coord = register_service
    ~path:["coord"]
    ~get_params:(coordinates "coord")
  (fun c () ->
    let module Html = Eliom_content.Html.F in
    return
  <:html< <html>
       <head><title></title></head>
       <body>
       <p>
         You clicked on coordinates:
         ($str:(string_of_int c.abscissa)$, $str:(string_of_int c.ordinate)$)
       </p>
       </body>
     </html> >>)

(* form to image *)
let imageform = register_service
    ~path:["imageform"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Image Form"];
                  Form.get_form coord
                    (fun n ->
                      [p [Form.image_input
                            ~src:(make_uri ~service:(static_dir ()) ["ocsigen5.png"])
                            ~name:n
                            ()]])
                ])))
(*wiki*


*wiki*)
let coord2 = register_service
    ~path:["coord2"]
    ~get_params:(coordinates "coord")
  (fun c () ->
    let module Html = Eliom_content.Html.F in
    return
  <:html< <html>
       <head><title></title></head>
       <body>
       <p>
         You clicked on coordinates:
         ($str:(string_of_int c.abscissa)$, $str:(string_of_int c.ordinate)$)
       </p>
       </body>
     </html> >>)

(*Wiki*

*wiki*)

(* lists *)
let coucou_list = register_service
    ~path:["coucou"]
    ~get_params:(list "a" (string "str"))
  (fun l () ->
    let module Html = Eliom_content.Html.F in
    let ll =
      List.map (fun s -> <:html< <strong>$str:s$</strong> >>) l in
      return
        <:html< <html>
             <head><title></title></head>
             <body>
             <p>
               You sent:
               $list:ll$
             </p>
             </body>
           </html> >>)
(*wiki*


*wiki*)
(*zap* Note:
   Actually almost all services will be overwritten by new versions,
   but not those with user_type parameters for example
   (because the type description contains functions)
 *zap*)

(* Form with list: *)
let create_listform f =
  (* Here, f.it is an iterator like List.map,
     but it must be applied to a function taking 3 arguments
     (unlike 1 in map), the first one being the name of the parameter,
     and the second one the element of list.
     The last parameter of f.it is the code that must be appended at the
     end of the list created
   *)
  let module Html = Eliom_content.Html.F in
  f.it (fun stringname v init ->
    <:htmllist< <p>Write the value for $str:v$:
      $Form.input ~input_type:`Text ~name:stringname Form.string$ </p> >>@init)
    ["one";"two";"three";"four"]
    <:htmllist< <p>$Form.input ~input_type:`Submit ~value:"Click" Form.string$</p> >>

let listform = register_service ["listform"] unit
  (fun () () ->
     let module Html = Eliom_content.Html.F in
     let f = Form.get_form coucou_list create_listform in
     return
      <:html< <html>
           <head><title></title></head>
           <body> $f$ </body>
         </html> >>)

(*wiki*


*wiki*)
(* Form for service with suffix: *)
(* let create_suffixform ((suff, endsuff),i) = *)
(*   let module Html = Eliom_content.Html.F in *)
(*   <:htmllist< *)
(*     <p> *)
(*       Write the suffix (integer): *)
(*       $Form.input ~input_type:`Text ~name:suff Form.int$ *)
(*       <br/> *)
(*       Write a string: *)
(*       $user_type_input (Ocsigen_lib.Url.string_of_url_path ~encode:false) ~input_type:`Text ~name:endsuff ()$ *)
(*       <br/> *)
(*       Write an int: $int_input ~input_type:`Text ~name:i ()$ *)
(*       <br/> *)
(*       $string_input ~input_type:`Submit ~value:"Click" ()$ *)
(*     </p> *)
(*   >> *)

(* let suffixform = register_service ["suffixform"] unit *)
(*   (fun () () -> *)
(*      let f = Form.get_form isuffix create_suffixform in *)
(*      let module Html = Eliom_content.Html.F in *)
(*      return *)
(*       <:html< <html> *)
(*            <head><title></title></head> *)
(*            <body> $f$ </body> *)
(*          </html> >>) *)

(*wiki*


*wiki*)
let upload = Http.service
    ~path:["upload"]
    ~get_params:unit
    ()

let upload2 = register_post_service
   ~fallback:upload
   ~post_params:(file "file")
    (fun () file ->
      let to_display =
        let newname = "/tmp/thefile" in
        (try
          Unix.unlink newname;
        with _ -> ());
        Lwt_log.ign_debug (Eliom_request_info.get_tmp_filename file);
        Unix.link (Eliom_request_info.get_tmp_filename file) newname;
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
    (fun () () ->
      let f =
        (Form.post_form upload2
           (fun file ->
             [p [Form.file_input ~name:file ();
                 br ();
                 Form.input ~input_type:`Submit ~value:"Send" Form.string
               ]]) ()) in
      return
        (html
           (head (title (pcdata "form")) [])
           (body [f])))


(*wiki*


*wiki*)
(* Hierarchical menu *)
open Eliom_tools

let hier1 = Http.service ~path:["hier1"] ~get_params:unit ()
let hier2 = Http.service ~path:["hier2"] ~get_params:unit ()
let hier3 = Http.service ~path:["hier3"] ~get_params:unit ()
let hier4 = Http.service ~path:["hier4"] ~get_params:unit ()
let hier5 = Http.service ~path:["hier5"] ~get_params:unit ()
let hier6 = Http.service ~path:["hier6"] ~get_params:unit ()
let hier7 = Http.service ~path:["hier7"] ~get_params:unit ()
let hier8 = Http.service ~path:["hier8"] ~get_params:unit ()
let hier9 = Http.service ~path:["hier9"] ~get_params:unit ()
let hier10 = Http.service ~path:["hier10"] ~get_params:unit ()

let mymenu : _ hierarchical_site =
  (
   (Main_page (Eliom_tools.Srv hier1)),

   [([pcdata "page 1"], Site_tree (Main_page (Eliom_tools.Srv hier1), []));

    ([pcdata "page 2"], Site_tree (Main_page (Eliom_tools.Srv hier2), []));

    ([pcdata "submenu 4"],
     Site_tree
       (Default_page (Eliom_tools.Srv hier4),
         [([pcdata "submenu 3"],
          Site_tree
             (Not_clickable,
              [([pcdata "page 3"], Site_tree (Main_page (Eliom_tools.Srv hier3), []));
               ([pcdata "page 4"], Site_tree (Main_page (Eliom_tools.Srv hier4), []));
               ([pcdata "page 5"], Site_tree (Main_page (Eliom_tools.Srv hier5), []))]
             )
          );

          ([pcdata "page 6"], Site_tree (Main_page (Eliom_tools.Srv hier6), []))]
       )
    );

    ([pcdata "page 7"],
     Site_tree (Main_page (Eliom_tools.Srv hier7), []));

    ([pcdata "disabled"], Disabled);

    ([pcdata "submenu 8"],
     Site_tree
       (Main_page (Eliom_tools.Srv hier8),
        [([pcdata "page 9"], Site_tree (Main_page (Eliom_tools.Srv hier9), []));
         ([pcdata "page 10"], Site_tree (Main_page (Eliom_tools.Srv hier10), []))]
       )
    )
  ]
  )

let f i s () () =
  return
    (html
       (head (title (pcdata ""))
          ((style ~a:[a_mime_type "text/css"]
             [cdata_style
 "a {color: red;}\n
  li.eliomtools_current > a {color: blue;}\n
  .breadthmenu li {\n
    display: inline;\n
    padding: 0px 1em;\n
    margin: 0px;\n
    border-right: solid 1px black;}\n
  .breadthmenu li.eliomtools_last {border: none;}\n
                "])::
                Eliom_tools.F.structure_links mymenu ~service:s ())
             )
       (body [h1 [pcdata ("Page "^string_of_int i)];
              h2 [pcdata "Depth first, whole tree:"];
              div
                (Eliom_tools.F.hierarchical_menu_depth_first
                   ~whole_tree:true mymenu ~service:s ());
              h2 [pcdata "Depth first, only current submenu:"];
              div (Eliom_tools.F.hierarchical_menu_depth_first mymenu ~service:s ());
              h2 [pcdata "Breadth first:"];
              div
                (Eliom_tools.F.hierarchical_menu_breadth_first
                   ~classe:["breadthmenu"] mymenu ~service:s ())]))


let _ =
  register hier1 (f 1 hier1);
  register hier2 (f 2 hier2);
  register hier3 (f 3 hier3);
  register hier4 (f 4 hier4);
  register hier5 (f 5 hier5);
  register hier6 (f 6 hier6);
  register hier7 (f 7 hier7);
  register hier8 (f 8 hier8);
  register hier9 (f 9 hier9);
  register hier10 (f 10 hier10)
