(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomexamples.ml
 * Copyright (C) 2007 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)


(* Other examples for Eliom, and various tests *)

open Lwt
open Eliom_parameters
open XHTML5.M

(*****************************************************************************)

(************************************************************)
(****************** Connection of users *********************)
(************************************************************)
(*zap* *)
let state_name = "connect_example_state"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example =
  Eliom_services.service
    ~path:["connect_example"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_services.post_coservice'
    ~name:"connection"
    ~post_params:(string "login")
    ()

(* disconnect action and box:                               *)

let disconnect_action =
  Eliom_output.Action.register_post_coservice'
    ~name:"disconnection"
    ~post_params:Eliom_parameters.unit
    (fun () () ->
      Eliom_state.close_session (*zap* *) ~state_name (* *zap*) ())

let disconnect_box s =
  Eliom_output.Xhtml5.post_form disconnect_action
    (fun _ -> [p [Eliom_output.Xhtml5.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

(* The following eref is true if the connection has action failed: *)
let bad_user = Eliom_references.eref (*zap* *) ~state_name (* *zap*) ~scope:`Request false

(* The following eref is the name of the user, when connected *)
let user = Eliom_references.eref (*zap* *) ~state_name (* *zap*) ~scope:`Session None

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired bad_u action =
  Eliom_output.Xhtml5.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Eliom_output.Xhtml5.string_input ~input_type:`Text ~name:loginname ()]
      in
      [p (if bad_u
        then (pcdata "Wrong user")::(br ())::l
        else
          if session_expired
          then (pcdata "Session expired")::(br ())::l
          else l)
      ])
    ()

(* -------------------------------------------------------- *)
(* Handler for the "connect_example" service (main page):   *)

let connect_example_handler () () =
  (* The following function tests whether the session has expired: *)
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~state_name (* *zap*) ()
  in
  Eliom_references.get bad_user >>= fun bad_u ->
  Eliom_references.get user >>= fun u ->
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body
          (match u, status with
            | Some name, _ ->
              [p [pcdata ("Hello "^name); br ()];
               disconnect_box "Close session"]
            | None, Eliom_state.Expired_state ->
              [login_box true bad_u connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
            | _ ->
              [login_box false bad_u connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))

(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  Eliom_state.close_session (*zap* *) ~state_name (* *zap*) () >>= fun () ->
  if login = "toto" (* Check user and password :-) *)
  then Eliom_references.set user (Some login)
  else Eliom_references.set bad_user true


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_output.Xhtml5.register ~service:connect_example connect_example_handler;
  Eliom_output.Action.register ~service:connect_action connect_action_handler


(*****************************************************************************)

(************************************************************)
(********* Connection of users with session groups **********)
(************************************************************)
(*zap* *)
let state_name = "session_group_example_state"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example =
  Eliom_services.service
    ~path:["sessgrp"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_services.post_coservice'
    ~name:"connection2"
    ~post_params:(string "login")
    ()

(* disconnect action and box:                               *)

let disconnect_action =
  Eliom_output.Action.register_post_coservice'
    ~name:"disconnection2"
    ~post_params:Eliom_parameters.unit
    (fun () () ->
      Eliom_state.close_session (*zap* *) ~state_name (* *zap*) ())

let disconnect_box s =
  Eliom_output.Xhtml5.post_form disconnect_action
    (fun _ -> [p [Eliom_output.Xhtml5.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

(* The following eref is true if the connection has action failed: *)
let bad_user = Eliom_references.eref (*zap* *) ~state_name (* *zap*) ~scope:`Request false

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired bad_u action =
  Eliom_output.Xhtml5.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Eliom_output.Xhtml5.string_input ~input_type:`Text ~name:loginname ()]
      in
      [p (if bad_u
        then (pcdata "Wrong user")::(br ())::l
        else
          if session_expired
          then (pcdata "Session expired")::(br ())::l
          else l)
     ])
    ()

(* -------------------------------------------------------- *)
(* Handler for the "connect_example" service (main page):   *)

let connect_example_handler () () =
  (* The following function tests whether the session has expired: *)
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~state_name (* *zap*) ()
  in
  let group =
    Eliom_state.get_volatile_data_session_group (*zap* *) ~state_name (* *zap*) ()
  in
  Eliom_references.get bad_user >>= fun bad_u ->
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body
          (match group, status with
          | Some name, _ ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box "Close session"]
          | None, Eliom_state.Expired_state ->
              [login_box true bad_u connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | _ ->
              [login_box false bad_u connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))

(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  Eliom_state.close_session (*zap* *) ~state_name (* *zap*) () >>= fun () ->
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_state.set_volatile_data_session_group ~set_max:4 (*zap* *) ~state_name (* *zap*) login;
    Eliom_output.Redirection.send Eliom_services.void_hidden_coservice'
  end
  else  
    Eliom_references.set bad_user true >>= fun () ->
    Eliom_output.Action.send ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_output.Xhtml5.register ~service:connect_example connect_example_handler;
  Eliom_output.Any.register ~service:connect_action connect_action_handler



(*****************************************************************************)

let myeref = Eliom_references.eref ~persistent:"perscount" 0

let count3 =
  let next =
    let mutex = Lwt_mutex.create () in
    (fun () ->
      Lwt_mutex.lock mutex >>= fun () ->
      Eliom_references.get myeref >>= fun oldc ->
      let newc = oldc + 1 in
      Eliom_references.set myeref newc >>= fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return newc)
  in
  Eliom_output.Xhtml5.register_service
    ~path:["count3"]
    ~get_params:unit
    (fun () () ->
      next () >>=
      (fun n ->
        Lwt.return
         (XHTML5.M.html
          (XHTML5.M.head (XHTML5.M.title (XHTML5.M.pcdata "counter")) [])
          (XHTML5.M.body [XHTML5.M.p [XHTML5.M.pcdata (string_of_int n)]]))))


(*****************************************************************************)

open Tutoeliom
open XHTML.M
open Eliom_output.Xhtmlcompact
open Eliom_output
open Eliom_services
open Eliom_state

(* Lists of lists *)

let lilists = service [ "lilists" ] unit ()

let lilists2 = service
  ["lilists2"] (list "l" (string "title" ** (list "il" (int "i")))) ()

let create_form f =
  let l =
    f.it (fun (sn, l2) v init ->
            (tr (td [pcdata ("Write a string: ")])
               ((td [string_input ~input_type:`Text ~name:sn ()])::
                   (td [pcdata ("Write integers: ")])::

                   (l2.it (fun iname v init ->
                     (td [int_input ~input_type:`Text ~name:iname ()])::init)
                      ["A"; "B"]
                      []))

            )::init)
      ["one";"two";"three"]
      []
  in
  [table (List.hd l) (List.tl l);
   p [string_input ~input_type:`Submit ~value:"Click" ()]]

let () = register lilists
  (fun () () ->
     let f = Eliom_output.Xhtml.get_form lilists2 create_form in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))

let () = register lilists2
  (fun ll () ->
     return
       (html
         (head (title (pcdata "")) [])
         (body 
            (List.map (fun (s, il) -> p (pcdata s::
                                           List.map (fun i -> pcdata (string_of_int i)) il)) ll))))


(* sums in parameters types *)

let sumserv = register_service
    ~path:["sum"]
    ~get_params:(sum (int "i") (sum (int "ii") (string "s")))
    (fun g () ->
       return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata "You sent: ";
                      strong [pcdata
                                (match g with
                                   | Inj1 i
                                   | Inj2 (Inj1 i) -> string_of_int i
                                   | Inj2 (Inj2 s) -> s) ]]])))

let create_form =
  (fun (name1, (name2, name3)) ->
    [p [
       Eliom_output.Xhtml.int_input
         ~name:name1 ~input_type:`Submit ~value:48 ();
       Eliom_output.Xhtml.int_input
         ~name:name2 ~input_type:`Submit ~value:55 ();
       Eliom_output.Xhtml.string_input
         ~name:name3 ~input_type:`Submit ~value:"plop" ();
     ]])

let sumform = register_service ["sumform"] unit
  (fun () () ->
     let f = Eliom_output.Xhtml.get_form sumserv create_form in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))


let sumform2 = service ~path:["sumform2"] ~get_params:unit ()

let sumserv = register_post_service
    ~fallback:sumform2
    ~post_params:(sum (int "i") (sum (int "ii") (string "s")))
    (fun () post ->
       return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata "You sent: ";
                      strong [pcdata
                                (match post with
                                   | Inj1 i
                                   | Inj2 (Inj1 i) -> string_of_int i
                                   | Inj2 (Inj2 s) -> s) ]]])))

let () = register sumform2
  (fun () () ->
     let f = Eliom_output.Xhtml.post_form sumserv create_form () in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))


(******)
(* unregistering services *)
let unregister_example =
  Eliom_output.Xhtml.register_service
    ~path:["unregister"]
    ~get_params:Eliom_parameters.unit
    (fun () () ->
       let s1 = Eliom_output.Xhtml.register_service
         ~path:["unregister1"]
         ~get_params:Eliom_parameters.unit
         (fun () () -> failwith "s1")
       in
       let s2 = Eliom_output.Xhtml.register_coservice
         ~fallback:s1
         ~get_params:Eliom_parameters.unit
         (fun () () -> failwith "s2")
       in
       let s3 = Eliom_output.Xhtml.register_coservice'
         ~get_params:Eliom_parameters.unit
         (fun () () -> failwith "s3")
       in
       Eliom_output.Xhtml.register ~scope:`Session
         ~service:s1
         (fun () () -> failwith "s4");
       Eliom_services.unregister s1;
       Eliom_services.unregister s2;
       Eliom_services.unregister s3;
       Eliom_services.unregister ~scope:`Session s1;
       Lwt.return
         (html
            (head (title (pcdata "Unregistering services")) [])
            (body [p [pcdata
                        "These services have been registered and unregistered"];
                   p [a s1 [pcdata "regular service"] ();
                      pcdata ", ";
                      a s2 [pcdata "coservice"] ();
                      pcdata ", ";
                      a s3 [pcdata "non attached coservice"] ();
                      pcdata ", ";
                      a s1 [pcdata "session service"] ();
                     ]]))
    )


(******)
(* CSRF GET *)

let csrfsafe_get_example =
  Eliom_services.service
    ~path:["csrfget"]
    ~get_params:Eliom_parameters.unit
    ()

let csrfsafe_example_get =
  Eliom_services.coservice
    ~csrf_safe:true
    ~timeout:10.
    ~fallback:csrfsafe_get_example
    ~get_params:Eliom_parameters.unit
    ()

let _ =
  let page () () =
    let l3 = Eliom_output.Xhtml.get_form csrfsafe_example_get
        (fun _ -> [p [Eliom_output.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"Click" ()]])
    in
    return
      (html
       (head (title (pcdata "CSRF safe service example")) [])
       (body [p [pcdata "A new coservice will be created each time this form is displayed"];
              l3]))
  in
  Eliom_output.Xhtml.register csrfsafe_get_example page;
  Eliom_output.Xhtml.register csrfsafe_example_get
    (fun () () ->
       Lwt.return
         (html
            (head (title (pcdata "CSRF safe service")) [])
            (body [p [pcdata "This is a GET CSRF safe service"]])))

(******)
(* CSRF POST on CSRF GET coservice *)

let csrfsafe_postget_example =
  Eliom_services.service
    ~path:["csrfpostget"]
    ~get_params:Eliom_parameters.unit
    ()

let csrfsafe_example_post =
  Eliom_services.post_coservice
    ~csrf_safe:true
    ~timeout:10.
    ~fallback:csrfsafe_example_get (* !!! *)
    ~post_params:Eliom_parameters.unit
    ()

let _ =
  let page () () =
    let l3 = Eliom_output.Xhtml.post_form csrfsafe_example_post
        (fun _ -> [p [Eliom_output.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"Click" ()]]) ()
    in
    return
      (html
       (head (title (pcdata "CSRF safe service example")) [])
       (body [p [pcdata "A new coservice will be created each time this form is displayed"];
              l3]))
  in
  Eliom_output.Xhtml.register csrfsafe_postget_example page;
  Eliom_output.Xhtml.register csrfsafe_example_post
    (fun () () ->
       Lwt.return
         (html
            (head (title (pcdata "CSRF safe service")) [])
            (body [p [pcdata "This is a POST CSRF safe service, combined with a GET CSRF safe service"]])))


(******)
(* CSRF for_session *)

let csrfsafe_session_example =
  Eliom_services.service
    ~path:["csrfsession"]
    ~get_params:Eliom_parameters.unit
    ()

let csrfsafe_example_session =
  Eliom_services.post_coservice'
    ~csrf_safe:true
    ~csrf_state_name:"plop"
    ~csrf_secure:true
    ~timeout:10.
    ~post_params:Eliom_parameters.unit
    ()

let _ =
  let page () () =
    Eliom_output.Xhtml.register ~scope:`Session
      ~state_name:"plop"
      ~secure_session:true
      ~service:csrfsafe_example_session
      (fun () () ->
         Lwt.return
           (html
              (head (title (pcdata "CSRF safe service")) [])
              (body [p [pcdata "This is a POST CSRF safe service"]])));
    let l3 = Eliom_output.Xhtml.post_form csrfsafe_example_session
        (fun _ -> [p [Eliom_output.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"Click" ()]])
        ()
    in
    return
      (html
       (head (title (pcdata "CSRF safe service example")) [])
       (body [p [pcdata "A new coservice will be created each time this form is displayed"];
              l3]))
  in
  Eliom_output.Xhtml.register csrfsafe_session_example page



(******)
(* optional suffix parameters *)

let optsuf =
  register_service
    ~path:["optsuf"]
    ~get_params:(suffix(opt(string "q" ** (opt (int "i")))))
    (fun o () ->
       Lwt.return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata (match o with
                                 | None -> "<none>"
                                 | Some (s, o) ->
                                     s^(match o with
                                          | None -> "<none>"
                                          | Some i -> string_of_int i));
                     ]])))

let optsuf2 =
  register_service
    ~path:["optsuf2"]
    ~get_params:(suffix(opt(string "q") ** (opt (int "i"))))
    (fun (s, i) () ->
       Lwt.return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata (match s with
                                 | None -> "<none>"
                                 | Some s -> s);
                      pcdata (match i with
                                | None -> "<none>"
                                | Some i -> string_of_int i)];
                     ])))

(*******)
let my_nl_params =
  Eliom_parameters.make_non_localized_parameters
    ~prefix:"tutoeliom"
    ~name:"mynlp"
    (Eliom_parameters.int "a" ** Eliom_parameters.string "s")

let void_with_nlp =
  Eliom_services.add_non_localized_get_parameters
    my_nl_params Eliom_services.void_hidden_coservice'

let nlparams = service
    ~path:["voidnl"]
    ~get_params:(suffix_prod (int "year" ** int "month") (int "w" ))
    ()

let nlparams_with_nlp =
  Eliom_services.add_non_localized_get_parameters
    my_nl_params nlparams

let () = register
  nlparams
  (fun ((aa, bb), w) () ->
     Lwt.return
       (html
          (head (title (pcdata "")) [])
          (body [p [
                   a void_with_nlp
                     [pcdata "void coservice with non loc param"] ((), (11, "aa"));
                   a nlparams_with_nlp
                     [pcdata "myself with non loc param"] (((4, 5), 777), (12, "ab"))];
                 p [pcdata "I have my suffix, ";
                    pcdata ("with values year = "^string_of_int aa^
                              " and month = "^string_of_int bb^
                              ". w = "^string_of_int w^".")];
                 (match Eliom_parameters.get_non_localized_get_parameters
                    my_nl_params
                  with
                    | None ->
                        p [pcdata "I do not have my non localized parameters"]
                    | Some (a, s) ->
                        p [pcdata "I have my non localized parameters, ";
                           pcdata ("with values a = "^string_of_int a^
                                     " and s = "^s^".")]
                 )]))
    )




(*******)
(* doing requests *)
(* Warning: compute_result may return an deflated result! *)
(* Check! (see for example Eliom_output.Action) *)
let extreq =
  register_service
    ~path:["extreq"]
    ~get_params:unit
    (fun () () ->
       Ocsigen_http_client.get "ocsigen.org" "/" () >>= fun frame ->
       (match frame.Ocsigen_http_frame.frame_content with
         | None -> Lwt.return ""
         | Some stream -> Ocsigen_stream.string_of_stream (Ocsigen_config.get_maxrequestbodysizeinmemory ()) (Ocsigen_stream.get stream)) >>= fun s ->
       (* Here use an XML parser,
          or send the stream directly using an appropriate Eliom_mkreg module *)
       return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata s]])))

let servreq =
  register_service
    ~path:["servreq"]
    ~get_params:unit
    (fun () () ->
       let ri = Eliom_request_info.get_ri () in
       let ri = Ocsigen_extensions.ri_of_url "tuto/" ri in
       Ocsigen_extensions.compute_result ri >>= fun result ->
       let stream = fst result.Ocsigen_http_frame.res_stream in
       Ocsigen_stream.string_of_stream (Ocsigen_config.get_maxrequestbodysizeinmemory ()) (Ocsigen_stream.get stream) >>= fun s ->
       (* Here use an XML parser,
          or send the stream directly using an appropriate Eliom_mkreg module *)
       return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata s]])))

let servreqloop =
  register_service
    ~path:["servreqloop"]
    ~get_params:unit
    (fun () () ->
       let ri = Eliom_request_info.get_ri () in
       Ocsigen_extensions.compute_result ri >>= fun result ->
       let stream = fst result.Ocsigen_http_frame.res_stream in
       Ocsigen_stream.string_of_stream (Ocsigen_config.get_maxrequestbodysizeinmemory ()) (Ocsigen_stream.get stream) >>= fun s ->
       (* Here use an XML parser,
          or send the stream directly using an appropriate Eliom_mkreg module *)
       return
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata s]])))





(* Customizing HTTP headers *)
let headers =
  register_service
    ~code:666
    ~charset:"plopcharset"
(*    ~content_type:"custom/contenttype" *)
    ~headers:(Http_headers.add
                (Http_headers.name "XCustom-header")
                "This is an example"
                Http_headers.empty)
    ~path:["httpheaders"]
    ~get_params:unit
    (fun () () ->
      Eliom_state.set_cookie
        ~path:[] ~name:"Customcookie" ~value:"Value" ~secure:true ();
      Eliom_state.set_cookie
        ~path:[] ~name:"Customcookie2" ~value:"Value2" ();
      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Look at my HTTP headers"]])))


(* form towards a suffix service with constants *)
let create_form (n1, (_, n2)) =
    <:xmllist< <p>
      $string_input ~input_type:`Text ~name:n1 ()$
      $string_input ~input_type:`Text ~name:n2 ()$
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let constform = register_service ["constform"] unit
  (fun () () ->
     let f = get_form Tutoeliom.constfix create_form in
     return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))


(* Suffix and other service at same URL *)
let su2 =
  register_service
    ~path:["fuffix";""]
    ~get_params:(suffix (all_suffix_string "s"))
    (fun s () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1
                   [pcdata s];
                 p [pcdata "Try page fuffix/a/b"]])))

let su =
  register_service
    ~path:["fuffix";"a";"b"]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Try another suffix"]])))

let su3 =
  register_service
    ~path:["fuffix";""]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Try another suffix"]])))

let su4 =
  register_service
    ~path:["fuffix";""]
    ~get_params:(suffix (string "s" ** suffix_const "CONST" ** string "ss"))
    ~priority:1
    (fun (s, ((), ss)) () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1
                   [pcdata s];
                 p [pcdata "I am a suffix service with a constant part, registered after the generic suffix service, but I have a priority, so that you can see me!"]])))

let create_suffixform_su2 s =
    <:xmllist< <p>Write a string:
      $string_input ~input_type:`Text ~name:s ()$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let suffixform_su2 = register_service ["suffixform_su2"] unit
  (fun () () ->
     let f = get_form su2 create_suffixform_su2 in
     return
       (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

(* optional parameters *)
let optparam =
  register_service
    ~path:["opt"]
    ~get_params:(Eliom_parameters.opt (Eliom_parameters.string "a" **
                                         Eliom_parameters.string "b"))
    (fun o () ->
      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo!"];
                  match o with
                    | None -> p [pcdata "no parameters"]
                    | Some (a, b) -> p [pcdata a;
                                        pcdata ", ";
                                        pcdata b]
                 ]))

    )

let optform =
  register_service
    ~path:["optform"]
    ~get_params:unit
    (fun () () ->
(* testing lwt_get_form *)
       Eliom_output.Xhtml.lwt_get_form
         ~service:optparam
         (fun (an, bn) ->
            Lwt.return
              [p [
                 string_input ~input_type:`Text ~name:an ();
                 string_input ~input_type:`Text ~name:bn ();
                 Eliom_output.Xhtml.string_input
                   ~input_type:`Submit
                   ~value:"Click" ()]])
      >>= fun form ->
      let form =
        (form : Xhtmltypes.form XHTML.M.elt :> [> Xhtmltypes.form ] XHTML.M.elt)
      in
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo!"];
                  form
                 ]))

 )


(* Preapplied service with suffix parameters *)

let presu_service =
  register_service
    ~path: ["preappliedsuffix2"]
    ~get_params: (suffix (int "i"))
    (fun i () ->
      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [p [ pcdata ("You sent: " ^ (string_of_int i))]])))


let creator_handler () () =
  let create_form () =
    [fieldset [string_input ~input_type:`Submit ~value:"Click" ()]] in
  let myservice = preapply presu_service 10 in
  let myform = get_form myservice create_form in
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body   [
        p [pcdata "Form with preapplied parameter:"];
        myform;
        p [a myservice [pcdata "Link with preapplied parameter"] ()]
      ]))

let preappliedsuffix =
  register_service
    ~path: ["preappliedsuffix"]
    ~get_params: unit
    creator_handler


(* URL with ? or / in data or paths *)

let url_encoding =
  register_service
    ~path:["urlencoding&à/=é?ablah"]
    ~get_params:(suffix_prod (all_suffix "s//\\à") any)
    (fun (suf, l) () ->
      let ll =
        List.map
          (fun (a,s) -> << <strong>($str:a$, $str:s$) </strong> >>) l
      in
      let sl =
        List.map
          (fun s -> << <strong>$str:s$ </strong> >>) suf
      in
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo"];
                  p sl;
                  p ll
                ])))


(* menu with preapplied services *)

let preappl = preapply coucou_params (3,(4,"cinq"))
let preappl2 = preapply uasuffix (1999,01)

let mymenu current =
  Eliom_tools.Xhtml.menu ~classe:["menuprincipal"]
    (coucou, <:xmllist< coucou >>)
    [
     (preappl, <:xmllist< params >>);
     (preappl2, <:xmllist< params and suffix >>);
   ] ~service:current ()

let preappmenu =
  register_service
    ~path:["menu"]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 mymenu coucou ])))




(* GET Non-attached coservice *)
let nonatt = coservice' ~get_params:(string "e") ()

(* GET coservice with preapplied fallback *)
(* + Non-attached coservice on a pre-applied coservice *)
(* + Non-attached coservice on a non-attached coservice *)
let f s =
  (html
     (head (title (pcdata "")) [])
     (body [h1 [pcdata s];
            p [a nonatt [pcdata "clic"] "nonon"];
            get_form nonatt
              (fun string_name ->
                [p [pcdata "Non attached coservice: ";
                    string_input ~input_type:`Text ~name:string_name ();
                    string_input ~input_type:`Submit ~value:"Click" ()]])
          ]))

let getco = register_coservice
    ~fallback:preappl
    ~get_params:(int "i" ** string "s")
    (fun (i,s) () -> return (f s))

let _ = register nonatt (fun s () -> return (f s))

let getcoex =
  register_service
    ~path:["getco"]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [p [a getco [pcdata "clic"] (22,"eee") ];
                 get_form getco
                   (fun (number_name,string_name) ->
                     [p [pcdata "Write an int: ";
                         int_input ~input_type:`Text ~name:number_name ();
                         pcdata "Write a string: ";
                         string_input ~input_type:`Text ~name:string_name ();
                         string_input  ~input_type:`Submit ~value:"Click" ()]])
               ])))


(* POST service with preapplied fallback are not possible: *)
(*
let my_service_with_post_params =
  register_post_service
    ~fallback:preappl
    ~post_params:(string "value")
    (fun () value ->  return
      (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata value]])))
*)

(* GET coservice with coservice fallback: not possible *)
(*
let preappl3 = preapply getco (777,"ooo")

let getco2 =
  register_coservice
    ~fallback:preappl3
    ~get_params:(int "i2" ** string "s2")
    (fun (i,s) () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata s]])))

*)


(* POST service with coservice fallback *)
let my_service_with_post_params =
  register_post_service
    ~fallback:getco
    ~post_params:(string "value")
    (fun (i,s) value ->  return
      (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata (s^" "^value)]])))

let postcoex = register_service ["postco"] unit
  (fun () () ->
     let f =
       (post_form my_service_with_post_params
          (fun chaine ->
            [p [pcdata "Write a string: ";
                string_input ~input_type:`Text ~name:chaine ()]])
          (222,"ooo")) in
     return
       (html
         (head (title (pcdata "form")) [])
         (body [f])))


(* action on GET attached coservice *)
let v = ref 0

let getact =
  service
    ~path:["getact"]
    ~get_params:(int "p")
    ()

let act = Action.register_coservice
    ~fallback:(preapply getact 22)
    ~get_params:(int "bip")
    (fun g p -> v := g; return ())

(* action on GET non-attached coservice on GET coservice page *)
let naact = Action.register_coservice'
    ~get_params:(int "bop")
    (fun g p -> v := g; return ())

let naunit = Unit.register_coservice'
    ~get_params:(int "bap")
    (fun g p -> v := g; return ())

let _ =
  register
    getact
    (fun aa () ->
      return
        (html
           (head (title (pcdata "getact")) [])
           (body [h1 [pcdata ("v = "^(string_of_int !v))];
                  p [pcdata ("p = "^(string_of_int aa))];
                  p [a getact [pcdata "link to myself"] 0;
                     br ();
                     a act [pcdata "an attached action to change v"]
                       (Random.int 100);
                     br ();
                     a naact [pcdata "a non attached action to change v"]
                       (100 + Random.int 100);
                     pcdata " (Actually if called after the previous one, v won't change. More precisely, it will change and turn back to the former value because the attached coservice is reloaded after action)";
                     br ();
                     a naunit [pcdata "a non attached \"Unit\" page to change v"]
                       (200 + Random.int 100);
                     pcdata " (Reload after clicking here)"
                   ]])))




(* Many cookies *)
let cookiename = "c"

let cookies = service ["c";""] (suffix (all_suffix_string "s")) ()

let _ = Eliom_output.Xhtml.register cookies
    (fun s () ->
      let now = Unix.time () in
      Eliom_state.set_cookie
        ~path:[] ~exp:(now +. 10.) ~name:(cookiename^"6")
        ~value:(string_of_int (Random.int 100)) ~secure:true ();
      Eliom_state.set_cookie
        ~path:[] ~exp:(now +. 10.) ~name:(cookiename^"7")
        ~value:(string_of_int (Random.int 100)) ~secure:true ();
      Eliom_state.set_cookie
        ~path:["c";"plop"] ~name:(cookiename^"8")
        ~value:(string_of_int (Random.int 100)) ();
      Eliom_state.set_cookie
        ~path:["c";"plop"] ~name:(cookiename^"9")
        ~value:(string_of_int (Random.int 100)) ();
      Eliom_state.set_cookie
        ~path:["c";"plop"] ~name:(cookiename^"10")
        ~value:(string_of_int (Random.int 100)) ~secure:true ();
      Eliom_state.set_cookie
        ~path:["c";"plop"] ~name:(cookiename^"11")
        ~value:(string_of_int (Random.int 100)) ~secure:true ();
      Eliom_state.set_cookie
        ~path:["c";"plop"] ~name:(cookiename^"12")
        ~value:(string_of_int (Random.int 100)) ~secure:true ();
      if Ocsigen_lib.String_Table.mem (cookiename^"1") (Eliom_request_info.get_cookies ())
      then
        (Eliom_state.unset_cookie ~name:(cookiename^"1") ();
         Eliom_state.unset_cookie ~name:(cookiename^"2") ())
      else begin
        Eliom_state.set_cookie
          ~name:(cookiename^"1") ~value:(string_of_int (Random.int 100))
          ~secure:true ();
        Eliom_state.set_cookie
          ~name:(cookiename^"2") ~value:(string_of_int (Random.int 100)) ();
        Eliom_state.set_cookie
          ~name:(cookiename^"3") ~value:(string_of_int (Random.int 100)) ()
      end;

      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [p
                     (Ocsigen_lib.String_Table.fold
                        (fun n v l ->
                          (pcdata (n^"="^v))::
                            (br ())::l
                        )
                        (Eliom_request_info.get_cookies ())
                        [a cookies [pcdata "send other cookies"] ""; br ();
                         a cookies [pcdata "send other cookies and see the url /c/plop"] "plop"]
                     )]))
    )




(* Send file *)
let sendfileex =
  register_service
    ~path:["files";""]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With a suffix, that page will send a file"]])))

let sendfile2 =
  Files.register_service
    ~path:["files";""]
    ~get_params:(suffix (all_suffix "filename"))
    (fun s () ->
      return ("/var/www/ocsigen/"^(Ocsigen_lib.string_of_url_path ~encode:false s)))

let sendfileexception =
  register_service
    ~path:["files";"exception"]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With another suffix, that page will send a file"]])))


(* Complex suffixes *)
let suffix2 =
  service
    ~path:["suffix2";""]
    ~get_params:(suffix (string "suff1" ** int "ii" ** all_suffix "ee"))
    ()

let _ =
  register suffix2
    (fun (suf1, (ii, ee)) () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "The suffix of the url is ";
                  strong [pcdata (suf1^", "^(string_of_int ii)^", "^
                                  (Ocsigen_lib.string_of_url_path ~encode:false ee))]];
              p [a suffix2 [pcdata "link to myself"] ("a", (2, []))]])))

let suffix3 =
  register_service
    ~path:["suffix3";""]
    ~get_params:(suffix_prod
                   (string "suff1" ** int "ii" **
                      all_suffix_user int_of_string string_of_int "ee")
                   (string "a" ** int "b"))
    (fun ((suf1, (ii, ee)), (a, b)) () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "The parameters in the url are ";
                  strong [pcdata (suf1^", "^(string_of_int ii)^", "^
                                  (string_of_int ee)^", "^
                                  a^", "^(string_of_int b))]]])))

let create_suffixform2 (suf1, (ii, ee)) =
    <:xmllist< <p>Write a string:
      $string_input ~input_type:`Text ~name:suf1 ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:ii ()$ <br/>
      Write a string: $user_type_input
      (Ocsigen_lib.string_of_url_path ~encode:false)
      ~input_type:`Text ~name:ee ()$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let suffixform2 = register_service ["suffixform2"] unit
  (fun () () ->
     let f = get_form suffix2 create_suffixform2 in
     return
       (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

let create_suffixform3 ((suf1, (ii, ee)), (a, b)) =
    <:xmllist< <p>Write a string:
      $string_input ~input_type:`Text ~name:suf1 ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:ii ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:ee ()$ <br/>
      Write a string: $string_input ~input_type:`Text ~name:a ()$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:b ()$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let suffixform3 = register_service ["suffixform3"] unit
  (fun () () ->
     let f = get_form suffix3 create_suffixform3 in
     return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

let suffix5 =
  register_service
    ~path:["suffix5"]
    ~get_params:(suffix (all_suffix "s"))
    (fun s () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "This is a page with suffix ";
                  strong [pcdata (Ocsigen_lib.string_of_url_path
                                    ~encode:false s)]]])))

let nosuffix =
  register_service
    ~path:["suffix5";"notasuffix"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [p [pcdata "This is a page without suffix. Replace ";
                  code [pcdata "notasuffix"];
                  pcdata " in the URL by something else."
                ]])))



(* Send file with regexp *)
let sendfileregexp =
  register_service
    ~path:["files2";""]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "With a suffix, that page will send a file"]])))

let r = Netstring_pcre.regexp "~([^/]*)(.*)"

let sendfile2 =
  Files.register_service
    ~path:["files2";""]
(*    ~get_params:(regexp r "/home/$1/public_html$2" "filename") *)
    ~get_params:(suffix ~redirect_if_not_suffix:false
                   (all_suffix_regexp r "$u($1)/public_html$2"
                      ~to_string:(fun s -> s) "filename"))
    (fun s () -> return s)

(* Here I am using redirect_if_not_suffix:false because
   otherwise I would need to write a more sophisticated to_string function *)

(*
let sendfile2 =
  Files.register_service
    ~path:["files2";""]
    ~get_params:(suffix
                   (all_suffix_regexp r "/home/$1/public_html$2" "filename"))
(*    ~get_params:(suffix (all_suffix_regexp r "$$u($1)$2" "filename")) *)
    (fun s () -> return s)
*)

let create_suffixform4 n =
    <:xmllist< <p>Write the name of the file:
      $string_input ~input_type:`Text ~name:n ()$
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let suffixform4 = register_service ["suffixform4"] unit
  (fun () () ->
     let f = get_form sendfile2 create_suffixform4 in
     return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))


(* Advanced use of any *)
let any2 = register_service
    ~path:["any2"]
    ~get_params:(int "i" ** any)
  (fun (i,l) () ->
    let ll =
      List.map
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>
         <br/>
         i = $str:(string_of_int i)$
       </p>
       </body>
     </html> >>)

(* the following will not work because s is taken in any. (not checked) *)
let any3 = register_service
    ~path:["any3"]
    ~get_params:(int "i" ** any ** string "s")
  (fun (i,(l,s)) () ->
    let ll =
      List.map
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>
         <br/>
         i = $str:(string_of_int i)$
         <br/>
         s = $str:s$
       </p>
       </body>
     </html> >>)


(* any cannot be in suffix: (not checked) *)
let any4 = register_service
    ~path:["any4"]
    ~get_params:(suffix any)
  (fun l () ->
    let ll =
      List.map
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)


let any5 = register_service
    ~path:["any5"]
    ~get_params:(suffix_prod (string "s") any)
  (fun (s, l) () ->
    let ll =
      List.map
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent <strong>$str:s$</strong> and :
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)

(* list in suffix *)
let sufli = service
    ~path:["sufli"]
    ~get_params:(suffix (list "l" (string "s" ** int "i")))
    ()

let _ = register sufli
  (fun l () ->
    let ll =
      List.map
        (fun (s, i) -> << <strong> $str:(s^string_of_int i)$ </strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>
       </p>
       <p>
         $a sufli [pcdata "myself"] [("a", 2)]$,
         $a sufli [pcdata "myself (empty list)"] []$
       </p>
       </body>
     </html> >>)

let create_sufliform f =
  let l =
    f.it (fun (sn, iname) v init ->
            (tr (td [pcdata ("Write a string: ")])
               [td [string_input ~input_type:`Text ~name:sn ()];
                td [pcdata ("Write an integer: ")];
                td [int_input ~input_type:`Text ~name:iname ()];
               ])::init)
      ["one";"two";"three"]
      []
  in
  [table (List.hd l) (List.tl l);
   p [string_input ~input_type:`Submit ~value:"Click" ()]]

let sufliform = register_service ["sufliform"] unit
  (fun () () ->
     let f = get_form sufli create_sufliform in
     return
       (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

(*
(* mmmh ... disabled dynamically for now *)
let sufli2 = service
    ~path:["sufli2"]
    ~get_params:(suffix ((list "l" (int "i")) ** int "j"))
    ()

let _ = register sufli2
  (fun (l, j) () ->
    let ll =
      List.map (fun i -> << <strong> $str:(string_of_int i)$ </strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>,
         and
         j=$str:string_of_int j$.
       </p>
       <p>
         $a sufli2 [pcdata "myself"] ([1; 2], 3)$,
         $a sufli2 [pcdata "myself (empty list)"] ([], 1)$
       </p>
       </body>
     </html> >>)
*)

let sufliopt = service
    ~path:["sufliopt"]
    ~get_params:(suffix (list "l" (opt (string "s"))))
    ()

let _ = register sufliopt
  (fun l () ->
    let ll =
      List.map
        (function None -> pcdata "<none>"
           | Some s -> << <strong> $str:s$ </strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>
       </p>
       <p>
         $a sufliopt [pcdata "myself"] [Some "a"; None; Some "po"; None; None; Some "k"; None]$,
         $a sufliopt [pcdata "myself (empty list)"] []$
         $a sufliopt [pcdata "myself (list [None; None])"] [None; None]$
         $a sufliopt [pcdata "myself (list [None])"] [None]$
       </p>
       </body>
     </html> >>)


let sufliopt2 = service
    ~path:["sufliopt2"]
    ~get_params:(suffix (list "l" (opt (string "s" ** string "ss"))))
    ()

let _ = register sufliopt2
  (fun l () ->
    let ll =
      List.map
        (function None -> pcdata "<none>"
           | Some (s, ss) -> << <strong> ($str:s$, $str:ss$) </strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>
       </p>
       <p>
         $a sufliopt2 [pcdata "myself"] [Some ("a", "jj"); None; Some ("po", "jjj"); None; None; Some ("k", "pp"); None]$,
         $a sufliopt2 [pcdata "myself (empty list)"] []$
         $a sufliopt2 [pcdata "myself (list [None; None])"] [None; None]$
         $a sufliopt2 [pcdata "myself (list [None])"] [None]$
       </p>
       </body>
     </html> >>)


(* set in suffix *)
let sufset = register_service
    ~path:["sufset"]
    ~get_params:(suffix (Eliom_parameters.set string "s"))
  (fun l () ->
    let ll =
      List.map
        (fun s -> << <strong>$str:s$</strong> >>) l
    in
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent:
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)



(* form to any2 *)
let any2form = register_service
    ~path:["any2form"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Any Form"];
                  get_form any2
                    (fun (iname,grr) ->
                      [p [pcdata "Form to any2: ";
                          int_input ~input_type:`Text ~name:iname ();
                          raw_input ~input_type:`Text ~name:"plop" ();
                          raw_input ~input_type:`Text ~name:"plip" ();
                          raw_input ~input_type:`Text ~name:"plap" ();
                          string_input ~input_type:`Submit ~value:"Click" ()]])
                ])))


(* bool list *)

let boollist = register_service
    ~path:["boollist"]
    ~get_params:(list "a" (bool "b"))
  (fun l () ->
    let ll =
      List.map (fun b ->
        (strong [pcdata (if b then "true" else "false")])) l in
    return
      (html
         (head (title (pcdata "")) [])
         (body
            [p ((pcdata "You sent: ")::ll)]
         )))

let create_listform f =
  (* Here, f.it is an iterator like List.map,
     but it must be applied to a function taking 2 arguments
     (and not 1 as in map), the first one being the name of the parameter.
     The last parameter of f.it is the code that must be appended at the
     end of the list created
   *)
  let l =
    f.it (fun boolname v init ->
            (tr (td [pcdata ("Write the value for "^v^": ")])
               [td [bool_checkbox ~name:boolname ()]])::init)
      ["one";"two";"three"]
      []
  in
  [table (List.hd l) (List.tl l);
   p [raw_input ~input_type:`Submit ~value:"Click" ()]]

let boollistform = register_service ["boolform"] unit
  (fun () () ->
     let f = get_form boollist create_listform in return
        (html
          (head (title (pcdata "")) [])
          (body [f])))


(********)


(* any with POST *)
let any = register_post_service
    ~fallback:coucou
    ~post_params:any
  (fun () l ->
    let ll =
      List.map
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l
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

(* form to any *)
let anypostform = register_service
    ~path:["anypostform"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Any Form"];
                  post_form any
                    (fun () ->
                      [p [pcdata "Empty form to any: ";
                          string_input ~input_type:`Submit ~value:"Click" ()]])
                    ()
                ])))

(**********)
(* upload *)

(* ce qui suit ne doit pas fonctionner. Mais il faudrait l'interdire *)
let get_param_service =
  register_service
   ~path:["uploadget"]
   ~get_params:(string "name" ** file "file")
    (fun (name,file) () ->
         let to_display =
           let newname = "/tmp/fichier" in
           (try
             Unix.unlink newname;
           with _ -> ());
           Unix.link (Eliom_request_info.get_tmp_filename file) newname;
           let fd_in = open_in newname in
           try
             let line = input_line fd_in in close_in fd_in; line (*end*)
           with End_of_file -> close_in fd_in; "vide"
         in
         return
            (html
                (head (title (pcdata name)) [])
                (body [h1 [pcdata to_display]])))


let uploadgetform = register_service ["uploadget"] unit
  (fun () () ->
    let f =
(* ARG        (post_form ~a:[(XHTML.M.a_enctype "multipart/form-data")] fichier2 *)
     (get_form ~a:[(XHTML.M.a_enctype "multipart/form-data")] ~service:get_param_service
     (*post_form my_service_with_post_params        *)
        (fun (str, file) ->
          [p [pcdata "Write a string: ";
              string_input ~input_type:`Text ~name:str ();
              br ();
              file_input ~name:file ()]])) in  return
         (html
           (head (title (pcdata "form")) [])
           (body [f])))


(*******)
(* Actions that raises an exception *)
let exn_act = Action.register_coservice'
    ~get_params:unit
    (fun g p -> fail Not_found)

let exn_act_main =
  register_service
    ~path:["exnact"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "exnact")) [])
           (body [h1 [pcdata "Hello"];
                  p [a exn_act [pcdata "Do the action"] ()
                   ]])))


(* close sessions from outside *)
let close_from_outside =
  register_service
    ~path:["close_from_outside"]
    ~get_params:unit
    (fun () () ->
      discard_all ~state_name:"persistent_sessions" () >>= fun () ->
      discard_all ~state_name:"action_example2" () >>= fun () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "all sessions called \"persistent_sessions\" and \"action_example2\" closed"];
                  p [a persist_session_example [pcdata "try"] ()]])))



(* setting timeouts *)
let set_timeout =
register_service
    ~path:["set_timeout"]
    ~get_params:(int "t" ** (bool "recompute" ** bool "overrideconfig"))
    (fun (t, (recompute, override_configfile)) () ->
      set_global_persistent_data_state_timeout
        ~override_configfile
        ~state_name:(Some "persistent_sessions")
        ~recompute_expdates:recompute (Some (float_of_int t));
      set_global_volatile_state_timeout
        ~override_configfile
        ~state_name:(Some "action_example2")
        ~recompute_expdates:recompute (Some (float_of_int t));
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Setting timeout"];
                  p [
                  if recompute
                  then pcdata ("The timeout for sessions called \"persistent_sessions\" and \"action_example2\" has been set to "^(string_of_int t)^" seconds (all expiration dates updated).")
                  else pcdata ("From now, the timeout for sessions called \"persistent_sessions\" and \"action_example2\" will be "^(string_of_int t)^" seconds (expiration dates not updated)."); br ();
                  a persist_session_example [pcdata "Try"] ()]])))


let create_form =
  (fun (number_name, (bool1name, bool2name)) ->
    [p [pcdata "New timeout: ";
        Eliom_output.Xhtml.int_input ~input_type:`Text ~name:number_name ();
        br ();
        pcdata "Check the box if you want to recompute all timeouts: ";
        Eliom_output.Xhtml.bool_checkbox ~name:bool1name ();
        br ();
        pcdata "Check the box if you want to override configuration file: ";
        Eliom_output.Xhtml.bool_checkbox ~name:bool2name ();
        Eliom_output.Xhtml.string_input ~input_type:`Submit ~value:"Submit" ()]])

let set_timeout_form =
  register_service
    ["set_timeout"]
    unit
    (fun () () ->
      let f = Eliom_output.Xhtml.get_form set_timeout create_form in
      return
        (html
           (head (title (pcdata "")) [])
           (body [f])))



(******************************************************************)

let sraise =
  register_service
    ~path:["raise"]
    ~get_params:unit
    (fun () () -> failwith "Bad use of exceptions")

let sfail =
  register_service
    ~path:["fail"]
    ~get_params:unit
    (fun () () -> Lwt.fail (Failure "Service raising an exception"))


(******************************************************************)
let mainpage = register_service ["tests"] unit
 (fun () () ->
   return
    (html
     (head (title (pcdata "Test"))
        [css_link (make_uri ~service:(static_dir ()) ["style.css"]) ()])
     (body
       [h1 [img ~alt:"Ocsigen" ~src:(make_uri ~service:(static_dir ()) ["ocsigen5.png"]) ()];
        h3 [pcdata "Eliom tests"];
        p
        [
         a coucou [pcdata "coucou"] (); br ();
         a sumform [pcdata "alternative parameters"] (); br ();
         a sumform2 [pcdata "alternative parameters with POST"] (); br ();
         a optform [pcdata "Optional parameters"] (); br ();
         a sfail [pcdata "Service raising an exception"] (); br ();
         a sraise [pcdata "Wrong use of exceptions during service"] (); br ();
         a getcoex [pcdata "GET coservice with preapplied fallback, etc"] (); br ();
         a postcoex [pcdata "POST service with coservice fallback"] (); br ();
         a su [pcdata "Suffix and other service at same URL"] (); br ();
         a suffixform_su2 [pcdata "Suffix and other service at same URL: a form towards the suffix service"] (); br ();
         a su4 [pcdata "Suffix service with constant part and priority"] ("aa", ((), "bb")); br ();
         a preappliedsuffix [pcdata "Preapplied suffix"] (); br ();
         a constform [pcdata "Form towards suffix service with constants"] (); br ();
         a getact [pcdata "action on GET attached coservice, etc"] 127; br ();
         a cookies [pcdata "Many cookies"] "le suffixe de l'URL"; br ();
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
         a uploadgetform [pcdata "Upload with GET"] (); br ();
         a sufli [pcdata "List in suffix"] [("bo", 4);("ba", 3);("bi", 2);("bu", 1)]; br ();
         a sufliform [pcdata "Form to list in suffix"] (); br ();
         a sufliopt [pcdata "List of optional values in suffix"] [None; Some "j"]; br ();
         a sufliopt2 [pcdata "List of optional pairs in suffix"] [None; Some ("j", "ee")]; br ();
         a sufset [pcdata "Set in suffix"] ["bo";"ba";"bi";"bu"]; br ();
(*         a sufli2 [pcdata "List not in the end of in suffix"] ([1; 2; 3], 4); br (); *)
         a boollistform [pcdata "Bool list"] (); br ();
         a lilists [pcdata "List of lists in parameters"] (); br ();
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
         a ~service:(static_dir_with_params ~get_params:Eliom_parameters.any ())
           [pcdata "Static file with GET parameters"]
           (["ocsigen5.png"], [("aa", "lmk"); ("bb", "4")]); br ();

         a extreq [pcdata "External request"] (); br ();
         a servreq [pcdata "Server request"] (); br ();
         a servreqloop [pcdata "Looping server request"] (); br ();

         a nlparams [pcdata "nl params and suffix, on void coservice"] ((3, 5), 222); br ();
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


       ]])))
