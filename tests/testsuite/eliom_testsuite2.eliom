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

module Eliom_service = Eliom_testsuite_base.Service
module Eliom_registration = Eliom_testsuite_base.Registration

open Eliom_lib
open Eliom_content
open Html.F
open Lwt
open Eliom_parameter
open Ocsigen_cookies


(*****************************************************************************)
(* Test for raw_post_data *)

let raw_post_example =
  Eliom_registration.Html.register_service
    ~path:["rawpost"]
    ~get_params:unit
    (fun () () ->
      Lwt.return
        (html
           (head (title (pcdata "raw post data")) [])
           (body [p [pcdata "It is possible to send POST data to this URL, using any content-type other than form data or multipart. Try it with telnet. Cut and paste in a terminal:"];
                  pre [pcdata "telnet localhost 8080
POST /rawpost HTTP/1.0
Content-type: plop/plop
Content-length: 124"];
                 ]))
    )

let raw_post_service =
  Eliom_registration.Html.register_post_service
    ~fallback:raw_post_example
    ~post_params:raw_post_data
    (fun () (ct, b) ->
      let ct = match ct with
        | None -> "<none>"
        | Some ((content_type1, content_type2), _) ->
          content_type1^"/"^content_type2
      in
      lwt s = Cohttp_lwt_body.to_string b in
      let s = String.sub s 0 1000 in
      Lwt.return
        (html
           (head (title (pcdata "raw post data")) [])
           (body [p [pcdata "I received POST data, with content-type = ";
                     pcdata ct;
                     pcdata ", and the first 1000 bytes of the content are:"];
                  p [pcdata s]])
        )
    )


(*****************************************************************************)

(************************************************************)
(****************** Connection of users *********************)
(************************************************************)
(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "connect_example_state"
let session = `Session scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example =
  Eliom_service.Http.service
    ~path:["connect_example"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connection"
    ~post_params:(string "login")
    ()

(* disconnect action and box:                               *)

let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnection"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard ~scope:session ())

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s Form.string]]) ()

(* The following eref is true if the connection has action failed: *)
let bad_user = Eliom_reference.eref ~scope:Eliom_common.request_scope false

(* The following eref is the name of the user, when connected *)
let user = Eliom_reference.eref ~scope:session None

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired bad_u action =
  Html.D.Form.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Html.D.Form.input ~input_type:`Text ~name:loginname
           Html.D.Form.string]
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
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~scope:session (* *zap*) ()
  in
  Eliom_reference.get bad_user >>= fun bad_u ->
  Eliom_reference.get user >>= fun u ->
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
  lwt () = Eliom_state.discard ~scope:session () in
  if login = "toto" (* Check user and password :-) *)
  then Eliom_reference.set user (Some login)
  else Eliom_reference.set bad_user true


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:connect_example connect_example_handler;
  Eliom_registration.Action.register ~service:connect_action connect_action_handler


(*****************************************************************************)

(************************************************************)
(********* Connection of users with session groups **********)
(************************************************************)
(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "session_group_example_state"
let session = `Session scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example =
  Eliom_service.Http.service
    ~path:["sessgrp"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connection2"
    ~post_params:(string "login")
    ()

(* disconnect action and box:                               *)

let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnection2"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () ->
      Eliom_state.discard ~scope:session ())

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s Html.D.Form.string]]) ()

(* The following eref is true if the connection has action failed: *)
let bad_user = Eliom_reference.eref ~scope:Eliom_common.request_scope false

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired bad_u action =
  Html.D.Form.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Html.D.Form.input ~input_type:`Text ~name:loginname
           Html.D.Form.string]
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
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~scope:session (* *zap*) ()
  in
  let group =
    Eliom_state.get_volatile_data_session_group (*zap* *) ~scope:session (* *zap*) ()
  in
  let group_size =
    Eliom_state.get_volatile_data_session_group_size ~scope:session () in
  Eliom_reference.get bad_user >>= fun bad_u ->
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body
          (match group, group_size, status with
          | Some name, Some size , _ ->
              [p [pcdata ("Hello "^name); br ();
		  pcdata (Printf.sprintf "There are %i session in this group" size); br ()];
              disconnect_box "Close session"]
          | None, _, Eliom_state.Expired_state ->
              [login_box true bad_u connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | _ ->
              [login_box false bad_u connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))

(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler () login =
  lwt () = Eliom_state.discard ~scope:session () in
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_state.set_volatile_data_session_group ~set_max:4 (*zap* *) ~scope:session (* *zap*) login;
    Eliom_registration.Redirection.send
      (Eliom_registration.Redirection Eliom_service.reload_action_hidden)
  end
  else
    Eliom_reference.set bad_user true >>= fun () ->
    Eliom_registration.Action.send ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:connect_example connect_example_handler;
  Eliom_registration.Any.register ~service:connect_action connect_action_handler



(*****************************************************************************)

let myeref = Eliom_reference.eref ~scope:`Global ~persistent:"perscount" 0

let count3 =
  let next =
    let mutex = Lwt_mutex.create () in
    (fun () ->
      Lwt_mutex.lock mutex >>= fun () ->
      Eliom_reference.get myeref >>= fun oldc ->
      let newc = oldc + 1 in
      Eliom_reference.set myeref newc >>= fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return newc)
  in
  Eliom_registration.Html.register_service
    ~path:["count3"]
    ~get_params:unit
    (fun () () ->
      next () >>=
      (fun n ->
        Lwt.return
         (Html.F.html
          (Html.F.head (Html.F.title (Html.F.pcdata "counter")) [])
          (Html.F.body [Html.F.p [Html.F.pcdata (string_of_int n)]]))))


(*****************************)
(* Volatile eliom references *)

let volatile_references =
  let page elts =
    Html.D.(
      html
        (head
           (title (pcdata "Volatile reference"))
           [])
        (body elts)
    )
  in
  let eref =
    Eliom_reference.Volatile.eref
      ~scope:Eliom_common.default_session_scope
      10
  in
  let service =
    Eliom_service.Http.service
      ~path:["volatile_reference"]
      ~get_params:Eliom_parameter.unit
      ()
  in
  let set_service =
    Eliom_registration.Html.register_post_coservice
      ~fallback:service
      ~post_params:(Eliom_parameter.int "n")
      (fun () n ->
         lwt () = Eliom_reference.set (eref :> _ Eliom_reference.eref) n in
         Lwt.return
           (page Html.D.([
             pcdata "Reference was set.";
             Html.D.a ~service [pcdata "back"] ();
           ])))
  in
  let unset_service =
    Eliom_registration.Html.register_post_coservice
      ~fallback:service
      ~post_params:Eliom_parameter.unit
      (fun () () ->
         let () = Eliom_reference.Volatile.unset eref in
         Lwt.return
           (page Html.F.([
             pcdata "Reference was unset.";
             Html.D.a ~service [pcdata "back"] ();
           ])))
  in
  Eliom_registration.Html.register
    ~service
    (fun () () ->
       let v = Eliom_reference.Volatile.get eref in
       Lwt.return
         (page Html.D.([
             h2 [pcdata "Volatile reference"];
             p [pcdata "Value is "; pcdata (string_of_int v)];
             Html.D.(
               Form.post_form ~service:set_service
                 (fun name -> [
                   Form.input ~input_type:`Text ~name Form.int;
                   Form.input ~input_type:`Submit ~value:"Set" Form.string;
                 ]) ()
             );
             Html.D.(
               Form.post_form ~service:unset_service
                 (fun () -> [
                   Form.input ~input_type:`Submit ~value:"Unset" Form.string;
                 ]) ()
             );
         ])));
  service



(*****************************)
(* Eliom references from fun *)

let reference_from_fun =
  let page elts =
    Html.D.(
      html
        (head
           (title (pcdata "Reference from fun"))
           [])
        (body elts)
    )
  in
  let eref =
    Eliom_reference.eref_from_fun
      ~scope:Eliom_common.default_session_scope
      (fun () ->
         print_endline "Eliom references from fun: init value";
         Random.int 100)
  in
  let service =
    Eliom_service.Http.service
      ~path:["reference_from_fun"]
      ~get_params:Eliom_parameter.unit
      ()
  in
  let set_service =
    Eliom_registration.Html.register_post_coservice
      ~fallback:service
      ~post_params:(Eliom_parameter.int "n")
      (fun () n ->
         lwt () = Eliom_reference.set eref n in
         Lwt.return
           (page Html.D.([
             pcdata "Reference was set.";
             Html.D.a ~service [pcdata "back"] ();
           ])))
  in
  let unset_service =
    Eliom_registration.Html.register_post_coservice
      ~fallback:service
      ~post_params:Eliom_parameter.unit
      (fun () () ->
         lwt () = Eliom_reference.unset eref in
         Lwt.return
           (page Html.D.([
             pcdata "Reference was unset.";
             Html.D.a ~service [pcdata "back"] ();
           ])))
  in
  Eliom_registration.Html.register
    ~service
    (fun () () ->
       lwt v = Eliom_reference.get eref in
       Lwt.return
         (page Html.D.([
             h2 [pcdata "Reference from fun"];
             p [pcdata "Value is "; pcdata (string_of_int v)];
             Html.D.(
               Form.post_form ~service:set_service
                 (fun name -> [
                   Form.input ~input_type:`Text ~name Form.int;
                   Form.input ~input_type:`Submit ~value:"Set" Form.string;
                 ]) ()
             );
             Html.D.(
               Form.post_form ~service:unset_service
                 (fun () -> [
                      Form.input ~input_type:`Submit ~value:"Unset" Form.string;
                 ]) ()
             );
         ])));
  service



(*****************************************************************************)

open Eliom_testsuite1
open Eliom_registration.Html
open Eliom_service
open Eliom_state

(* Lists of lists *)

let lilists = Http.service [ "lilists" ] unit ()

let lilists2 = Http.service
  ["lilists2"] (list "l" (string "title" ** (list "il" (int "i")))) ()

let create_form f =
  let l =
    f.it (fun (sn, l2) v init ->
            (tr ((td [pcdata ("Write a string: ")])
                 ::(td [Form.input ~input_type:`Text ~name:sn Form.string])
                 ::(td [pcdata ("Write integers: ")])
                 ::(l2.it (fun iname v init ->
                   (td [Form.input ~input_type:`Text ~name:iname Form.int])::init)
                      ["A"; "B"]
                      []))

            )::init)
      ["one";"two";"three"]
      []
  in
  [table l;
   p [Form.input ~input_type:`Submit ~value:"Click" Form.string]]

let () = register lilists
  (fun () () ->
     let f = Html.D.Form.get_form lilists2 create_form in
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

(* other example of list of list (mail W. Le Ferrant 2011/11/24) *)

let wlf_lists =
  Http.service [ "wlflists" ] (list "items" (list "followers" (int "follower"))) ()

let handler elements _ =
  lwt () = Lwt_log.debug "> nb of elements: %a" (fun _ e -> string_of_int (List.length e)) elements in
  Lwt.return "ok"

let _ = Eliom_registration.Html_text.register wlf_lists handler


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
       Html.D.Form.input
         ~name:name1 ~input_type:`Submit ~value:48
         Html.D.Form.int;
       Html.D.Form.input
         ~name:name2 ~input_type:`Submit ~value:55
         Html.D.Form.int;
       Html.D.Form.input
         ~name:name3 ~input_type:`Submit ~value:"plop"
         Html.D.Form.string;
     ]])

let sumform = register_service ["sumform"] unit
  (fun () () ->
     let f = Html.D.Form.get_form sumserv create_form in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))


let sumform2 = Http.service ~path:["sumform2"] ~get_params:unit ()

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
    let f = Html.D.Form.post_form sumserv create_form () in
    return
      (html
         (head (title (pcdata "")) [])
         (body [f])))


(******)
(* unregistering services *)
let unregister_example =
  Eliom_registration.Html.register_service
    ~path:["unregister"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
       let s1 = Eliom_registration.Html.register_service
         ~path:["unregister1"]
         ~get_params:Eliom_parameter.unit
         (fun () () -> failwith "s1")
       in
       let s2 = Eliom_registration.Html.register_coservice
         ~fallback:s1
         ~get_params:Eliom_parameter.unit
         (fun () () -> failwith "s2")
       in
       let s3 = Eliom_registration.Html.register_coservice'
         ~get_params:Eliom_parameter.unit
         (fun () () -> failwith "s3")
       in
       Eliom_registration.Html.register ~scope:Eliom_common.default_session_scope
         ~service:s1
         (fun () () -> failwith "s4");
       Eliom_service.unregister s1;
       Eliom_service.unregister s2;
       Eliom_service.unregister s3;
       Eliom_service.unregister ~scope:Eliom_common.default_session_scope s1;
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
  Eliom_service.Http.service
    ~path:["csrfget"]
    ~get_params:Eliom_parameter.unit
    ()

let csrfsafe_example_get =
  Eliom_service.Http.coservice
    ~csrf_safe:true
    ~timeout:10.
    ~fallback:csrfsafe_get_example
    ~get_params:Eliom_parameter.unit
    ()

let _ =
  let page () () =
    let l3 = Html.D.Form.get_form csrfsafe_example_get
        (fun _ -> [p [Html.D.Form.input
                        ~input_type:`Submit
                        ~value:"Click" Form.string]])
    in
    return
      (html
       (head (title (pcdata "CSRF safe service example")) [])
       (body [p [pcdata "A new coservice will be created each time this form is displayed. Your server must be running with HTTPS enabled."];
              l3]))
  in
  Eliom_registration.Html.register csrfsafe_get_example page;
  Eliom_registration.Html.register csrfsafe_example_get
    (fun () () ->
       Lwt.return
         (html
            (head (title (pcdata "CSRF safe service")) [])
            (body [p [pcdata "This is a GET CSRF safe service"]])))

(******)
(* CSRF POST on CSRF GET coservice *)

(* let csrfsafe_postget_example = *)
(*   Eliom_service.Http.service *)
(*     ~path:["csrfpostget"] *)
(*     ~get_params:Eliom_parameter.unit *)
(*     () *)

(* Disabling this test because fallbacks can no longer be
   coservices *)
(*
let csrfsafe_example_post =
  Eliom_service.Http.post_coservice
    ~csrf_safe:true
    ~timeout:10.
    ~fallback:csrfsafe_example_get (* !!! *)
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
  Eliom_registration.Html.register csrfsafe_postget_example page;
  Eliom_registration.Html.register csrfsafe_example_post
    (fun () () ->
       Lwt.return
         (html
            (head (title (pcdata "CSRF safe service")) [])
            (body [p [pcdata "This is a POST CSRF safe service, combined with a GET CSRF safe service"]])))
*)

(******)
(* CSRF for_session *)

let csrfsafe_session_example =
  Eliom_service.Http.service
    ~path:["csrfsession"]
    ~get_params:Eliom_parameter.unit
    ()

let myscope = (`Session (Eliom_common.create_scope_hierarchy "plop"))

let csrfsafe_example_session =
  Eliom_service.Http.post_coservice'
    ~csrf_safe:true
    ~csrf_scope:myscope
    ~csrf_secure:true
    ~timeout:10.
    ~post_params:Eliom_parameter.unit
    ()

let _ =
  let page () () =
    Eliom_registration.Html.register ~scope:myscope
      ~secure_session:true
      ~service:csrfsafe_example_session
      (fun () () ->
         Lwt.return
           (html
              (head (title (pcdata "CSRF safe service")) [])
              (body [p [pcdata "This is a POST CSRF safe service"]])));
    let l3 = Html.D.Form.post_form csrfsafe_example_session
        (fun _ -> [p [Html.D.Form.input
                        ~input_type:`Submit
                        ~value:"Click"
                        Html.D.Form.string]])
        ()
    in
    return
      (html
       (head (title (pcdata "CSRF safe service example")) [])
       (body [p [pcdata "A new coservice will be created each time this form is displayed"];
              l3]))
  in
  Eliom_registration.Html.register csrfsafe_session_example page



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
  Eliom_parameter.make_non_localized_parameters
    ~prefix:"tutoeliom"
    ~name:"mynlp"
    (Eliom_parameter.int "a" ** Eliom_parameter.string "s")

let void_with_nlp =
  Eliom_service.add_non_localized_get_parameters
    my_nl_params Eliom_service.reload_action

let hidden_void_with_nlp =
  Eliom_service.add_non_localized_get_parameters
    my_nl_params Eliom_service.reload_action_hidden

let nlparams2 = Http.service
    ~path:["voidnl"]
    ~get_params:(suffix_prod (int "year" ** int "month") (int "w" ))
    ()

let nlparams2_with_nlp =
  Eliom_service.add_non_localized_get_parameters
    my_nl_params nlparams2

let () = register
  nlparams2
  (fun ((aa, bb), w) () ->
     Lwt.return
       (html
          (head (title (pcdata "")) [])
          (body [p [
                   a void_with_nlp
                     [pcdata "void coservice with non loc param"] ((), (11, "aa"));
                   br ();
                   a hidden_void_with_nlp
                     [pcdata "void hidden coservice with non loc param"] ((), (22, "bb"));
                   br ();
                   a nlparams2_with_nlp
                     [pcdata "myself with non loc param"] (((4, 5), 777), (12, "ab"))];
                 p [pcdata "I have my suffix, ";
                    pcdata ("with values year = "^string_of_int aa^
                              " and month = "^string_of_int bb^
                              ". w = "^string_of_int w^".")];
                 (match Eliom_parameter.get_non_localized_get_parameters
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


(***********)
let nlpost_entry =
  Eliom_service.Http.service
    ~path:["nlpost"]
    ~get_params:(Eliom_parameter.unit)
    ()

let nlpost =
  Eliom_service.Http.post_coservice
    ~fallback:nlpost_entry
    ~name:"nlpost"
    ~post_params:(Eliom_parameter.unit)
    ()

let nlpost_with_nlp =
  Eliom_service.add_non_localized_get_parameters
    my_nl_params nlpost

let create_form_nl s =
  (fun () -> [Html.F.p [Html.D.Form.input
                           ~input_type:`Submit ~value:s
                           Html.D.Form.string]])

let () = Eliom_registration.Html.register nlpost
  (fun () () ->
    let nlp =
      match Eliom_parameter.get_non_localized_get_parameters my_nl_params with
        | None -> "no non localised parameter"
        | Some _ -> "some non localised parameter" in
     Lwt.return
       Html.F.(html
          (head (title (pcdata "")) [])
          (body [div [
            pcdata nlp; br();
            Html.D.Form.post_form
              nlpost_with_nlp (create_form_nl "with nl param") ((),(12, "ab"));
            Html.D.Form.post_form
              nlpost (create_form_nl "without nl param") ();
          ]])))

let () = Eliom_registration.Html.register nlpost_entry
  (fun () () ->
     Lwt.return
       Html.F.(html
          (head (title (pcdata "")) [])
          (body [div [
             Html.D.Form.post_form
               nlpost_with_nlp (create_form_nl "with nl param")
               ((),(12, "ab"));
             Html.D.Form.post_form
               nlpost (create_form_nl "without nl param") ();
          ]])))


(*******)
(* doing requests *)
(* Warning: compute_result may return an deflated result! *)
(* Check! (see for example Eliom_registration.Action) *)
let extreq =
  register_service
    ~path:["extreq"]
    ~get_params:unit
    (fun () () ->
       Cohttp_lwt_unix.Client.call
         `GET
           (Uri.of_string
              "http://ocsigen.org/ocsimoreadmin/static/ocsiwikistyle.css")
         >>= fun (_, b) ->
         let stream =
           Ocsigen_stream.of_lwt_stream
             (Cohttp_lwt_body.to_stream b)
         in
         Ocsigen_stream.string_of_stream (Ocsigen_config.get_maxrequestbodysizeinmemory ()) (Ocsigen_stream.get stream) >>= fun s ->
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
       let ri = Ocsigen_request.update ~uri:(Uri.of_string "tuto/") ri in
       Ocsigen_extensions.compute_result ri >>= fun result ->
       let stream =
         Ocsigen_response.to_cohttp result
         |> snd
         |> Cohttp_lwt_body.to_stream
         |> Ocsigen_stream.of_lwt_stream
       in
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
       let stream =
         Ocsigen_response.to_cohttp result
         |> snd
         |> Cohttp_lwt_body.to_stream
         |> Ocsigen_stream.of_lwt_stream
       in
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
    ~headers:(Cohttp.Header.init_with "XCustom-header" "This is an example")
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
  let module Html = Eliom_content.Html.F in
  <:htmllist< <p>
      $Form.input ~input_type:`Text ~name:n1 Form.string$
      $Form.input ~input_type:`Text ~name:n2 Form.string$
      $Form.input ~input_type:`Submit ~value:"Click" Form.string$</p> >>

let constform = register_service ["constform"] unit
  (fun () () ->
     let f = Form.get_form Eliom_testsuite1.constfix create_form in
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
  let module Html = Eliom_content.Html.F in
    <:htmllist< <p>Write a string:
      $Form.input ~input_type:`Text ~name:s Form.string$ <br/>
      $Form.input ~input_type:`Submit ~value:"Click" Form.string$</p> >>

let suffixform_su2 = register_service ["suffixform_su2"] unit
  (fun () () ->
     let f = Form.get_form su2 create_suffixform_su2 in
     return
       (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

(* optional parameters *)
let optparam =
  register_service
    ~path:["opt"]
    ~get_params:(Eliom_parameter.opt (Eliom_parameter.string "a" **
                                         Eliom_parameter.string "b"))
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
       Html.D.Form.lwt_get_form
         ~service:optparam
         (fun (an, bn) ->
            Lwt.return
              [p [
                 Form.input ~input_type:`Text ~name:an Form.string;
                 Form.input ~input_type:`Text ~name:bn Form.string;
                 Html.D.Form.input
                   ~input_type:`Submit
                   ~value:"Click" Html.D.Form.string]])
      >>= fun form ->
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
    [fieldset [Form.input ~input_type:`Submit ~value:"Click" Form.string]] in
  let myservice = preapply presu_service 10 in
  let myform = Form.get_form myservice create_form in
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
  let module Html = Eliom_content.Html.F in
  register_service
    ~path:["urlencoding&à/=é?abl<ah"]
    ~get_params:(suffix_prod (all_suffix "s//\\à") any)
    (fun (suf, l) () ->
      let ll =
        List.map
          (fun (a,s) -> <:html< <strong>($str:a$, $str:s$) </strong> >>) l
      in
      let sl =
        List.map
          (fun s -> <:html< <strong>$str:s$ </strong> >>) suf
      in
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata "All characters must be displayed correctly, including ampersand or unicode"];
                  p sl;
                  p ll
                ])))


(* menu with preapplied services *)

let preappl = preapply coucou_params (3,(4,"cinq"))
let preappl2 = preapply uasuffix (1999,01)

let preappmenu =
  register_service
    ~path:["menu"]
    ~get_params:unit
    (fun () () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 a ~service:coucou [pcdata "coucou"] ();
                 a ~service:preappl [pcdata "preappl"] ();
                 a ~service:preappl2 [pcdata "preappl2"] ()
                ])))




(* GET Non-attached coservice *)
let nonatt = Http.coservice' ~get_params:(string "e") ()

(* GET coservice with preapplied fallback *)
(* + Non-attached coservice on a pre-applied coservice *)
(* + Non-attached coservice on a non-attached coservice *)
let f s =
  (html
     (head (title (pcdata "")) [])
     (body [h1 [pcdata s];
            p [a nonatt [pcdata "clic"] "nonon"];
            Form.get_form nonatt
              (fun string_name ->
                [p [pcdata "Non attached coservice: ";
                    Form.input ~input_type:`Text ~name:string_name
                      Form.string;
                    Form.input ~input_type:`Submit ~value:"Click"
                      Form.string]])
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
                 Form.get_form getco
                   (fun (number_name,string_name) ->
                     [p [pcdata "Write an int: ";
                         Form.input ~input_type:`Text ~name:number_name
                           Form.int;
                         pcdata "Write a string: ";
                         Form.input ~input_type:`Text ~name:string_name
                           Form.string;
                         Form.input  ~input_type:`Submit ~value:"Click"
                           Form.string]])
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
       (Form.post_form my_service_with_post_params
          (fun chaine ->
            [p [pcdata "Write a string: ";
                Form.input ~input_type:`Text ~name:chaine Form.string]])
          (222,"ooo")) in
     return
       (html
         (head (title (pcdata "form")) [])
         (body [f])))


(* action on GET attached coservice *)
let v = ref 0

let getact =
  Http.service
    ~path:["getact"]
    ~get_params:(int "p")
    ()

let act =
  Eliom_registration.Action.create_attached_get
    ~fallback:(preapply getact 22)
    ~get_params:(int "bip")
    (fun g p -> v := g; return ())

(* action on GET non-attached coservice on GET coservice page *)
let naact =
  Eliom_registration.Action.create
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Get (int "bop"))
    (fun g p -> v := g; return ())

let naunit =
  Eliom_registration.Unit.create
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Get (int "bap"))
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



(* Attached links non-attached coservice *)

let get_coserv' =
  register_coservice'
    ~get_params:(Eliom_parameter.string "s")
    (fun s () ->
      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata "I'm a GET non-attached coservice with parameter ";
                     pcdata s;
                    ]])))

let post_coserv' =
  register_post_coservice'
    ~post_params:(Eliom_parameter.string "s")
    (fun () s ->
      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata "I'm a POST non-attached coservice with parameter ";
                     pcdata s;
                    ]])))

let attnonatt_service =
  register_service
    ~path: ["attnonatt"]
    ~get_params:unit
    (fun () () ->
       let service = Eliom_service.attach
        ~fallback:Eliom_testsuite1.coucou
        ~service:get_coserv'
        ()
      in
      let service2 = Eliom_service.attach
        ~fallback:Eliom_testsuite1.coucou
        ~service:post_coserv'
        ()
      in
      Lwt.return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata "This is an example of link to a GET non-attached coservice, with URL change (attach_coserv'): ";
                     a ~service [pcdata "click"] "LO"];
                  p [pcdata "And a form to a POST non-attached coservice:"];
                  Form.post_form ~service:service2
                    (fun n ->
                       [Form.input ~input_type:`Hidden ~name:n ~value:"toto"
                          Form.string;
                        Form.input ~input_type:`Submit ~value:"send"
                          Form.string]) ()
                 ])))






(* Many cookies *)
let cookiename = "c"

let cookies2 = Http.service ["c";""] (suffix (all_suffix_string "s")) ()

let _ = Eliom_registration.Html.register cookies2
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
      if CookiesTable.mem (cookiename^"1") (Eliom_request_info.get_cookies ())
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
                     (CookiesTable.fold
                        (fun n v l ->
                          (pcdata (n^"="^v))::
                            (br ())::l
                        )
                        (Eliom_request_info.get_cookies ())
                        [a cookies2 [pcdata "send other cookies"] ""; br ();
                         a cookies2 [pcdata "send other cookies and see the url /c/plop"] "plop"]
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
  Eliom_registration.File.create
    ~path:(Eliom_service.Path ["files";""])
    ~meth:(Eliom_service.Get (suffix (all_suffix "filename")))
    (fun s () ->
      return ("/var/www/ocsigen/"^(Url.string_of_url_path ~encode:false s)))

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
  Http.service
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
                                  (Url.string_of_url_path ~encode:false ee))]];
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
  let module Html = Eliom_content.Html.F in
  <:htmllist< <p>Write a string:
      $Form.input ~input_type:`Text ~name:suf1 Form.string$ <br/>
      Write an int: $Form.input ~input_type:`Text ~name:ii Form.int$ <br/>
      $Form.input ~input_type:`Submit ~value:"Click" Form.string$</p> >>
      (* Write a string: $user_type_input *)
      (* (Url.string_of_url_path ~encode:false) *)
      (* ~input_type:`Text ~name:ee ()$ <br/> *)

let suffixform2 = register_service ["suffixform2"] unit
  (fun () () ->
    let f = Form.get_form suffix2 create_suffixform2 in
     return
       (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"];
                 f ])))

let create_suffixform3 ((suf1, (ii, ee)), (a, b)) =
  let module Html = Eliom_content.Html.F in
    <:htmllist< <p>Write a string:
      $Form.input ~input_type:`Text ~name:suf1 Form.string$ <br/>
      Write an int: $Form.input ~input_type:`Text ~name:ii Form.int$ <br/>
      Write an int: $Form.input ~input_type:`Text ~name:ee Form.int$ <br/>
      Write a string: $Form.input ~input_type:`Text ~name:a Form.string$ <br/>
      Write an int: $Form.input ~input_type:`Text ~name:b Form.int$ <br/>
      $Form.input ~input_type:`Submit ~value:"Click" Form.string$</p> >>

let suffixform3 = register_service ["suffixform3"] unit
  (fun () () ->
     let f = Form.get_form suffix3 create_suffixform3 in
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
                  strong [pcdata (Url.string_of_url_path
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
  Eliom_registration.File.create
    ~path:(Eliom_service.Path ["files2";""])
(*    ~get_params:(regexp r "/home/$1/public_html$2" "filename") *)
    ~meth:(Eliom_service.Get
             (suffix ~redirect_if_not_suffix:false
                (all_suffix_regexp r "$u($1)/public_html$2"
                   ~to_string:(fun s -> s) "filename")))
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
  let module Html = Eliom_content.Html.F in
    <:htmllist< <p>Write the name of the file:
      $Form.input ~input_type:`Text ~name:n Form.string$
      $Form.input ~input_type:`Submit ~value:"Click" Form.string$</p> >>

let suffixform4 = register_service ["suffixform4"] unit
  (fun () () ->
     let f = Form.get_form sendfile2 create_suffixform4 in
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
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)


let any5 = register_service
    ~path:["any5"]
    ~get_params:(suffix_prod (string "s") any)
  (fun (s, l) () ->
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
         You sent <strong>$str:s$</strong> and :
         <span>$list:ll$</span>
       </p>
       </body>
     </html> >>)

(* list in suffix *)
let sufli = Http.service
    ~path:["sufli"]
    ~get_params:(suffix (list "l" (string "s" ** int "i")))
    ()

let _ = register sufli
  (fun l () ->
  let module Html = Eliom_content.Html.F in
    let ll =
      List.map
        (fun (s, i) -> <:html< <strong> $str:(s^string_of_int i)$ </strong> >>) l
    in
    return
  <:html< <html>
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
      (tr [td [pcdata ("Write a string: ")];
           td [Form.input ~input_type:`Text ~name:sn Form.string];
           td [pcdata ("Write an integer: ")];
           td [Form.input ~input_type:`Text ~name:iname Form.int];
          ])::init)
      ["one";"two";"three"]
      []
  in
  [table l;
   p [Form.input ~input_type:`Submit ~value:"Click" Form.string]]

let sufliform = register_service ["sufliform"] unit
  (fun () () ->
     let f = Form.get_form sufli create_sufliform in
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
      List.map (fun i -> <:html< <strong> $str:(string_of_int i)$ </strong> >>) l
    in
    return
  <:html< <html>
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

let sufliopt = Http.service
    ~path:["sufliopt"]
    ~get_params:(suffix (list "l" (opt (string "s"))))
    ()

let _ = register sufliopt
  (fun l () ->
  let module Html = Eliom_content.Html.F in
    let ll =
      List.map
        (function None -> pcdata "<none>"
           | Some s -> <:html< <strong> $str:s$ </strong> >>) l
    in
    return
  <:html< <html>
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


let sufliopt2 = Http.service
    ~path:["sufliopt2"]
    ~get_params:(suffix (list "l" (opt (string "s" ** string "ss"))))
    ()

let _ = register sufliopt2
  (fun l () ->
  let module Html = Eliom_content.Html.F in
    let ll =
      List.map
        (function None -> pcdata "<none>"
           | Some (s, ss) -> <:html< <strong> ($str:s$, $str:ss$) </strong> >>) l
    in
    return
  <:html< <html>
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
    ~get_params:(suffix (Eliom_parameter.set string "s"))
  (fun l () ->
  let module Html = Eliom_content.Html.F in
    let ll =
      List.map
        (fun s -> <:html< <strong>$str:s$</strong> >>) l
    in
    return
  <:html< <html>
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
                  Form.get_form any2
                    (fun (iname,grr) ->
                      [p [pcdata "Form to any2: ";
                          Form.input ~input_type:`Text ~name:iname Form.int;
                          input ~a:[a_input_type `Text; a_name "plop"] ();
                          input ~a:[a_input_type `Text; a_name "plip"] ();
                          input ~a:[a_input_type `Text; a_name "plap"] ();
                          Form.input ~input_type:`Submit ~value:"Click"
                            Form.string]])
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
            (tr[td [pcdata ("Write the value for "^v^": ")];
                td [Form.bool_checkbox_one ~name:boolname ()]])::init)
      ["one";"two";"three"]
      []
  in
  [table l;
   p [input ~a:[a_input_type `Submit; a_value "Click"] ()]]

let boollistform = register_service ["boolform"] unit
  (fun () () ->
     let f = Form.get_form boollist create_listform in return
        (html
          (head (title (pcdata "")) [])
          (body [f])))


(********)

let coucoucou =
  register_service
    ~path:["coucoucou"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo!"]])))

(* any with POST *)
let any = register_post_service
    ~fallback:coucoucou
    ~post_params:any
  (fun () l ->
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

(* form to any *)
let anypostform = register_service
    ~path:["anypostform"]
    ~get_params:unit
    (fun () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Any Form"];
                  Form.post_form any
                    (fun () ->
                      [p [pcdata "Empty form to any: ";
                          Form.input ~input_type:`Submit ~value:"Click"
                            Form.string]])
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
(* ARG        (post_form ~a:[(Html.F.a_enctype "multipart/form-data")] fichier2 *)
     (Form.get_form ~a:[(Html.F.a_enctype "multipart/form-data")] ~service:get_param_service
     (*post_form my_service_with_post_params        *)
        (fun (str, file) ->
          [p [pcdata "Write a string: ";
              Form.input ~input_type:`Text ~name:str Form.string;
              br ();
              Form.file_input ~name:file ()]])) in  return
         (html
           (head (title (pcdata "form")) [])
           (body [f])))


(*******)
(* Actions that raises an exception *)
let exn_act = Eliom_registration.Action.create
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Get unit)
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
                  p [a exn_act [pcdata "Do the action"] ();
                     pcdata "It will raise an exception, and you will receive an error 500."
                   ]])))


let action_example2_scope =
  `Session (Eliom_common.create_scope_hierarchy "action_example2")

(* close sessions from outside *)
let close_from_outside =
  register_service
    ~path:["close_from_outside"]
    ~get_params:unit
    (fun () () ->
      lwt () = discard_all ~scope:persistent_session_scope () in
      lwt () = discard_all ~scope:action_example2_scope () in
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
        ~cookie_scope:persistent_session_scope
        ~recompute_expdates:recompute (Some (float_of_int t));
      set_global_volatile_state_timeout
        ~override_configfile
        ~cookie_scope:action_example2_scope
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
        Html.D.Form.input ~input_type:`Text ~name:number_name
          Html.D.Form.int;
        br ();
        pcdata "Check the box if you want to recompute all timeouts: ";
        Html.D.Form.bool_checkbox_one ~name:bool1name ();
        br ();
        pcdata "Check the box if you want to override configuration file: ";
        Html.D.Form.bool_checkbox_one ~name:bool2name ();
        Html.D.Form.input ~input_type:`Submit ~value:"Submit"
          Html.D.Form.string]])

let set_timeout_form =
  register_service
    ["set_timeout"]
    unit
    (fun () () ->
      let f = Html.D.Form.get_form set_timeout create_form in
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


(*****************************************************************************)

(* 2011/08/02 Vincent - Volatile group data
   removing group data or not when no session in the group?*)
(*zap* *)
open Html.F

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "session_group_data_example_state"
let session = `Session scope_hierarchy
let group = `Session_group scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example_gd =
  Eliom_service.Http.service
    ~path:["sessgrpdata"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connectiongd"
    ~post_params:(string "login")
    ()

(* disconnect action and box:                               *)

let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnectiongd"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () -> Eliom_state.discard ~scope:session ())

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s Html.D.Form.string]]) ()

(* The following eref is true if the connection has action failed: *)
let bad_user = Eliom_reference.eref ~scope:Eliom_common.request_scope false

let my_group_data = Eliom_reference.eref ~scope:group None

let change_gd =
  Eliom_registration.Action.create
    ~name:"changegd"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () -> Eliom_reference.set my_group_data (Some (1000 + Random.int 1000)))

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired bad_u action =
  Html.D.Form.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Html.D.Form.input ~input_type:`Text ~name:loginname
           Html.D.Form.string]
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
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~scope:session (* *zap*) ()
  in
  let group =
    Eliom_state.get_volatile_data_session_group (*zap* *) ~scope:session (* *zap*) ()
  in
  Eliom_reference.get bad_user >>= fun bad_u ->
  Eliom_reference.get my_group_data >>= fun my_group_data ->
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body
          (match group, status with
          | Some name, _ ->
              [p [pcdata ("Hello "^name); br ();
                  (match my_group_data with
                    | None -> pcdata "You have no group data."
                    | Some i -> pcdata ("Your group data is "^string_of_int i^"."))];
               Html.D.Form.post_form change_gd
                 (fun () -> [p [Html.D.Form.input
                                  ~input_type:`Submit
                                   ~value:"Change group data"
                                   Html.D.Form.string]]) ();
               p [pcdata "Check that several sessions have the same group data."];
               p [pcdata "Volatile group data are currently discarded when all group disappear. This is weird and not coherent with persistent group data. But I don't really see a correct use of volatile group data. Is there any? And there is a risk of memory leak if we keep them. Besides, volatile sessions are (hopefully) going to disappear soon."];
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
  lwt () = Eliom_state.discard ~scope:session () in
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_state.set_volatile_data_session_group ~set_max:4 (*zap* *) ~scope:session (* *zap*) login;
    Eliom_reference.get my_group_data >>= fun mgd ->
    (if mgd = None
     then Eliom_reference.set my_group_data (Some (Random.int 1000))
     else Lwt.return ()) >>= fun () ->
    Eliom_registration.Redirection.send
      (Eliom_registration.Redirection Eliom_service.reload_action_hidden)
  end
  else
    Eliom_reference.set bad_user true >>= fun () ->
    Eliom_registration.Action.send ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:connect_example_gd connect_example_handler;
  Eliom_registration.Any.register ~service:connect_action connect_action_handler


(*****************************************************************************)

(* 2011/08/02 Vincent - Persistent group data
   removing group data or not when no session in the group? *)
(*zap* *)
open Html.F

(*zap* *)
let scope_hierarchy = Eliom_common.create_scope_hierarchy "pers_session_group_data_example_state"
let session = `Session scope_hierarchy
let group = `Session_group scope_hierarchy
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example_pgd =
  Eliom_service.Http.service
    ~path:["psessgrpdata"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_service.Http.post_coservice'
    ~name:"connectionpgd"
    ~post_params:(string "login")
    ()

(* disconnect action and box:                               *)

let disconnect_action =
  Eliom_registration.Action.create
    ~name:"disconnectionpgd"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () -> Eliom_state.discard ~scope:session ())

let disconnect_box s =
  Html.D.Form.post_form disconnect_action
    (fun _ -> [p [Html.D.Form.input
                    ~input_type:`Submit ~value:s
                    Html.D.Form.string]]) ()

(* The following eref is true if the connection has action failed: *)
let bad_user = Eliom_reference.eref ~scope:Eliom_common.request_scope false

let my_group_data = Eliom_reference.eref ~persistent:"pgd" ~scope:group None

let change_gd =
  Eliom_registration.Action.create
    ~name:"changepgd"
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    (fun () () -> Eliom_reference.set my_group_data (Some (1000 + Random.int 1000)))


(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box session_expired bad_u action =
  Html.D.Form.post_form action
    (fun loginname ->
      let l =
        [pcdata "login: ";
         Html.D.Form.input ~input_type:`Text ~name:loginname
           Html.D.Form.string]
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
  let status = Eliom_state.volatile_data_state_status (*zap* *) ~scope:session (* *zap*) ()
  in
  let group =
    Eliom_state.get_volatile_data_session_group (*zap* *) ~scope:session (* *zap*) ()
  in
  Eliom_reference.get bad_user >>= fun bad_u ->
  Eliom_reference.get my_group_data >>= fun my_group_data ->
  Lwt.return
    (html
       (head (title (pcdata "")) [])
       (body
          (match group, status with
          | Some name, _ ->
              [p [pcdata ("Hello "^name); br ();
                  (match my_group_data with
                    | None -> pcdata "You have no group data."
                    | Some i -> pcdata ("Your group data is "^string_of_int i^"."));
                 ];
               Html.D.Form.post_form change_gd
                 (fun () -> [p [Html.D.Form.input
                                   ~input_type:`Submit
                                   ~value:"Change group data"
                                   Html.D.Form.string]]) ();
               p [pcdata "Check that several sessions have the same group data."];
               p [pcdata "Check that persistent group data do not disappear when all sessions from the group are closed."];
               p [pcdata "Persistent group data are used as a basic database, for example to store user information (email, etc)."];
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
  lwt () = Eliom_state.discard ~scope:session () in
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_state.set_volatile_data_session_group ~set_max:4 (*zap* *) ~scope:session (* *zap*) login;
    Eliom_reference.get my_group_data >>= fun mgd ->
    (if mgd = None
     then Eliom_reference.set my_group_data (Some (Random.int 1000))
     else Lwt.return ()) >>= fun () ->
    Eliom_registration.Redirection.send
      (Eliom_registration.Redirection Eliom_service.reload_action_hidden)
  end
  else
    Eliom_reference.set bad_user true >>= fun () ->
    Eliom_registration.Action.send ()


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_registration.Html.register ~service:connect_example_pgd connect_example_handler;
  Eliom_registration.Any.register ~service:connect_action connect_action_handler


(*****************************************************************************)
(* Actions with `NoReload option *)
let noreload_ref = ref 0

let noreload_action =
  Eliom_registration.Action.create
    ~options:`NoReload
    ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Get unit)
    (fun () () -> noreload_ref := !noreload_ref + 1; Lwt.return ())

let noreload =
  register_service
    ~path:["noreload"]
    ~get_params:unit
    (fun () () ->
      return
        (html
         (head (title (pcdata "counter")) [])
         (body [p [pcdata (string_of_int (!noreload_ref)); br ();
                   Html.D.a ~service:noreload_action
                     [pcdata "Click to increment the counter."] ();
                   br ();
                   pcdata "You should not see the result if you do not reload the page."
                  ]])))




(*****************************************************************************)
(* neopt, by Dario Teixeira *)
let neopt_handler ((a, b), (c, d)) () =
  Lwt.return
    (html
    (head (title (pcdata "Coucou")) [])
    (body  [
      p [pcdata "Coucou:"];
      p [pcdata (Printf.sprintf "a: %s" a)];
      p [pcdata (Printf.sprintf "b: %s" (match b with Some b -> string_of_int b | None -> "(none)"))];
      p [pcdata (Printf.sprintf "c: %s" (match c with Some c -> string_of_float c | None -> "(none)"))];
      p [pcdata (Printf.sprintf "d: %s" (match d with Some d -> d | None -> "(none)"))];
      ]))


let neopt_service =
  Eliom_registration.Html.register_service
    ~path: ["neopt"]
    ~get_params: (suffix_prod
      (Eliom_parameter.string "a" ** neopt (Eliom_parameter.int "b"))
      (neopt (Eliom_parameter.float "c") ** neopt (Eliom_parameter.string "d")))
    neopt_handler


let neopt_form ((e_a, e_b), (e_c, e_d)) =
  [
  fieldset
    [
    label ~a:[a_label_for "e_a"] [pcdata "Enter string 'a':"];
    Html.D.Form.input ~a:[a_id "e_a"] ~input_type:`Text ~name:e_a
      Html.D.Form.string;
    br ();

    label ~a:[a_label_for "e_b"] [pcdata "Enter int 'b' (neopt):"];
    Html.D.Form.input ~a:[a_id "e_b"] ~input_type:`Text ~name:e_b
      Html.D.Form.int;
    br ();

    label ~a:[a_label_for "e_c"] [pcdata "Enter float 'c' (neopt):"];
    Html.D.Form.input ~a:[a_id "e_c"] ~input_type:`Text ~name:e_c
      Html.D.Form.float;
    br ();

    label ~a:[a_label_for "e_d"] [pcdata "Enter string 'd' (neopt):"];
    Html.D.Form.input ~a:[a_id "e_d"] ~input_type:`Text ~name:e_d
      Html.D.Form.string;
    br ();

    Html.D.Form.button_no_value ~button_type:`Submit [pcdata "Apply"];
    ]
  ]


let main_neopt_handler () () =
  Lwt.return
    (html
      (head (title (pcdata "Main")) [])
      (body  [
        p  [
          pcdata "Here's a ";
          Html.D.a neopt_service [pcdata "link"] (("foo", None), (None, None));
          pcdata " to the neopt service"
          ];
        p  [
          pcdata "Here's another ";
          Html.D.a neopt_service [pcdata "link"] (("foo", Some 1), (None, None));
          pcdata " to the neopt service"
          ];
        p  [
          pcdata "Here's yet another ";
          Html.D.a neopt_service [pcdata "link"] (("foo", None), (Some 2.0, Some "Olá!"));
          pcdata " to the neopt service"
          ];
        p  [
          pcdata "Here's the final ";
          Html.D.a neopt_service [pcdata "link"] (("foo", Some 1), (Some 2.0, Some "Olá!"));
          pcdata " to the neopt service"
          ];
        Html.D.Form.get_form neopt_service neopt_form;
        ]))

let main_neopt_service =
  Eliom_registration.Html.register_service
    ~path: ["neopt0"]
    ~get_params: Eliom_parameter.unit
    main_neopt_handler
