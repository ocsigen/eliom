(* Copyright Vincent Balat 2005 *)
(** An example of Ocsigen module using Ocsimore without savable pages *)

open Ocsigen
open Ocsigen.Xhtml
open Ocsidata
open Ocsipages
open Ocsisav
open Ocsiboxes
open Rights
open Ocsexample_util
open Lwt


(*****************************************************************************)
(* All the urls: *)

let home_service = new_service ~url:[""] ~get_params:unit ()

let message_service = new_service ["msg"] (StringMessage.index "num") ()

let connect_action = 
  new_action
    ~post_params:(string "login" ** string "password")

let deconnect_action = 
  register_new_action unit (fun sp () -> Lwt.return (close_session sp))

(*****************************************************************************)
(* Construction of default pages *)

let print_home_page sp () () = return
  (page ~css:["moncss.css"] sp
    [title_box "My forum";
     text_box "(user : toto and password : titi)";
     login_box_action sp connect_action;
     news_headers_list_box 
       sp messageslist_number anonymoususer rocsexample message_service])

let print_message_page sp i () = return
  (page ~css:["moncss.css"] sp
    [title_box "My message";
     login_box_action sp connect_action;
     string_message_box i anonymoususer rocsexample])

let user_home_page user sp () () = return
  (page ~css:["moncss.css"] sp
    [title_box "My forum";
     text_box "Bonjour !";
     connected_box deconnect_action sp user;
     news_headers_list_box sp messageslist_number user rocsexample message_service])

let user_message_page user sp i () = return
  (page ~css:["moncss.css"] sp
    [title_box "My message";
     connected_box deconnect_action sp user;
     string_message_box i user rocsexample])


(* Services registration *)

let _ = register_service
  ~service:home_service
  print_home_page

let _ = register_service
  ~service:message_service
  print_message_page

let launch_session sp user =
  register_service_for_session sp ~service:home_service 
    (user_home_page user);
  register_service_for_session sp ~service:message_service 
    (user_message_page user)

let _ = register_action
  ~action:connect_action
    (fun sp (login, password) -> return
        (launch_session sp (connect login password)))


