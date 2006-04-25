(* Copyright Vincent Balat 2005 *)
(** An example of Ocsigen module using Ocsimore without savable pages *)

open Ocsigen
open Ocsidata
open Ocsipages
open Ocsisav
open Ocsiboxes
open Rights
open Ocsexample_util


(*****************************************************************************)
(* All the urls: *)

let main_page = new_url ~path:[""]
    ~server_params:http_params ~get_params:no_get_param ()

let news_page = new_url ["msg"] http_params (StringMessage.index "num") ()

let connect_action = 
  new_actionurl
    ~server_params:no_server_param
    ~get_params:(string "login" ** string "password")


(*****************************************************************************)
(* Construction of default pages *)

let accueil h =
  page ~css:["moncss.css"] h
    [title_box "Mon site";
     text_box "(user : toto and password : titi)";
     login_box_action h connect_action;
     news_headers_list_box 
       h messageslist_number anonymoususer rocsexample news_page]

let print_news_page h i = 
  page ~css:["moncss.css"] h
    [title_box "Info";
     login_box_action h connect_action;
     string_message_box i anonymoususer rocsexample]

let user_main_page user h =
  page ~css:["moncss.css"] h
    [title_box "Mon site";
     text_box "Bonjour !";
     connected_box h user;
     news_headers_list_box h messageslist_number user rocsexample news_page]

let user_news_page user h i = 
  page ~css:["moncss.css"] h
    [title_box "Info";
     connected_box h user;
     string_message_box i user rocsexample]


(* Page registering *)

let _ = register_url
  ~url:main_page
  accueil

let _ = register_url
  ~url:news_page
  print_news_page

let launch_session user =
  register_url_for_session ~url:main_page (user_main_page user);
  register_url_for_session ~url:news_page (user_news_page user)

let _ = register_actionurl
  ~actionurl:connect_action
  ~action:(fun login password ->
	     launch_session (connect login password))


