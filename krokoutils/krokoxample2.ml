(* Copyright Vincent Balat 2005 *)
(** An example of Kroko module using Krokoutils without savable pages *)

open Kroko
open Krokodata
open Krokopages
open Krokosavable
open Krokoboxes
open Rights
open Krokoxample_util


(*****************************************************************************)
(* All the urls: *)

let main_page = new_url ~name:(Url [""]) ~params:(_http_params _noparam)

let news_page = new_url (Url ["msg"]) (_http_params (StringMessage._index "num"))

let connect_action = 
  new_actionurl ~params:(_string "login" ** _string "password")


(*****************************************************************************)
(* Construction of default pages *)

let accueil h =
  page h
    [title_box "Mon site";
     text_box "(user : toto and password : titi)";
     login_box_action h connect_action;
     news_headers_list_box 
       h messageslist_number anonymoususer rkrokexample news_page]

let print_news_page h i = 
  page h
    [title_box "Info";
     login_box_action h connect_action;
     string_message_box i anonymoususer rkrokexample]

let user_main_page user h =
  page h
    [title_box "Mon site";
     text_box "Bonjour !";
     connected_box h user;
     news_headers_list_box h messageslist_number user rkrokexample news_page]

let user_news_page user h i = 
  page h
    [title_box "Info";
     connected_box h user;
     string_message_box i user rkrokexample]


(* Page registering *)

let _ = register_url
  ~url:main_page
  ~action:accueil

let _ = register_url
  ~url:news_page
  ~action:print_news_page

let launch_session user =
  register_session_url ~url:main_page ~action:(user_main_page user);
  register_session_url ~url:news_page ~action:(user_news_page user)

let _ = register_actionurl
  ~actionurl:connect_action
  ~action:(fun login password ->
	     launch_session (connect login password))


