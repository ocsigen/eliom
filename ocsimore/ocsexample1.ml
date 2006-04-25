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
    ~server_params:http_params
    ~get_params:no_get_param ()

let news_page = new_url ["msg"] http_params (StringMessage.index "num") ()


(*****************************************************************************)
(* Construction of default pages *)

let accueil h =
  page ~css:["moncss.css"] h
    [title_box "Mon site";
     news_headers_list_box 
       h messageslist_number anonymoususer rocsexample news_page]

let print_news_page h i = 
  page ~css:["moncss.css"] h
    [title_box "Info";
     string_message_box i anonymoususer rocsexample]



(*****************************************************************************)
(* Page registering *)

let _ = register_url
  ~url:main_page
  accueil

let _ = register_url
  ~url:news_page
  print_news_page




