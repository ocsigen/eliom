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

let main_page = new_url ~path:[""] ~params:(_http_params _noparam) ()

let news_page = new_url ["msg"] (_http_params (StringMessage._index "num")) ()


(*****************************************************************************)
(* Construction of default pages *)

let accueil h =
  page h
    [title_box "Mon site";
     news_headers_list_box 
       h messageslist_number anonymoususer rocsexample news_page]

let print_news_page h i = 
  page h
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




