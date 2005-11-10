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


(*****************************************************************************)
(* Construction of default pages *)

let accueil h =
  page h
    [title_box "Mon site";
     news_headers_list_box 
       h messageslist_number anonymoususer rkrokexample news_page]

let print_news_page h i = 
  page h
    [title_box "Info";
     string_message_box i anonymoususer rkrokexample]



(*****************************************************************************)
(* Page registering *)

let _ = register_url
  ~url:main_page
  ~page:accueil

let _ = register_url
  ~url:news_page
  ~page:print_news_page




