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


(*****************************************************************************)
(* All the urls: *)

let main_page = new_service ~url:[""]
    ~get_params:unit ()

let news_page = new_service ["msg"] (StringMessage.index "num") ()


(*****************************************************************************)
(* Construction of default pages *)

let accueil h () () =
  page ~css:["moncss.css"] h
    [title_box "Mon site";
     news_headers_list_box 
       h messageslist_number anonymoususer rocsexample news_page]

let print_news_page h i () = 
  page ~css:["moncss.css"] h
    [title_box "Info";
     string_message_box i anonymoususer rocsexample]



(*****************************************************************************)
(* Services registration *)

let _ = register_service
  ~service:main_page
  accueil

let _ = register_service
  ~service:news_page
  print_news_page




