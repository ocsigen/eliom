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

let home_service = new_service ~url:[""]
    ~get_params:unit ()

let message_service = new_service ["msg"] (StringMessage.index "num") ()


(*****************************************************************************)
(* Construction of default pages *)

let print_home_page sp () () = return
  (page ~css:["moncss.css"] sp
    [title_box "My forum";
     news_headers_list_box 
       sp messageslist_number anonymoususer rocsexample message_service])

let print_message_page sp i () = return 
  (page ~css:["moncss.css"] sp
    [title_box "My message";
     string_message_box i anonymoususer rocsexample])



(*****************************************************************************)
(* Services registration *)

let _ = register_service
  ~service:home_service
  print_home_page

let _ = register_service
  ~service:message_service
  print_message_page




