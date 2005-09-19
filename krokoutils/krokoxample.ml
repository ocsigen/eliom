(* Copyright Vincent Balat 2005 *)
(** An example of Kroko module using Kiko *)

open Omlet
open Krokodata
open Krokopages


(** Some boxes and a page we need for the example: *)
(** A page displaying a message *)
let page_message = new_url (Url ["msg"]) (_int "num")

(** A box that prints the beginning of a message, with a link to the 
    full message *)
let string_message_header_box key user = 
  let msg = StringMessage.dbget user key
  in let l = link "see" page_message key
  in << <div> $str:msg$ $l$ </div> >>

(** A box that prints a list of a message headers *)
let string_messages_headers_list_box key user = 
  let msglist = 
    List.map 
      (fun n -> string_message_header_box n user)
      (MessagesList.dbget user key)
  in << <div>$list:msglist$</div> >>

let fold_string_messages_headers_list_userbox = 
  RegisterUserBox.register ~name:"string_messages_headers_list_userbox" 
    ~constructor:string_messages_headers_list_box


(* -- Here I populate the database with some examples: *)

let forumadmin = Rights.connect "forumadmin" "pwd"

let messageslist_number =
  Krokopersist.make_persistant_lazy "messageslist_number"
    (fun () -> 
       MessagesList.dbinsert forumadmin
	 [StringMessage.dbinsert forumadmin "Ceci est un premier message";
	  StringMessage.dbinsert forumadmin "Ceci est un deuxième message";
	  StringMessage.dbinsert forumadmin "Ceci est un troisième message";
	  StringMessage.dbinsert forumadmin "Ceci est un quatrième message";
	  StringMessage.dbinsert forumadmin "Ceci est un cinquième message";
	  StringMessage.dbinsert forumadmin "Ceci est un sixième message";
	  StringMessage.dbinsert forumadmin "Ceci est un septième message"])


let example_main_page_number = 
  Krokopersist.make_persistant_lazy "example_page_number"
    (fun () -> 
       dbinsertdyn forumadmin
	 (fold_userpage
	    [`Box (fold_title_box "Kikoo !"); 
	     `ParamBox (fold_string_messages_headers_list_userbox
			  (Krokopersist.get messageslist_number))]))

let example_msg_page_number = 
  Krokopersist.make_persistant_lazy "example_msg_page_number"
    (fun () -> 
       dbinsertdyn forumadmin
	 (fold_userintpage
	    [`Box (fold_title_box "Voici le message"); 
	     `SessionParamBox (fold_string_message_userintbox ())])
    )

(* -- End population of the database with an example *)


let _ = register_url page_message
  (fun n -> 
     (RegisterUserIntPage.dbget Rights.anonymoususer
	(Krokopersist.get example_msg_page_number)) Rights.anonymoususer n)

let objsav = register_new_url (Url []) _noparam 
  ((RegisterUserPage.dbget
      Rights.anonymoususer 
      (Krokopersist.get example_main_page_number)) Rights.anonymoususer)

