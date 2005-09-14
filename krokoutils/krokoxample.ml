(** An example of Kroko module using Kiko *)

open Omlet
open Krokodata
open Krokopages


(** Some boxes and a page we need for the example: *)
(** A page displaying a message *)
let page_message = new_url (Url ["msg"]) (_int "num")

(** A box that prints the beginning of a message, with a link to the 
    full message *)
let string_message_header_box key = 
  let msg = StringMessage.dbget key
  in let l = link "see" page_message key
  in << <div> $str:msg$ $l$ </div> >>

(* éventuellement :
let fold_string_message_header_box = 
  RegisterBox.register ~name:"string_message_header_box" 
    ~constructor:string_message_header_box

let fold_string_message_header_intbox = 
  RegisterIntBox.register ~name:"string_message_header_box" 
    ~constructor:(fun () -> string_message_header_box)
*)

(** A box that prints a list of a message headers *)
let string_messages_headers_list_box key = 
  let msglist = 
    List.map 
      (fun n -> string_message_header_box n)
      (MessagesList.dbget key)
  in << <div>$list:msglist$</div> >>

let fold_string_messages_headers_list_box = 
  RegisterBox.register ~name:"string_messages_headers_list_box" 
    ~constructor:string_messages_headers_list_box

(* éventuellement :
let fold_string_messages_headers_list_box = 
  RegisterIntBox.register ~name:"string_messages_headers_list_box" 
    ~constructor:(fun () -> string_messages_headers_list_box)
*)



(* -- Here I populate the database with some examples: *)

let messageslist_number =
  Krokopersist.make_persistant_lazy "messageslist_number"
    (fun () -> 
       MessagesList.dbinsert
	 [StringMessage.dbinsert "Ceci est un premier message";
	  StringMessage.dbinsert "Ceci est un deuxième message";
	  StringMessage.dbinsert "Ceci est un troisième message";
	  StringMessage.dbinsert "Ceci est un quatrième message";
	  StringMessage.dbinsert "Ceci est un cinquième message";
	  StringMessage.dbinsert "Ceci est un sixième message";
	  StringMessage.dbinsert "Ceci est un septième message"])


let example_main_page_number = 
  Krokopersist.make_persistant_lazy "example_page_number"
    (fun () -> 
       dbinsertdyn
	 (fold_page_fromdb
	    [fold_title_box "Kikoo !"; 
	     fold_string_messages_headers_list_box 
	       (Krokopersist.get messageslist_number)]))

let example_msg_page_number = 
  Krokopersist.make_persistant_lazy "example_msg_page_number"
    (fun () -> 
       dbinsertdyn
	 (fold_int_page
	    [`Box (fold_title_box "Voici le message"); 
	     `ParamBox (fold_string_message_intbox ())])
    )

(* -- End population of the database with an example *)


let _ = register_url page_message
  (fun n ->
     (RegisterIntPage.dbget (Krokopersist.get example_msg_page_number)) n)

let objsav = register_new_url (Url []) _noparam 
  (RegisterPage.dbget (Krokopersist.get example_main_page_number))

