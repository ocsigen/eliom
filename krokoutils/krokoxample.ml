(** An example of Kroko module using Kiko *)

open Omlet
open Krokodata
open Krokopages


(** Some boxes and a page we need for the example: *)
(** A page displaying a message *)
let page_message = new_url (Url ["msg"]) (_int "num")

(** A box that prints the beginning of a message, with a link to the 
    full message *)
class string_message_header_box key = object
  inherit box
  val msg = StringMessage.dbget key
  val l = link "see" page_message key
  method print =  << <div> $str:msg$ $l$ </div> >>
end

let fold_string_message_header_box = 
  RegisterIntBox.register ~name:"string_message_header_box" 
    ~constructor:(fun () -> new string_message_header_box)

(** A box that prints a list of a message headers *)
class string_messages_headers_list_box key = object
  inherit box
  val msglist = 
    List.map 
      (fun n -> (new string_message_header_box n)#print)
      (MessagesList.dbget key)
  method print = << <div>$list:msglist$</div> >>
end

let fold_string_messages_headers_list_box = 
  RegisterBox.register ~name:"string_messages_headers_list_box" 
    ~constructor:(new string_messages_headers_list_box)



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
	 (fold_int_page_fromdb
	    [fold_forget_param_int_box (fold_title_box "Voici le message"); 
	     fold_string_message_box ()])
    )

(* -- End population of the database with an example *)


let _ = register_url page_message
  (fun n -> 
     ((RegisterIntPage.dbget 
	(Krokopersist.get example_msg_page_number)) n)#print)

let objsav = register_new_url (Url []) _noparam 
  (RegisterPage.dbget (Krokopersist.get example_main_page_number))#print

