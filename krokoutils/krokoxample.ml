(** An example of Kroko module using Kiko *)

open Omlet
open Krokodata
open Krokopages

(** Some boxes and a page we need for the example: *)
(** A page displaying a message *)
let page_message = register_new_url (Url ["msg"]) (_int "num")
  (fun k -> 
     (new page
	[new_title_box "Voici le message"; 
	 new_string_message_box k])#print
  )

(** A box that prints the begining of a message, with a link to the 
    full message *)
let new_beginning_of_string_message_box = 
  constructor_for_new_savable_data_box "beginning_of_string_message_box"
    (fun key ->
       let msg = StringMessage.dbget key
       in let l = link "see" page_message key
       in << <div> $str:msg$ $l$ </div> >>)

(** A box that prints a list of messages *)
let new_beginning_of_string_messages_list_box =
  constructor_for_new_savable_data_box "beginning_of_string_messages_list_box"
    (fun key ->
       let l = 
	 List.map 
	   (fun n -> (new_beginning_of_string_message_box n)#print)
	   (MessagesList.dbget key)
       in << <div> $list:l$ </div> >>)




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


let example_page_number = 
  Krokopersist.make_persistant_lazy "example_page_number"
    (fun () -> 
       dbinsert_page
	 [new_title_box "Kikoo !"; 
	  new_beginning_of_string_messages_list_box 
	    (Krokopersist.get messageslist_number)])

(* -- End population of the database with an example *)



let mapage = dbget_page (Krokopersist.get example_page_number)

let objsav = register_new_url (Url []) _noparam (mapage#print)








(* Other tries:

(* With objects, without savable pages: *)
let mapage2 = 
  new page
    [new_title_box "Kikoo !"; 
     new_string_messages_list_box (Krokopersist.get messageslist_number)]

let nosav = register_new_url (Url ["nosav"]) _noparam (mapage2#print)

(* Without objects and savable pages: *)
open Krokoboites

let noobj = register_new_url (Url ["noobj"]) _noparam
  (let l = 
     [(title_box "Kiko Kroko !");
      (string_messages_list_box (Krokopersist.get messageslist_number))]
   in << <html> $list:l$ </html> >>)





(** Main page *)
let _ = register_new_url (Url []) _noparam
  (let l = link "With objects and savable pages" objsav in
   let ll = link "With objects, no savable pages" nosav in
   let lll = link "Without object and savable pages" noobj in
     << <html> $l$ <br/> $ll$ <br/> $lll$ <br/> </html> >>)



(* dans krokoboites.ml :*)
open Krokodata
open Omlet

(******************************************************************)
(* The boxes that can appear in pages *)

(** Title *)
let title_box titre = << <h1>$str:titre$</h1> >>

(** A box that prints an error message *)
let error_box msg = << <b>$str:msg$</b> >>

(** A simple box that prints a message of the db *)
let string_message_box key =
  let msg = StringMessage.dbget key
  in << $str:msg$ >>

(** A page displaying a message *)
let page_message = register_new_url (Url ["msg"]) (_int "num")
  (fun k -> 
     (let l = 
	[(title_box "Kiko Kroko !");
	 (string_message_box k)]
      in << <html> $list:l$ </html> >>)
  )

(** A box that prints the begining of a message, with a link to the 
    full message *)
let beginning_of_string_message_box key =
  let msg = StringMessage.dbget key
  in let l = link "see" page_message key
  in << <div> $str:msg$ $l$ </div> >>

(** A box that prints a list of messages *)
let string_messages_list_box key =
  let l = List.map beginning_of_string_message_box (MessagesList.dbget key)
  in << <div> $list:l$ </div> >>


*)
