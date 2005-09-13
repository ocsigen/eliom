(** An example of Kroko module using Kiko *)

open Omlet
open Krokodata
open Krokopages
open Krokoboites


(* -- Here I populate the database with some examples: *)

let messageslist_number =
  Krokopersist.make_persistant_lazy "messageslist_number"
    (fun () -> 
       MessagesList.dbinsert
	 [StringMessage.dbinsert "Ceci est un message";
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
	  new_string_messages_list_box (Krokopersist.get messageslist_number)])

(* -- End population of the database with an example *)



(* With objects and savable pages: *)
let mapage = dbget_page (Krokopersist.get example_page_number)

let objsav = register_new_url (Url ["objsav"]) _noparam (mapage#print)


(* With objects, without savable pages: *)
let mapage2 = 
  new page
    [new_title_box "Kikoo !"; 
     new_string_messages_list_box (Krokopersist.get messageslist_number)]

let nosav = register_new_url (Url ["nosav"]) _noparam (mapage2#print)

(* Without objects and savable pages: *)
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

