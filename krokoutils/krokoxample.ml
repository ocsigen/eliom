(** An example of Kroko module using Kiko *)

open Omlet
open Krokodata
open Krokopages




(** Some boxes and a page we need for the example: *)
(** A page displaying a message *)
let page_message = register_new_url (Url ["msg"]) (_int "num")
  (fun k -> 
     (new page
	[new title_box "Voici le message" () (); 
	 new string_message_box () () k] () k)#print
  )

(** A box that prints the beginning of a message, with a link to the 
    full message *)
class string_message_header_box () sessionparam key = object
  inherit box () sessionparam key
  val msg = StringMessage.dbget key
  val l = link "see" page_message key
  method print =  << <div> $str:msg$ $l$ </div> >>
end

let fold_string_message_header_box = 
  RegisterIntBox.register ~name:"string_message_header_box" 
    ~constructor:(new string_message_header_box)

(** A box that prints a list of a message headers *)
class string_messages_headers_list_box key sessionparam pageparam = object
  inherit box key sessionparam pageparam
  val msglist = 
    List.map 
      (fun n -> (new string_message_header_box () sessionparam n)#print)
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


let example_page_number = 
  Krokopersist.make_persistant_lazy "example_page_number"
    (fun () -> 
       dbinsertdyn
	 (fold_page_fromdb
	    [fold_title_box "Kikoo !"; 
	     fold_string_messages_headers_list_box 
	       (Krokopersist.get messageslist_number)]))

(* -- End population of the database with an example *)



let mapage = (RegisterPage.dbget (Krokopersist.get example_page_number)) () ()

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
