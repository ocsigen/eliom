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

