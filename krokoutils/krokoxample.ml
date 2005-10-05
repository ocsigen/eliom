(* Copyright Vincent Balat 2005 *)
(** An example of Kroko module using Krokoutils *)

open Omlet
open Krokodata
open Krokopages
open Krokosavable
open Krokoboxes


(** Some boxes and a page we need for the example: *)
(** A page displaying a message *)
let page_message = new_url (Url ["savable";"msg"]) (_int "num")


(** A box that prints the beginning of a message, with a link to the 
    full message *)
let string_message_header_box httpparam key user = 
  let msg = StringMessage.dbget user key
  in let l = link "see" httpparam.current_url page_message key
  in << <div> $str:msg$ $l$ </div> >>

(** A box that prints a list of a message headers *)
let string_messages_headers_list_box httpparam key user = 
  let msglist = 
    List.map 
      (fun n -> string_message_header_box httpparam n user)
      (MessagesList.dbget user key)
  in << <div>$list:msglist$</div> >>

(* To be able to use this box in a savable page,
   we register its constructor in the register of pages available for users:
*)
let fold_string_messages_headers_list_box = 
  RegisterHPUserBoxes.register ~name:"string_messages_headers_list_box" 
    ~constructor:(fun ~box_param:i h u -> string_messages_headers_list_box h i u)



(******************************************************************)
(* -- Here I populate the database with some examples: *)

let forumadmin = Rights.connect "root" ""

let stringmessageslist_number =
  Krokopersist.make_persistant_lazy "stringmessageslist_number"
    (fun () -> 
       MessagesList.dbinsert forumadmin
	 [StringMessage.dbinsert forumadmin "Ceci est un premier message";
	  StringMessage.dbinsert forumadmin "Ceci est un deuxième message";
	  StringMessage.dbinsert forumadmin "Ceci est un troisième message";
	  StringMessage.dbinsert forumadmin "Ceci est un quatrième message";
	  StringMessage.dbinsert forumadmin "Ceci est un cinquième message";
	  StringMessage.dbinsert forumadmin "Ceci est un sixième message";
	  StringMessage.dbinsert forumadmin "Ceci est un septième message"])


(* This is just an example of savable pages. *)
let example_main_page_number =
  Krokopersist.make_persistant_lazy "example_page_number"
    (fun () -> 
       RegisterHPUserBoxes.dbinsertlist forumadmin
	 [RegisterHPUserBoxes.make_box (fold_title_box "Titre");
	  RegisterHPUserBoxes.make_box 
	    (fold_string_messages_headers_list_box 
	       (Krokopersist.get stringmessageslist_number))]
    )

let example_msg_page_number = 
  Krokopersist.make_persistant_lazy "example_msg_page_number"
    (fun () -> 
       RegisterUserIntBoxes.dbinsertlist forumadmin
	 [RegisterUserIntBoxes.make_box (fold_title_box "Oup");
	  RegisterUserIntBoxes.make_box (fold_string_message_box ())]
    )


(* An user *)
let toto_created =
  Krokopersist.make_persistant_lazy "toto_created"
  (fun () -> 
     ignore (Rights.create_user forumadmin "toto" "Toto" "titi"); 
     true)

(* -- End population of the database with an example *)
(******************************************************************)

(* example with savable pages *)

let _ = register_url page_message
  (fun n -> 
     userintboxes_page 
       (Krokopersist.get example_msg_page_number) Rights.anonymoususer n)

let _ = register_new_url (Url ["savable"]) (_http_params _noparam)
  (fun h -> 
     hpuserboxes_page
       (Krokopersist.get example_main_page_number) h Rights.anonymoususer)












(* For the following I don't use savable pages *)
let public_session_without_post_params = 
  new_url 
    ~name:(Url ["session"]) 
    ~params:(_http_params _noparam)

let public_session_with_post_params = 
  new_post_url 
    ~fallback:public_session_without_post_params
    ~post_params:(_string "login" ** _string "password")

let accueil h =
  let f = login_box h public_session_with_post_params in
  let ml = string_messages_headers_list_box h
    (Krokopersist.get stringmessageslist_number) Rights.anonymoususer in
  << <html> (user : toto and password : titi) $f$ $ml$ </html> >>

let _ = register_url
  ~url:public_session_without_post_params
  ~action:accueil

let rec launch_session h user =
  let close = register_new_session_url
    ~fallback:public_session_without_post_params 
    ~action:(fun h -> close_session (); accueil h)
  in
  let new_main_page h =
    let ml = string_messages_headers_list_box h
      (Krokopersist.get stringmessageslist_number) user
    and l = link "close session" h.current_url close in
    << <html>
         Bienvenue ! <br/>
         $ml$
         $l$
      </html> >>
  in
    register_session_url 
      ~url:public_session_without_post_params 
      ~action:new_main_page;
    new_main_page h
      
let verif_password login password h =
  try
    launch_session h (Rights.connect login password)
  with _ -> accueil h

let _ =
  register_post_url
    ~url:public_session_with_post_params
    ~action:verif_password

