(* Copyright Vincent Balat 2005 *)
(** An example of Kroko module using Krokoutils *)


open Omlet
open Krokodata
open Krokopages
open Krokosavable
open Krokoboxes


(*****************************************************************************)
(* All the urls: *)

let main_page = new_url ~name:(Url [""]) ~params:(_http_params _noparam)

let news_page = new_url (Url ["msg"]) (_http_params (_int "num"))

let connect_action = 
  new_actionurl ~params:(_string "login" ** _string "password")


(******************************************************************)
(* -- Here I populate the database with some examples: *)

let forumadmin = Rights.connect "root" ""

let messageslist_number =
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_messageslist_number"
       (fun () -> 
	  MessagesList.dbinsert forumadmin
	    [StringMessage.dbinsert forumadmin "Ceci est un premier message";
	     StringMessage.dbinsert forumadmin "Ceci est un deuxième message";
	     StringMessage.dbinsert forumadmin "Ceci est un troisième message";
	     StringMessage.dbinsert forumadmin "Ceci est un quatrième message";
	     StringMessage.dbinsert forumadmin "Ceci est un cinquième message";
	     StringMessage.dbinsert forumadmin "Ceci est un sixième message";
	     StringMessage.dbinsert forumadmin "Ceci est un septième message"])
    )


(* An user *)
let toto_created =
  Krokopersist.make_persistant_lazy "toto_created"
  (fun () -> 
     ignore (Rights.create_user forumadmin "toto" "Toto" "titi"); 
     true)

(* -- End population of the database with an example *)


(******************************************************************)
(* My boxes *)

(** A box that prints the beginning of a message, with a link to the 
    full message *)
let news_header_box httpparam key user = 
  let msg = StringMessage.dbget user key
  in let l = link "see" httpparam.current_url news_page key
  in << <div> $str:msg$ $l$ </div> >>

(** A box that prints a list of a message headers *)
let news_headers_list_box httpparam key user = 
  let msglist = 
    List.map 
      (fun n -> news_header_box httpparam n user)
      (MessagesList.dbget user key)
  in << <div>$list:msglist$</div> >>





(*****************************************************************************)
(* Register for public pages: *)
module RegisterPublicOrNotBoxes =
  MakeRegister
    (struct 
       type t = Omlet.http_params -> Rights.user -> Xhtmlpp.xhtmlcont
       let name = "krokoxample_publicornotpageboxes"
       let default_handler ex h u = box_exn_handler ex
       let default_tables =
	 [RegisterBoxes.get_table (fun d h u -> d)]
     end)

(* Register for public pages: *)
module RegisterPublicPage =
  MakeRegister
    (struct 
       type t = Omlet.http_params -> Xhtmlpp.xhtmlcont
       let name = "krokoxample_publicpageboxes"
       let default_handler ex h = box_exn_handler ex
       let default_tables =
	 [RegisterPublicOrNotBoxes.get_table 
	    (fun d h -> d h Rights.anonymoususer)]
     end)

(* Register for piece of news pages: *)
module RegisterNewsPage =
  MakeRegister
    (struct 
       type t = Omlet.http_params -> Rights.user -> int -> Xhtmlpp.xhtmlcont
       let name = "krokoxample_newspageboxes"
       let default_handler ex h u i = box_exn_handler ex
       let default_tables = []
     end)

(* Register for the public piece of news page: *)
module RegisterPublicNewsPage =
  MakeRegister
    (struct 
       type t = Omlet.http_params -> int -> Xhtmlpp.xhtmlcont
       let name = "krokoxample_publicnewspageboxes"
       let default_handler ex h i = box_exn_handler ex
       let default_tables =
	 [RegisterNewsPage.get_table (fun d h i -> d h Rights.anonymoususer i);
	  RegisterPublicPage.get_table (fun d h i -> d h)]
     end)

module RegisterUserPage =
  MakeRegister
    (struct 
       type t = Omlet.http_params -> Rights.user -> Xhtmlpp.xhtmlcont
       let name = "krokoxample_userpages"
       let default_handler ex h u = box_exn_handler ex
       let default_tables =
	 [RegisterPublicOrNotBoxes.get_table (fun d h u -> d h u)]
     end)

module RegisterUserNewsPage =
  MakeRegister
    (struct 
       type t = Omlet.http_params -> Rights.user -> int -> Xhtmlpp.xhtmlcont
       let name = "krokoxample_usernewspage"
       let default_handler ex h u i = box_exn_handler ex
       let default_tables =
	 [RegisterUserPage.get_table (fun d h u i -> d h u);
	  RegisterNewsPage.get_table (fun d h u i -> d h u i)]
     end)



let fold_news_headers_list_box =
  RegisterPublicOrNotBoxes.register ~name:"krokoxample_news_headers_list_box" 
    ~constructor:(fun ~box_param:i h u -> news_headers_list_box h i u)

let fold_user_pages_deconnect_box =
  RegisterUserPage.register ~name:"krokoxample_user_pages_deconnect_box" 
    ~constructor:(fun ~box_param:() h u -> deconnect_box h "close session")

let fold_news_box =
  RegisterNewsPage.register ~name:"krokoxample_news_box" 
    ~constructor:(fun ~box_param:() h u i -> string_message_box i u)

let fold_login_box_action =
  RegisterPublicPage.register ~name:"krokoxample_login_box_action" 
    ~constructor:(fun ~box_param:() h -> login_box_action h connect_action)


(*****************************************************************************)
(* Construction of default pages *)

let public_main_page_number =
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_public_main_page_number"
       (fun () -> 
	  RegisterPublicPage.dbinsertlist forumadmin
	    [RegisterPublicPage.make_box (fold_title_box "Mon site");
	     RegisterPublicPage.make_box 
	       (fold_text_box "(user : toto and password : titi)");
	     RegisterPublicPage.make_box (fold_login_box_action ());
	     RegisterPublicPage.make_box 
	       (fold_news_headers_list_box messageslist_number)]
       ))

let public_news_page_number = 
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_public_news_page_number"
       (fun () ->
	  RegisterPublicNewsPage.dbinsertlist forumadmin
	    [RegisterPublicNewsPage.make_box (fold_title_box "Info");
	     RegisterPublicNewsPage.make_box (fold_login_box_action ());
	     RegisterPublicNewsPage.make_box (fold_news_box ())]
       ))

let user_main_page_number =
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_user_main_page_number"
       (fun () -> 
	  RegisterUserPage.dbinsertlist forumadmin
	    [RegisterUserPage.make_box (fold_title_box "Mon site");
	     RegisterUserPage.make_box 
	       (fold_text_box "Bonjour !");
	     RegisterUserPage.make_box (fold_user_pages_deconnect_box ());
	     RegisterUserPage.make_box 
	       (fold_news_headers_list_box messageslist_number)]
       ))

let user_news_page_number = 
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_user_news_page_number"
       (fun () ->
	  RegisterUserNewsPage.dbinsertlist forumadmin
	    [RegisterUserNewsPage.make_box (fold_title_box "Info");
	     RegisterUserNewsPage.make_box (fold_user_pages_deconnect_box ());
	     RegisterUserNewsPage.make_box (fold_news_box ())]
       ))


(*****************************************************************************)
let accueil h = 
  page h
    (RegisterPublicPage.dbgetlist
       ~user:Rights.anonymoususer
       ~filter:(fun d -> d h)
       ~key:public_main_page_number)

let print_news_page h i = 
  page h
    (RegisterPublicNewsPage.dbgetlist
       ~user:Rights.anonymoususer
       ~filter:(fun d -> d h i)
       ~key:public_news_page_number)

let _ = register_url
  ~url:main_page
  ~action:accueil

let _ = register_url
  ~url:news_page
  ~action:print_news_page

let user_main_page user h =
  page h
    (RegisterUserPage.dbgetlist
       ~user:user
       ~filter:(fun d -> d h user)
       ~key:user_main_page_number)

let user_news_page user h i =
  page h
    (RegisterUserNewsPage.dbgetlist
       ~user:user
       ~filter:(fun d -> d h user i)
       ~key:user_news_page_number)

let launch_session user =
  register_session_url ~url:main_page ~action:(user_main_page user);
  register_session_url ~url:news_page ~action:(user_news_page user)

let _ = register_actionurl
  ~actionurl:connect_action
  ~action:(fun login password ->
	     launch_session (Rights.connect login password))






(******* AEFFFFFF old
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
	    (fold_string_messages_headers_list_box stringmessageslist_number)]
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
     userintboxes_page example_msg_page_number Rights.anonymoususer n)

let _ = register_new_url (Url ["savable"]) (_http_params _noparam)
  (fun h -> 
     hpuserboxes_page example_main_page_number h Rights.anonymoususer)












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

*)
