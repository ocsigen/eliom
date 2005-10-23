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

let news_page = new_url (Url ["msg"]) (_http_params (StringMessage._index "num"))

let connect_action = 
  new_actionurl ~params:(_string "login" ** _string "password")

(** A list of messages numbers *)
module StringMessageIndexList = 
  MakeSaver (struct 
	       type t = StringMessage.t index list
	       let name = "string_message_index_list"
	     end)


(******************************************************************)
(* -- Here I populate the database with some examples: *)

let forumadmin = Rights.connect "root" ""

let messageslist_number =
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_messageslist_number"
       (fun () -> 
	  StringMessageIndexList.dbinsert forumadmin
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
      (StringMessageIndexList.dbget user key)
  in << <div>$list:msglist$</div> >>





(*****************************************************************************)
(* Register for public pages: *)
module RegisterPublicOrNotBoxes =
  MakeRegister
    (struct 
      type content = Xhtmlpp.xhtmlcont
      type 'a t = Omlet.http_params -> Rights.user -> 'a
      type box = [`PublicOrNotBox of content t tfolded]
      type boxes = [ box | RegisterBoxes.boxes ]
      let name = "KrokoxampleRegisterPublicOrNotBoxes"
      let tag x = `PublicOrNotBox x
      let untag (`PublicOrNotBox x) = x
      let default_handler ex h u = box_exn_handler ex
      let make_boxofboxes ~filter l h u = 
	List.map (fun b -> (filter b) h u) l
      type container_param = string option * string option
      let container f ~box_param:((classe,id),l) h u = 
	boxes_container ?classe:classe ?id:id (f ~user:u l h u)
     end)

let fold_publicornotboxes = 
  RegisterPublicOrNotBoxes.register_unfolds
    ~box_constructor:(fun b h u -> match b with
      #RegisterPublicOrNotBoxes.box as bb -> 
	RegisterPublicOrNotBoxes.unfold bb h u
    | #RegisterBoxes.boxes as bb -> RegisterBoxes.unfolds bb
		     )

(* Register for public pages: *)
module RegisterPublicBoxes =
  MakeRegister
    (struct 
      type content = Xhtmlpp.xhtmlcont
      type 'a t = Omlet.http_params -> 'a
      type box = [`PublicBox of content t tfolded]
      type boxes = [ box | RegisterPublicOrNotBoxes.boxes ]
      let name = "KrokoxampleRegisterPublicBoxes"
      let tag x = `PublicBox x
      let untag (`PublicBox x) = x
      let default_handler ex h = box_exn_handler ex
      let make_boxofboxes ~filter l h = 
	List.map (fun b -> (filter b) h) l
      type container_param = string option * string option
      let container f ~box_param:((classe,id),l) h = 
	boxes_container ?classe:classe ?id:id 
	  (f ~user:Rights.anonymoususer l h)
     end)

let fold_publicboxes = 
  RegisterPublicBoxes.register_unfolds
    ~box_constructor:(fun b h -> match b with
      #RegisterPublicBoxes.box as bb -> 
	RegisterPublicBoxes.unfold bb h
    | #RegisterPublicOrNotBoxes.boxes as bb -> 
	RegisterPublicOrNotBoxes.unfolds bb h Rights.anonymoususer
		     )

(* Register for piece of news pages: *)
module RegisterNewsBoxes =
  MakeRegister
    (struct 
      type content = Xhtmlpp.xhtmlcont
      type 'a t = Omlet.http_params -> Rights.user -> 
	StringMessage.t index -> 'a
      type box = [`NewsBox of content t tfolded]
      type boxes = box
      let name = "KrokoxampleRegisterNewsBoxes"
      let tag x = `NewsBox x
      let untag (`NewsBox x) = x
      let default_handler ex h u i = box_exn_handler ex
      let make_boxofboxes ~filter l h u i = 
	List.map (fun b -> (filter b) h u i) l
      type container_param = string option * string option
      let container f ~box_param:((classe,id),l) h u i = 
	boxes_container ?classe:classe ?id:id (f ~user:u l h u i)
    end)

let fold_newsboxes = 
  RegisterNewsBoxes.register_unfolds
    ~box_constructor:RegisterNewsBoxes.unfold

(* Register for the public piece of news page: *)
module RegisterPublicNewsBoxes =
  MakeRegister
    (struct 
      type content = Xhtmlpp.xhtmlcont
      type 'a t = Omlet.http_params -> StringMessage.t index -> 'a
      type box = [`PublicNewsBox of content t tfolded]
      type boxes = [ box
	| RegisterNewsBoxes.boxes
	| RegisterPublicBoxes.boxes ]
      let name = "KrokoxampleRegisterPublicNewsBoxes"
      let tag x = `PublicNewsBox x
      let untag (`PublicNewsBox x) = x
      let default_handler ex h i = box_exn_handler ex
      let make_boxofboxes ~filter l h i = 
	List.map (fun b -> (filter b) h i) l
      type container_param = string option * string option
      let container f ~box_param:((classe,id),l) h i = 
	boxes_container ?classe:classe ?id:id (f ~user:Rights.anonymoususer l h i)
    end)

let fold_publicnewsboxes = 
  RegisterPublicNewsBoxes.register_unfolds
    ~box_constructor:(fun b h i -> match b with
      #RegisterPublicNewsBoxes.box as bb -> 
	RegisterPublicNewsBoxes.unfold bb h i
    | #RegisterNewsBoxes.boxes as bb -> 
	RegisterNewsBoxes.unfolds bb h Rights.anonymoususer i
    | #RegisterPublicBoxes.boxes as bb -> 
	RegisterPublicBoxes.unfolds bb h
		     )

module RegisterUserBoxes =
  MakeRegister
    (struct 
      type content = Xhtmlpp.xhtmlcont
      type 'a t = Omlet.http_params -> Rights.user -> 'a
      type box = [`UserBox of content t tfolded]
      type boxes = [ box | RegisterPublicOrNotBoxes.boxes ]
      let name = "KrokoxampleRegisterUserBoxes"
      let tag x = `UserBox x
      let untag (`UserBox x) = x
      let default_handler ex h u = box_exn_handler ex
      let make_boxofboxes ~filter l h u = 
	List.map (fun b -> (filter b) h u) l
      type container_param = string option * string option
      let container f ~box_param:((classe,id),l) h u = 
	boxes_container ?classe:classe ?id:id (f ~user:u l h u)
    end)

let fold_userboxes = 
  RegisterUserBoxes.register_unfolds
    ~box_constructor:(fun b h u -> match b with
      #RegisterUserBoxes.box as bb -> 
	RegisterUserBoxes.unfold bb h u
    | #RegisterPublicOrNotBoxes.boxes as bb -> 
	RegisterPublicOrNotBoxes.unfolds bb h u
		     )

module RegisterUserNewsBoxes =
  MakeRegister
    (struct 
      type content = Xhtmlpp.xhtmlcont
      type 'a t = Omlet.http_params -> Rights.user -> 
	StringMessage.t index -> 'a
      type box = [`UserNewsBox of content t tfolded]
      type boxes = [ box | RegisterUserBoxes.boxes | RegisterNewsBoxes.boxes ]
      let name = "KrokoxampleRegisterUserNewsBoxes"
      let tag x = `UserNewsBox x
      let untag (`UserNewsBox x) = x
      let default_handler ex h u i = box_exn_handler ex
      let make_boxofboxes ~filter l h u i = 
	List.map (fun b -> (filter b) h u i) l
      type container_param = string option * string option
      let container f ~box_param:((classe,id),l) h u i = 
	boxes_container ?classe:classe ?id:id (f ~user:u l h u i)
    end)

let fold_usernewsboxes = 
  RegisterUserNewsBoxes.register_unfolds
    ~box_constructor:(fun b h u i -> match b with
      #RegisterUserNewsBoxes.box as bb -> 
	RegisterUserNewsBoxes.unfold bb h u i
    | #RegisterUserBoxes.boxes as bb -> RegisterUserBoxes.unfolds bb h u
    | #RegisterNewsBoxes.boxes as bb -> RegisterNewsBoxes.unfolds bb h u i
		     )


let fold_news_headers_list_box =
  RegisterPublicOrNotBoxes.register ~name:"krokoxample_news_headers_list_box" 
    ~constructor:(fun ~box_param:i h u -> news_headers_list_box h i u)

let fold_user_pages_deconnect_box =
  RegisterUserBoxes.register ~name:"krokoxample_user_pages_deconnect_box" 
    ~constructor:(fun ~box_param:() h u -> deconnect_box h "close session")

let fold_news_box =
  RegisterNewsBoxes.register ~name:"krokoxample_news_box" 
    ~constructor:(fun ~box_param:() h u i -> string_message_box i u)

let fold_login_box_action =
  RegisterPublicBoxes.register ~name:"krokoxample_login_box_action" 
    ~constructor:(fun ~box_param:() h -> login_box_action h connect_action)


(*****************************************************************************)
(* Construction of default pages *)

let public_main_page_number =
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_public_main_page_number"
       (fun () -> 
	  RegisterPublicBoxes.dbinsertlist forumadmin
	   (fold_publicboxes
	      [((fold_title_box "Mon site") :> RegisterPublicBoxes.boxes);
	       ((fold_text_box "(user : toto and password : titi)")
		  :> RegisterPublicBoxes.boxes);
	       ((fold_login_box_action ())
		  :> RegisterPublicBoxes.boxes);
	       ((fold_news_headers_list_box messageslist_number)
		  :> RegisterPublicBoxes.boxes)])
       ))

let public_news_page_number = 
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_public_news_page_number"
       (fun () ->
	  RegisterPublicNewsBoxes.dbinsertlist forumadmin
	   (fold_publicnewsboxes
	      [((fold_title_box "Info")
		  :> RegisterPublicNewsBoxes.boxes);
	       ((fold_login_box_action ())
		  :> RegisterPublicNewsBoxes.boxes);
	       ((fold_news_box ())
		  :> RegisterPublicNewsBoxes.boxes)])
       ))

let user_main_page_number =
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_user_main_page_number"
       (fun () -> 
	  RegisterUserBoxes.dbinsertlist forumadmin
	   (fold_userboxes
	      [((fold_title_box "Mon site")
		  :> RegisterUserBoxes.boxes);
	       ((fold_text_box "Bonjour !")
		  :> RegisterUserBoxes.boxes);
	       ((fold_user_pages_deconnect_box ())
		  :> RegisterUserBoxes.boxes);
	       ((fold_news_headers_list_box messageslist_number)
		  :> RegisterUserBoxes.boxes)])
       ))

let user_news_page_number = 
  Krokopersist.get
    (Krokopersist.make_persistant_lazy "krokoxample_user_news_page_number"
       (fun () ->
	  RegisterUserNewsBoxes.dbinsertlist forumadmin
	   (fold_usernewsboxes
	      [((fold_title_box "Info")
		  :> RegisterUserNewsBoxes.boxes);
	       ((fold_user_pages_deconnect_box ())
		  :> RegisterUserNewsBoxes.boxes);
	       ((fold_news_box ())
		  :> RegisterUserNewsBoxes.boxes)])
       ))


(*****************************************************************************)
let accueil h = 
  page h
    ((RegisterPublicBoxes.dbgetlist
	~user:Rights.anonymoususer
	~key:public_main_page_number) h)

let print_news_page h i = 
  page h
    ((RegisterPublicNewsBoxes.dbgetlist
	~user:Rights.anonymoususer
	~key:public_news_page_number) h i)

let _ = register_url
  ~url:main_page
  ~action:accueil

let _ = register_url
  ~url:news_page
  ~action:print_news_page

let user_main_page user h =
  page h
    ((RegisterUserBoxes.dbgetlist
	~user:user
	~key:user_main_page_number) h user)

let user_news_page user h i =
  page h
    ((RegisterUserNewsBoxes.dbgetlist
	~user:user
	~key:user_news_page_number) h user i)

let launch_session user =
  register_session_url ~url:main_page ~action:(user_main_page user);
  register_session_url ~url:news_page ~action:(user_news_page user)

let _ = register_actionurl
  ~actionurl:connect_action
  ~action:(fun login password ->
	     launch_session (Rights.connect login password))


