(** An example of Ocsigen module using Ocsimore *)

open Ocsigen
open Ocsidata
open Ocsipages
open Ocsisav
open Ocsiboxes
open Rights
open Ocsexample_util


(*****************************************************************************)
(* All the urls: *)

let main_page = new_url ~path:[""] 
    ~get_params:unit ()

let news_page = new_url ["msg"] (StringMessage.index "num") ()

let connect_action = 
  new_actionurl
    ~post_params:(string "login" ** string "password")



(*****************************************************************************)
(* Register for public pages: *)
module RegisterPublicOrNotBoxes =
  MakeRegister
    (struct 
      type content = Xhtmltypes.body_content XHTML.M.elt
      type 'a t = server_params -> user -> resource -> 'a
      type box = [`PublicOrNotBox of content t tfolded]
      type boxes = [ box | RegisterBoxes.boxes ]
      let name = "OcsexampleRegisterPublicOrNotBoxes"
      let tag x = `PublicOrNotBox x
      let untag (`PublicOrNotBox x) = x
      let default_handler ex h u r = box_exn_handler ex
      let make_boxofboxes ~filter l h u r = 
	List.map (fun b -> (filter b) h u r) l
      type container_param = Xhtmltypes.div_attrib XHTML.M.attrib list option
      let container f ~box_param:(a,l) h u r = 
	boxes_container ?a:a (f ~user:u ~resource:r l h u r)
     end)

let fold_publicornotboxes = 
  RegisterPublicOrNotBoxes.register_unfolds
    ~box_constructor:(fun b h u r -> match b with
      #RegisterPublicOrNotBoxes.box as bb -> 
	RegisterPublicOrNotBoxes.unfold bb h u r
    | #RegisterBoxes.boxes as bb -> RegisterBoxes.unfolds bb
		     )

(* Register for public pages: *)
module RegisterPublicBoxes =
  MakeRegister
    (struct 
      type content = Xhtmltypes.body_content XHTML.M.elt
      type 'a t = server_params -> resource -> 'a
      type box = [`PublicBox of content t tfolded]
      type boxes = [ box | RegisterPublicOrNotBoxes.boxes ]
      let name = "OcsexampleRegisterPublicBoxes"
      let tag x = `PublicBox x
      let untag (`PublicBox x) = x
      let default_handler ex h r = box_exn_handler ex
      let make_boxofboxes ~filter l h r = 
	List.map (fun b -> (filter b) h r) l
      type container_param = Xhtmltypes.div_attrib XHTML.M.attrib list option
      let container f ~box_param:(a,l) h r = 
	boxes_container ?a:a 
	  (f ~user:anonymoususer ~resource:r l h r)
     end)

let fold_publicboxes = 
  RegisterPublicBoxes.register_unfolds
    ~box_constructor:(fun b h r -> match b with
      #RegisterPublicBoxes.box as bb -> 
	RegisterPublicBoxes.unfold bb h r
    | #RegisterPublicOrNotBoxes.boxes as bb -> 
	RegisterPublicOrNotBoxes.unfolds bb h anonymoususer r
		     )

(* Register for piece of news pages: *)
module RegisterNewsBoxes =
  MakeRegister
    (struct 
      type content = Xhtmltypes.body_content XHTML.M.elt
      type 'a t = server_params -> user -> resource ->
	StringMessage.t index -> 'a
      type box = [`NewsBox of content t tfolded]
      type boxes = box
      let name = "OcsexampleRegisterNewsBoxes"
      let tag x = `NewsBox x
      let untag (`NewsBox x) = x
      let default_handler ex h u r i = box_exn_handler ex
      let make_boxofboxes ~filter l h u r i = 
	List.map (fun b -> (filter b) h u r i) l
      type container_param = Xhtmltypes.div_attrib XHTML.M.attrib list option
      let container f ~box_param:(a,l) h u r i = 
	boxes_container ?a:a (f ~user:u ~resource:r l h u r i)
    end)

let fold_newsboxes = 
  RegisterNewsBoxes.register_unfolds
    ~box_constructor:RegisterNewsBoxes.unfold

(* Register for the public piece of news page: *)
module RegisterPublicNewsBoxes =
  MakeRegister
    (struct 
      type content = Xhtmltypes.body_content XHTML.M.elt
      type 'a t = server_params -> resource -> StringMessage.t index -> 'a
      type box = [`PublicNewsBox of content t tfolded]
      type boxes = [ box
	| RegisterNewsBoxes.boxes
	| RegisterPublicBoxes.boxes ]
      let name = "OcsexampleRegisterPublicNewsBoxes"
      let tag x = `PublicNewsBox x
      let untag (`PublicNewsBox x) = x
      let default_handler ex h r i = box_exn_handler ex
      let make_boxofboxes ~filter l h r i = 
	List.map (fun b -> (filter b) h r i) l
      type container_param = Xhtmltypes.div_attrib XHTML.M.attrib list option
      let container f ~box_param:(a,l) h r i = 
	boxes_container ?a:a 
	  (f ~user:anonymoususer ~resource:r l h r i)
    end)

let fold_publicnewsboxes = 
  RegisterPublicNewsBoxes.register_unfolds
    ~box_constructor:(fun b h r i -> match b with
      #RegisterPublicNewsBoxes.box as bb -> 
	RegisterPublicNewsBoxes.unfold bb h r i
    | #RegisterNewsBoxes.boxes as bb -> 
	RegisterNewsBoxes.unfolds bb h anonymoususer r i
    | #RegisterPublicBoxes.boxes as bb -> 
	RegisterPublicBoxes.unfolds bb h r
		     )

module RegisterUserBoxes =
  MakeRegister
    (struct 
      type content = Xhtmltypes.body_content XHTML.M.elt
      type 'a t = server_params -> user -> resource -> 'a
      type box = [`UserBox of content t tfolded]
      type boxes = [ box | RegisterPublicOrNotBoxes.boxes ]
      let name = "OcsexampleRegisterUserBoxes"
      let tag x = `UserBox x
      let untag (`UserBox x) = x
      let default_handler ex h u r = box_exn_handler ex
      let make_boxofboxes ~filter l h u r = 
	List.map (fun b -> (filter b) h u r) l
      type container_param = Xhtmltypes.div_attrib XHTML.M.attrib list option
      let container f ~box_param:(a,l) h u r = 
	boxes_container ?a:a 
	  (f ~user:u ~resource:r l h u r)
    end)

let fold_userboxes = 
  RegisterUserBoxes.register_unfolds
    ~box_constructor:(fun b h u r -> match b with
      #RegisterUserBoxes.box as bb -> 
	RegisterUserBoxes.unfold bb h u r
    | #RegisterPublicOrNotBoxes.boxes as bb -> 
	RegisterPublicOrNotBoxes.unfolds bb h u r
		     )

module RegisterUserNewsBoxes =
  MakeRegister
    (struct 
      type content = Xhtmltypes.body_content XHTML.M.elt
      type 'a t = server_params -> user -> resource ->
	StringMessage.t index -> 'a
      type box = [`UserNewsBox of content t tfolded]
      type boxes = [ box | RegisterUserBoxes.boxes | RegisterNewsBoxes.boxes ]
      let name = "OcsexampleRegisterUserNewsBoxes"
      let tag x = `UserNewsBox x
      let untag (`UserNewsBox x) = x
      let default_handler ex h u r i = box_exn_handler ex
      let make_boxofboxes ~filter l h u r i = 
	List.map (fun b -> (filter b) h u r i) l
      type container_param = Xhtmltypes.div_attrib XHTML.M.attrib list option
      let container f ~box_param:(a,l) h u r i = 
	boxes_container ?a:a 
	  (f ~user:u ~resource:r l h u r i)
    end)

let fold_usernewsboxes = 
  RegisterUserNewsBoxes.register_unfolds
    ~box_constructor:(fun b h u r i -> match b with
      #RegisterUserNewsBoxes.box as bb -> 
	RegisterUserNewsBoxes.unfold bb h u r i
    | #RegisterUserBoxes.boxes as bb -> RegisterUserBoxes.unfolds bb h u r
    | #RegisterNewsBoxes.boxes as bb -> RegisterNewsBoxes.unfolds bb h u r i
		     )


let fold_news_headers_list_box =
  RegisterPublicOrNotBoxes.register ~name:"ocsexample_news_headers_list_box" 
    ~constructor:(fun ~box_param:i h u r -> 
      news_headers_list_box h i u r news_page)

let fold_user_pages_connected_box =
  RegisterUserBoxes.register ~name:"ocsexample_user_pages_deconnect_box" 
    ~constructor:(fun ~box_param:() h u r -> connected_box h u)

let fold_news_box =
  RegisterNewsBoxes.register ~name:"ocsexample_news_box" 
    ~constructor:(fun ~box_param:() h u r i -> string_message_box i u r)

let fold_login_box_action =
  RegisterPublicBoxes.register ~name:"ocsexample_login_box_action" 
    ~constructor:(fun ~box_param:() h r -> login_box_action h connect_action)


(*****************************************************************************)
(* Construction of default pages *)

let public_main_page_number =
  Ocsipersist.get
    (Ocsipersist.make_persistant_lazy "ocsexample_public_main_page_number"
       (fun () -> 
	  RegisterPublicBoxes.dbinsertlist
	   ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
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
  Ocsipersist.get
    (Ocsipersist.make_persistant_lazy "ocsexample_public_news_page_number"
       (fun () ->
	  RegisterPublicNewsBoxes.dbinsertlist
	   ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	   (fold_publicnewsboxes
	      [((fold_title_box "Info")
		  :> RegisterPublicNewsBoxes.boxes);
	       ((fold_login_box_action ())
		  :> RegisterPublicNewsBoxes.boxes);
	       ((fold_news_box ())
		  :> RegisterPublicNewsBoxes.boxes)])
       ))

let user_main_page_number =
  Ocsipersist.get
    (Ocsipersist.make_persistant_lazy "ocsexample_user_main_page_number"
       (fun () -> 
	  RegisterUserBoxes.dbinsertlist
	   ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	   (fold_userboxes
	      [((fold_title_box "Mon site")
		  :> RegisterUserBoxes.boxes);
	       ((fold_text_box "Bonjour !")
		  :> RegisterUserBoxes.boxes);
	       ((fold_user_pages_connected_box ())
		  :> RegisterUserBoxes.boxes);
	       ((fold_news_headers_list_box messageslist_number)
		  :> RegisterUserBoxes.boxes)])
       ))

let user_news_page_number = 
  Ocsipersist.get
    (Ocsipersist.make_persistant_lazy "ocsexample_user_news_page_number"
       (fun () ->
	  RegisterUserNewsBoxes.dbinsertlist	   
	   ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	   (fold_usernewsboxes
	      [((fold_title_box "Info")
		  :> RegisterUserNewsBoxes.boxes);
	       ((fold_user_pages_connected_box ())
		  :> RegisterUserNewsBoxes.boxes);
	       ((fold_news_box ())
		  :> RegisterUserNewsBoxes.boxes)])
       ))


(*****************************************************************************)
let accueil h () () = 
  page h
    ((RegisterPublicBoxes.dbgetlist
	~user:anonymoususer
	~resource:rocsexample
	~key:public_main_page_number) h rocsexample)

let print_news_page h i () = 
  page h
    ((RegisterPublicNewsBoxes.dbgetlist
	~user:anonymoususer
	~resource:rocsexample
	~key:public_news_page_number) h rocsexample i)

let _ = register_url
  ~url:main_page
  accueil

let _ = register_url
  ~url:news_page
  print_news_page

let user_main_page user h () () =
  page h
    ((RegisterUserBoxes.dbgetlist
	~user:user
	~resource:rocsexample
	~key:user_main_page_number) h user rocsexample)

let user_news_page user h i () =
  page h
    ((RegisterUserNewsBoxes.dbgetlist
	~user:user
	~resource:rocsexample
	~key:user_news_page_number) h user rocsexample i)

let launch_session user =
  register_url_for_session ~url:main_page (user_main_page user);
  register_url_for_session ~url:news_page (user_news_page user)

let _ = register_actionurl
  ~actionurl:connect_action
  ~action:(fun h (login, password) ->
	     launch_session (connect login password))


