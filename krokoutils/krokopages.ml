open Krokodata

(******************************************************************)
(* The boxes that can appear in pages and be saved in the database *)

(** A box that prints an error message *)
let empty_box = << <b>empty box</b> >>

module RegisterBox = MakeRegister(struct 
				    type t = Xhtmlpp.xhtmlcont
				    let default_content = empty_box
				  end)
module RegisterIntBox = MakeRegister(struct 
				       type t = int -> Xhtmlpp.xhtmlcont
				       let default_content i = empty_box
				     end)

  
(*****************************************************************************)
(** Some usefull boxes: *)
(** Title *)
let title_box titre = << <h1>$str:titre$</h1> >>

let fold_title_box = 
  RegisterBox.register ~name:"title_box" ~constructor:title_box


(** A simple box that prints something *)
let text_box msg = << <div>$str:msg$</div> >>

let fold_text_box = 
  RegisterBox.register ~name:"text_box" ~constructor:text_box


(** A simple box that prints a message of the db *)
let string_message_box key =
  let msg = StringMessage.dbget key
  in << <div>$str:msg$</div> >>

let fold_string_message_box = 
  RegisterBox.register ~name:"string_message_box" 
    ~constructor:string_message_box

let fold_string_message_intbox = 
  RegisterIntBox.register ~name:"string_message_intbox" 
    ~constructor:(fun () -> string_message_box)




(******************************************************************)
(* Now the pages *)

(** The class for description of web pages.
    We need two constructors, one to create pages from database,
    the other one manually.
 *)

let page bl = << <html> $list:bl$ </html> >>

let page_fromdb boxdescrlist = 
  page (List.map (fun a -> (RegisterBox.unfold a)) boxdescrlist)

let empty_page = page <:xmllist< <b>empty page</b> >>

module RegisterPage = 
  MakeRegister(struct 
		 type t = Xhtmlpp.xhtml
		 let default_content = empty_page
	       end)


let fold_page_fromdb = 
  RegisterPage.register ~name:"page_fromdb" ~constructor:page_fromdb


(** We create a new kind of pages, that can contain either 
    Boxes or boxes with a parameter
*)
let param_page_fromdb boxdescrlist n = 
  page
    (List.map
       (function 
	    `ParamBox a -> (RegisterIntBox.unfold a) n
	  | `Box a -> (RegisterBox.unfold a))
       boxdescrlist)


(** Pages that take one int parameter *)
module RegisterIntPage = 
  MakeRegister(struct 
		 type t = int -> Xhtmlpp.xhtml
		 let default_content i = empty_page
	       end)

let fold_int_page
    : [ `Box of Krokodata.Dyn.t | `ParamBox of Krokodata.Dyn.t ] list 
    -> Krokodata.Dyn.t =
  (RegisterIntPage.register ~name:"int_page_fromdb" 
     ~constructor:param_page_fromdb)
(* Ici je suis obligé de préciser le type sinon il y a un type _'a qui
   ne peut être généralisé. 
   Rajouter un paramètre résoud le problème de typage mais conduit à un 
   programme FAUX parce que l'enregistrement de la page n'est pas effectué
   une fois pour toutes au début.
 *)
