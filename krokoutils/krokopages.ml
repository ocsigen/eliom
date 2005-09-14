open Krokodata

(******************************************************************)
(* The boxes that can appear in pages and be saved in the database *)

class virtual ['a] generic_item = object

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method virtual print : 'a

end

class virtual box = object
  inherit [Xhtmlpp.xhtmlcont] generic_item

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method virtual print : Xhtmlpp.xhtmlcont
end

(** A box that prints an error message *)
class empty_box = object
  inherit box
  method print = << <b>empty box</b> >>
end

module RegisterBox = MakeRegister(struct 
				    type t = box
				    let default_content = new empty_box
				  end)
module RegisterIntBox = MakeRegister(struct 
				       type t = int -> box
				       let default_content i = new empty_box
				     end)

  
(*****************************************************************************)
(** Some usefull boxes: *)
(** Title *)
class title_box titre = object
  inherit box
  method print = << <h1>$str:titre$</h1> >>
end

let fold_title_box = 
  RegisterBox.register ~name:"title_box" ~constructor:(new title_box)


(** A simple box that prints something *)
class text_box msg = object
  inherit box
  method print = << <div>$str:msg$</div> >>
end

let fold_text_box = 
  RegisterBox.register ~name:"text_box" ~constructor:(new text_box)


(** A simple box that prints a message of the db *)
class string_message_box key = object
  inherit box
  val msg = StringMessage.dbget key
  method print = << <div>$str:msg$</div> >>
end

let fold_string_message_box = 
  RegisterBox.register ~name:"string_message_box" 
    ~constructor:(new string_message_box)

let fold_string_message_intbox = 
  RegisterIntBox.register ~name:"string_message_intbox" 
    ~constructor:(fun () -> new string_message_box)




(******************************************************************)
(* Now the pages *)

(** The class for description of web pages.
    We need two constructors, one to create pages from database,
    the other one manually.
 *)
class page_ = object (moi)

  inherit [Xhtmlpp.xhtml] generic_item

  method boxlist : box list = []

  method print : Xhtmlpp.xhtml =
    let l = List.map (fun o -> o#print) moi#boxlist
    in << <html> $list:l$ </html> >>

end

class page bl = object

  inherit page_
  method boxlist = bl

end

class page_fromdb boxdescrlist = object

  inherit page_

  method boxlist = List.map (fun a -> (RegisterBox.unfold a)) boxdescrlist

end

module RegisterPage = 
  MakeRegister(struct 
		 type t = page_
		 let default_content = 
		   new page [new empty_box]
	       end)


let fold_page_fromdb = 
  RegisterPage.register ~name:"page_fromdb" ~constructor:(new page_fromdb)


(** We create a new class of pages, that can contain either 
    Boxes or int boxes
*)
class int_page_fromdb boxdescrlist n = object

  inherit page_

  method boxlist = List.map
    (function 
	 `IntBox a -> (RegisterIntBox.unfold a) n
       | `Box a -> (RegisterBox.unfold a))
    boxdescrlist
    
end

(** Pages that take one int parameter *)
module RegisterIntPage = 
  MakeRegister(struct 
		 type t = int -> page_
		 let default_content i = 
		   new page [new empty_box]
	       end)

let fold_int_page_fromdb 
    : [ `Box of Krokodata.Dyn.t | `IntBox of Krokodata.Dyn.t ] list 
    -> Krokodata.Dyn.t =
  (RegisterIntPage.register ~name:"int_page_fromdb" 
     ~constructor:(new int_page_fromdb))
(* Ici je suis obligé de préciser le type sinon il y a un type _'a qui
   ne peut être généralisé. 
   Rajouter un paramètre résoud le problème de typage mais conduit à un 
   programme FAUX parce que l'enregistrement de la page n'est pas effectué
   une fois pour toutes au début.
 *)


