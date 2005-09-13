open Krokodata

(******************************************************************)
(* The boxes that can appear in pages and be saved in the database *)

class virtual ['a] generic_item = object

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method virtual print : 'a

end

class virtual box boxparam sessionparam pageparam = object
  inherit [Xhtmlpp.xhtmlcont] generic_item

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method virtual print : Xhtmlpp.xhtmlcont
end

(** A box that prints an error message *)
class empty_box () sessionparam pageparam = object
  inherit box () sessionparam pageparam
  method print = << <b>empty box</b> >>
end

module RegisterBox = MakeRegister(struct 
				    type t = unit -> unit -> box
				    let default_content = new empty_box ()
				  end)
module RegisterIntBox = MakeRegister(struct 
				       type t = unit -> int -> box
				       let default_content = new empty_box ()
				     end)

  
(*****************************************************************************)
(** Some usefull boxes: *)
(** Title *)
class title_box titre sessionparam pageparam = object
  inherit box titre sessionparam pageparam
  method print = << <h1>$str:titre$</h1> >>
end

let fold_title_box = 
  RegisterBox.register ~name:"title_box" ~constructor:(new title_box)


(** A simple box that prints something *)
class text_box msg sessionparam pageparam = object
  inherit box msg sessionparam pageparam
  method print = << <div>$str:msg$</div> >>
end

let fold_text_box = 
  RegisterBox.register ~name:"text_box" 
    ~constructor:(new text_box)


(** A simple box that prints a message of the db *)
class string_message_box () sessionparam key = object
  inherit box () sessionparam key
  val msg = StringMessage.dbget key
  method print = << <div>$str:msg$</div> >>
end

let fold_string_message_box = 
  RegisterIntBox.register ~name:"string_message_box" 
    ~constructor:(new string_message_box)






(******************************************************************)
(* Now the pages *)

(** The class for description of web pages.
    We need two constructors, one to create pages from database,
    the other one manually.
 *)
class page_ sessionparam pageparam = object (moi)

  inherit [Xhtmlpp.xhtml] generic_item

  method boxlist : box list = []

  method print : Xhtmlpp.xhtml =
    let l = List.map (fun o -> o#print) moi#boxlist
    in << <html> $list:l$ </html> >>

end

class page bl sessionparam pageparam = object

  inherit page_ sessionparam pageparam
  method boxlist = bl

end

class page_fromdb boxdescrlist sessionparam pageparam = object

  inherit page_ sessionparam pageparam

  val bl = 
    List.map (fun a -> (RegisterBox.unfold a) sessionparam pageparam)
    boxdescrlist

  method boxlist = bl

end

module RegisterPage = 
  MakeRegister(struct 
		 type t = unit -> unit -> page_
		 let default_content = new page [new empty_box () () ()]
	       end)


let fold_page_fromdb = 
  RegisterPage.register ~name:"page_fromdb" 
    ~constructor:(new page_fromdb)


