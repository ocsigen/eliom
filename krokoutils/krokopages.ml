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
				       let default_content = 
					 (fun i -> new empty_box)
				     end)
module RegisterStringBox = MakeRegister(struct 
					  type t = string -> box
					  let default_content = 
					    (fun s -> new empty_box)
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
  RegisterIntBox.register ~name:"string_message_box" 
    ~constructor:(fun () -> new string_message_box)

(** A box that contains another box but removing a parameter *)
class forget_param_box boxdescr param = object
  inherit box
  val b = (RegisterBox.unfold boxdescr)#print
  method print = b
end

let fold_forget_param_int_box = 
  RegisterIntBox.register ~name:"forget_param_int_box" 
    ~constructor:(new forget_param_box)

let fold_forget_param_string_box = 
  RegisterStringBox.register ~name:"forget_param_string_box"
    ~constructor:(new forget_param_box)








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

  val bl = 
    List.map (fun a -> (RegisterBox.unfold a))
    boxdescrlist

  method boxlist = bl

end

module RegisterPage = 
  MakeRegister(struct 
		 type t = page_
		 let default_content = 
		   new page [new empty_box]
	       end)


let fold_page_fromdb = 
  RegisterPage.register ~name:"page_fromdb" 
    ~constructor:(new page_fromdb)


(* Pages that take an int paramater *)
class param_page_fromdb boxdescrlist param = object

  inherit page_

  val bl = 
    List.map (fun a -> (RegisterIntBox.unfold a) param)
      boxdescrlist

  method boxlist = bl

end

module RegisterIntPage = 
  MakeRegister(struct 
		 type t = int -> page_
		 let default_content = 
		   fun _ -> new page [new empty_box]
	       end)

let fold_int_page_fromdb = 
  RegisterIntPage.register ~name:"int_page_fromdb" 
    ~constructor:(new param_page_fromdb)


module RegisterStringPage = 
  MakeRegister(struct 
		 type t = string -> page_
		 let default_content = 
		   fun _ -> new page [new empty_box]
	       end)

let fold_string_page_fromdb = 
  RegisterIntPage.register ~name:"string_page_fromdb" 
    ~constructor:(new param_page_fromdb)


