open Krokodata

(******************************************************************)
(* The boxes that can appear in pages *)

class virtual ['a] generic_item = object

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method virtual print : 'a

end

class box = object
  inherit [Xhtmlpp.xhtmlcont] generic_item

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method print = << empty box >>

end


class savable_box data fold = object
  inherit box
  inherit savable_data data fold
end

module RegisterBox = MakeRegister(struct 
				    type t = savable_box 
				  end)

let fold_box = RegisterBox.register ~name:"savable_box"
  ~decode:(fun () -> new savable_box ())

let new_savable_box () = new savable_box () fold_box

class data_box data print = object
  inherit box
  method print = print data
end

class savable_data_box (data:'data) print fold = object
  inherit data_box data print
  inherit savable_data data fold
end

(** This is the main function used to create a new kind of box that can
    be saved in the database. You give the name of the box and the function
    to print its contents. 
    It returns the constructor for an object with a method [print]
    and a method [save].
*)
let constructor_for_new_savable_data_box name print =
    let fold = RegisterBox.register ~name:name
      ~decode:(fun data -> new savable_data_box data print)
    in fun data -> new savable_data_box data print fold

(*****************************************************************************)
(** Some usefull boxes: *)
(** Title *)
let new_title_box = 
  constructor_for_new_savable_data_box "title_box"
    (fun titre -> << <h1>$str:titre$</h1> >>)

(* Last line is equivalent to:
   class title t = object
     inherit box
   
     method print = << <h1>$str:t$</h1> >>
   
   end;;
   
   class savable_title t fold = object
     inherit title t
     inherit [string] savable_data t fold
   end;;
   
   let fold_title = RegisterBox.register ~name:"savable_title"
     ~decode:(fun data -> new savable_title data)
   
   let new_savable_title t = new savable_title t fold_title
*)

(****)
(** A box that prints an error message *)
let new_error_box = 
  constructor_for_new_savable_data_box "error_box"
    (fun msg -> << <b>$str:msg$</b> >>)

let error_box = new_error_box "Unknown box"

let get_box v = 
  try RegisterBox.get v
  with _ -> error_box

let dbget_box ~key = 
  try RegisterBox.get (ObjCache.get key)
  with _ -> error_box

(******************************************************************)
(* Now the pages *)

(** The class for description of web pages. 
*)
class page boxlist = object

  inherit [Xhtmlpp.xhtml] generic_item

  val boxlist = boxlist

  method print : Xhtmlpp.xhtml =
    let l = List.map (fun o -> o#print) boxlist
    in << <html> $list:l$ </html> >>

end

(****)
(** The class for web pages that can be saved in the database. 
*)
class savable_page (boxlist : savable_box list) fold = object
  inherit page boxlist
  inherit savable fold

  method save = fold (List.map (fun o -> o#save) boxlist)
end

module RegisterPage = 
  MakeRegister(struct 
		 type t = savable_page
	       end)

let fold_page = RegisterPage.register ~name:"savable_page"
  ~decode:(fun data -> 
	   let boxlist = List.map get_box data
	   in new savable_page boxlist)

let new_savable_page boxlist = new savable_page boxlist fold_page

let error_page =  new_savable_page [new_error_box "Unknown page"]

let dbinsert_page boxlist = dbinsertobj (new_savable_page boxlist)

let get_page v = 
  try RegisterPage.get v
  with _ -> error_page

let dbget_page ~key = 
  try RegisterPage.get (ObjCache.get key)
  with _ -> error_page

(*****************************************************************************)
(** Now the messages *)
(** We can save messages in the database. 
    (Some boxes are used print these messages) 
    Messages have no print method, as the printing depends on the kind of
    box it is in.
    Actually for messages we don't use objects.
    We use the Dyn module and dbinsert dbupdate to save in the db.
*)

module MakeNewMessage (A: sig 
			 type t
			 val name : string
			 val default_content : t
		       end) : 
sig
  val dbinsert_message : A.t -> int
  val dbupdate_message : key:int -> value:A.t -> unit
  val dbget_message : key:int -> A.t
end =
struct

  let fold,unfold = Dyn.register A.name

  let dbinsert_message d = dbinsertdyn (fold d)

  let dbupdate_message ~key ~value = dbupdatedyn key (fold value)

  let dbget_message ~key = 
    try unfold (dbgetdyn key)
    with _ -> A.default_content

end


(****)
(** A simple string message *)
module StringMessage = 
  MakeNewMessage(struct 
		   type t = string
		   let default_content = "Message not found"
		   let name = "string_message"
		 end)

(****)
(** A simple box that prints a message of the db *)
let new_string_message_box = 
  constructor_for_new_savable_data_box "string_message_box"
    (fun key -> 
       let msg = StringMessage.dbget_message key
       in << $str:msg$ >>)

