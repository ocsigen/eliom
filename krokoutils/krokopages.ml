open Krokodata

(******************************************************************)
(* The boxes that can appear in pages and be saved in the database *)

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

(** This is a function used to create a new kind of basic box that can
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



(****)
(** A simple box that prints a message of the db *)
let new_string_message_box = 
  constructor_for_new_savable_data_box "string_message_box"
    (fun key ->
       let msg = StringMessage.dbget key
       in << $str:msg$ >>)


let new_string_messages_list_box =
  constructor_for_new_savable_data_box "string_messages_list_box"
    (fun key ->
       let l = 
	 List.map
	   (fun n -> let msg = StringMessage.dbget n 
	    in << <div> $str:msg$ </div> >>)
	   (MessagesList.dbget key)
       in << <div> $list:l$ </div> >>
    )
