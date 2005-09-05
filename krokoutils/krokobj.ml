type saved_data = string * int * Obj.t (* abstract in the mli *)
(* The string is the name of the class, 
   the int is the version of the class,
   the Obj.t is a data of any type
*)

module SavedDataCache =
  Cache.Make(
    struct
      type tvalue = saved_data
	  
      let sql_t_list_to_tvalue = function
	  [`Binary data
	  ] -> ((Marshal.from_string data 0) : tvalue)
	| _ -> raise (Cache.Cache_error "content (probably database table wrong?)")
	    
      let tvalue_to_sql_t_list o =
	[`Binary (Marshal.to_string o [])
	]
	  
      let sql_fields_list_without_key = ["data"]
	
      let table = "content"
	
      let key = "content_key"
	
    end)

let dbinsert ~value = SavedDataCache.insert value#save

let dbupdate ~key ~value = SavedDataCache.update key value#save


(** Each time we want to create a savable class, we need to register in a table
    - the function to load it from the store (database)
    (The function to save it to the database is a method of the class).

    This module register can be used to save types that are not classes,
    encoded.
    In this case, the string is saved_data is the encoding used.
    When registering such an encoding [load] is the decode function

    We need a register module for each type of data we want to save,
    in order to get back the right type.
    When constructing the Register module for a type using MakeRegister, 
    we give a constructor for a default value that will be given by
    get and dbget if they didn't manage to find the value.
*)

module type REGISTER =
sig

  (** The type we want to save *)
  type t

  (** The type in with it will be encoded before saving *)
  type t_encoded

  exception Duplicate_registering
  
  val register : name:string -> version:int ->
    load:(int -> 'data -> ('data -> saved_data) -> t) ->
    ('data -> saved_data)
      
  val get : saved_data -> t

  val dbget : key:int -> t
    
end

module MakeRegister (A: sig 
		       type t
		       type t_encoded
		       val make_default : (t_encoded -> saved_data) -> t
		     end) 
  : REGISTER with type t = A.t and type t_encoded = A.t_encoded =
 
struct

  type t = A.t
  type t_encoded = A.t_encoded

  exception Duplicate_registering

  let table = Hashtbl.create 10

  let fold name version = (fun x -> (name, version, Obj.repr x))
    
  let register ~name ~version ~load:constructor = 
    if Hashtbl.mem table name
    then raise Duplicate_registering
    else
      (Hashtbl.add table name 
	 (fun version data -> 
	    constructor version (Obj.obj data) (fold name version));
       fold name version)
      
  let default = A.make_default (fold "default" 1)

  let get (name, version, data) = 
    try (Hashtbl.find table name) version data
    with _ -> default

  let dbget ~key = 
    try get (SavedDataCache.get key)
    with _ -> default

end;;


(** Generic class that can be saved in the database.
    To force registering all new class,
    it is not possible to call the constructor because we don't have any
    value of type saved_data. To instantiate the class, use the module
    below
*)
class virtual ['data] savable (fold : 'data -> saved_data) = object

  method virtual save : saved_data

end

(** A generic savable class that saves the data given to the constructor *)
class ['data] savable_data (data : 'data) (fold : 'data -> saved_data) = object

  inherit ['data] savable fold
  method save = fold data

end

(******************************************************************)
(* Now the boxes that can appear in pages *)

class virtual ['a] generic_item = object

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method virtual print : 'a

end

class box = object
  inherit [Xhtmlpp.xhtmlcont] generic_item

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method print = << empty box >>

end


class savable_box fold = object
  inherit box
  inherit [unit] savable_data () fold
end

module RegisterBox = MakeRegister(struct 
				    type t = savable_box 
				    type t_encoded = unit
				    let make_default = new savable_box
				  end)

let fold_box = RegisterBox.register ~name:"savable_box" ~version:1
  ~load:(fun version () -> new savable_box)

let new_savable_box () = new savable_box fold_box

class data_box data print = object
  inherit box
  method print = print data
end

class ['data] savable_data_box (data:'data) print fold = object
  inherit data_box data print
  inherit ['data] savable_data data fold
end

(** This is the main function used to create a new kind of box that can
    be saved in the database. You give the name of the box and the function
    to print its contents. 
    It returns the constructor for an object with a method [print]
    and a method [save].
*)
let constructor_for_new_savable_data_box name version print =
    let fold = RegisterBox.register ~name:name ~version:version
      ~load:(fun version data -> new savable_data_box data print)
    in fun data -> new savable_data_box data print fold

(*****************************************************************************)
(** Some usefull boxes: *)
(** Title *)
let new_title_box = 
  constructor_for_new_savable_data_box "title_box" 1
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
   
   let fold_title = RegisterBox.register ~name:"savable_title" ~version:1
     ~load:(fun version data -> new savable_title data)
   
   let new_savable_title t = new savable_title t fold_title
*)

(****)
(** A box that prints an error message *)
let new_error_box = 
  constructor_for_new_savable_data_box "error_box" 1
    (fun msg -> << <b>$str:msg$</b> >>)


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
  inherit [saved_data list] savable fold

  method save = fold (List.map (fun o -> o#save) boxlist)
end

module RegisterPage = 
  MakeRegister(struct 
		 type t = savable_page
		 type t_encoded = saved_data list
		 let make_default = 
		   new savable_page 
		     [new_error_box "Unknown page"]
	       end)

let fold_page = RegisterPage.register ~name:"savable_page" ~version:1
  ~load:(fun version data -> 
	   let boxlist = List.map RegisterBox.get data
	   in new savable_page boxlist)

let new_savable_page boxlist = new savable_page boxlist fold_page

let dbinsert_page boxlist = dbinsert (new_savable_page boxlist)

(*****************************************************************************)
(** Now the messages *)
(** We can save messages in the database. 
    (Some boxes are used print these messages) 
    Messages have no print method, as the printing depends on the kind of
    box it is in.
    But messages have a method called get, used to give the content of
    the message.
    Each kind of messages has a different type for get.
    That's why we need a Register table for each of these types.
*)
class ['a] generic_message data = object
  method get : 'a = data
end

class ['a] savable_message data fold = object
  inherit ['a] generic_message data
  inherit ['a] savable_data data fold
end

module MakeNewMessage (A: sig 
			 type t
			 val default_content : t
			 val name : string
		       end) : 
sig
  val new_message : A.t -> A.t savable_message
  val dbinsert_message : A.t -> int
  val dbupdate_message : key:int -> value:A.t -> unit
  val dbget_message : key:int -> A.t savable_message
end =
struct
  module Reg = MakeRegister(struct
			      type t = A.t savable_message
			      type t_encoded = A.t
			      let make_default =
				new savable_message A.default_content
			    end)

  let fold = Reg.register 
    ~name:A.name ~version:1
    ~load:(fun version data -> new savable_message data)
    
  let new_message t = new savable_message t fold

  let dbinsert_message d = dbinsert (new_message d)

  let dbupdate_message ~key ~value = dbupdate key (new_message value)

  let dbget_message = Reg.dbget
end


(****)
(** A simple string message *)
module StringMessage = 
  MakeNewMessage(struct 
		   type t = string
		   let default_content = "Message not found"
		   let name = "string_message"
		 end)

(* The last module definition is equivalent to:
module RegisterStringMessage = 
  MakeRegister(struct
  		 type t = string savable_message
		 type t_encoded = string
		 let make_default = new savable_message "message not found" 
	       end)

let fold_string_message = RegisterStringMessage.register
  ~name:"string_savable_message" ~version:1
  ~load:(fun version data -> new savable_message data)

let new_string_savable_message t =
  new savable_message t fold_string_message
*)




(****)
(** A simple box that prints a message of the db *)
let new_string_message_box = 
  constructor_for_new_savable_data_box "string_message_box" 1
    (fun key -> 
       let msg = (StringMessage.dbget_message key)#get
       in << $str:msg$ >>)

(* The last def is equivalent to:

class string_message_box key = object
  inherit box
  method print =
    let message = (RegisterStringMessage.dbget key)#get
    in << $str:message$ >>
end

class savable_string_message_box key fold = object
  inherit string_message_box key
  inherit [int] savable_data key fold
end

let fold_savable_string_message_box = RegisterBox.register
  ~name:"savable_string_message_box" ~version:1
  ~load:(fun version data -> new savable_string_message_box data)

let new_savable_string_message_box t = 
  new savable_string_message_box t fold_savable_string_message_box
*)
