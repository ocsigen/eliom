module Dyn : sig
  type t
  exception Dyn_duplicate_registering
  exception Dyn_unfold_error
  val register : string -> ('a -> t) * (t -> 'a)
  val tag : t -> string
end = struct

  type t = string * Obj.t
      (* The string is the name of the class, 
	 the int is a version number,
	 the Obj.t is a data of any type
      *)

  exception Dyn_duplicate_registering
  exception Dyn_unfold_error

  let table = ref []

  let register name = 
    if List.mem name !table
    then raise Dyn_duplicate_registering
    else
      table := name::!table;
      ((fun v -> (name, Obj.repr v)),
       (fun (name', rv) ->
	  if name = name' then Obj.magic rv 
	  else raise Dyn_unfold_error))

  let tag (n,_) = n

end


module DyntCache =
  Cache.Make(
    struct
      type tvalue = Dyn.t
	  
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

let dbinsertdyn ~value = DyntCache.insert value

let dbupdatedyn ~key ~value = DyntCache.update key value

let dbgetdyn ~key = DyntCache.get key


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
type saved_obj = Dyn.t (* abstract in the mli *)

module type REGISTER =
sig

  (** The type we want to save *)
  type t

  exception Duplicate_registering
  
  val register : name:string ->
    decode:('encoded_data -> ('encoded_data -> saved_obj) -> t) ->
    ('encoded_data -> saved_obj)
      
  val get : saved_obj -> t

end

module MakeRegister (A: sig 
		       type t
		       type encoding_default
		       val make_default : 
			 ((encoding_default -> saved_obj) -> saved_obj) -> t
		     end) 
  : REGISTER with type t = A.t =
 
struct

  type t = A.t

  exception Duplicate_registering

  let table = Hashtbl.create 10

  let register ~name ~decode = 
    if Hashtbl.mem table name
    then raise Duplicate_registering
    else let fold,unfold = Dyn.register name 
    in (Hashtbl.add table name 
	  (fun data -> decode (unfold data) fold);
	fold)
      
  let get data = 
    (Hashtbl.find table (Dyn.tag data)) data

end;;

let dbinsertobj ~value = DyntCache.insert value#save

let dbupdateobj ~key ~value = DyntCache.update key value#save




(** Generic class that can be saved in the database.
    To force registering all new class,
    it is not possible to call the constructor because we don't have any
    value of type saved_data. To instantiate the class, use the module
    below
*)
class virtual ['data] savable (fold : 'data -> saved_obj) = object

  method virtual save : saved_obj

end

(** A generic savable class that saves the data given to the constructor *)
class ['data] savable_data (data : 'data) (fold : 'data -> saved_obj) = 
object

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
				    type encoding_default = unit
				    let make_default = new savable_box
				  end)

let fold_box = RegisterBox.register ~name:"savable_box"
  ~decode:(fun () -> new savable_box)

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
  inherit [saved_obj list] savable fold

  method save = fold (List.map (fun o -> o#save) boxlist)
end

module RegisterPage = 
  MakeRegister(struct 
		 type t = savable_page
		 type encoding_default = saved_obj list
		 let make_default = 
		   new savable_page 
		     [new_error_box "Unknown page"]
	       end)

let fold_page = RegisterPage.register ~name:"savable_page"
  ~decode:(fun data -> 
	   let boxlist = List.map RegisterBox.get data
	   in new savable_page boxlist)

let new_savable_page boxlist = new savable_page boxlist fold_page

let dbinsert_page boxlist = dbinsertobj (new_savable_page boxlist)

let dbget_page ~key = RegisterPage.get (DyntCache.get key)

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

