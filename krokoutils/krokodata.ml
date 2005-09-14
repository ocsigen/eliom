(* A module to save and load data of any type to and from the database *)


module Dyn : sig
  type t
  exception Dyn_duplicate_registering of string
  exception Dyn_typing_error_while_unfolding of (string * string)
  val register : string -> ('a -> t) * (t -> 'a)
  val tag : t -> string
end = struct

  type t = string * Obj.t
      (* The string is the name of the class, 
	 the int is a version number,
	 the Obj.t is a data of any type
      *)

  exception Dyn_duplicate_registering of string
  exception Dyn_typing_error_while_unfolding of (string * string)

  let table = ref []

  let register name = 
    if List.mem name !table
    then raise (Dyn_duplicate_registering name)
    else
      table := name::!table;
      ((fun v -> (name, Obj.repr v)),
       (fun (name', rv) ->
	  if name = name' then Obj.magic rv 
	  else raise (Dyn_typing_error_while_unfolding (name, name'))))

  let tag (n,_) = n

end

module DyntCache =
  Krokache.Make(
    struct
      type tvalue = Dyn.t

      let sql_t_list_to_tvalue = function
          [`Binary data
          ] -> ((Marshal.from_string data 0) : tvalue)
        | _ -> raise (Krokache.Cache_error "content (probably database table wrong?)")

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

module MakeSaver (A: sig 
		    type t
		    val name : string
		    val default_content : t
		  end) : 
sig
  val dbinsert : value:A.t -> int
  val dbupdate : key:int -> value:A.t -> unit
  val dbget : key:int -> A.t
end =
struct

  let fold,unfold = Dyn.register A.name

  let dbinsert ~value = dbinsertdyn (fold value)

  let dbupdate ~key ~value = dbupdatedyn key (fold value)

  let dbget ~key = 
    try unfold (dbgetdyn key)
    with _ -> A.default_content

end


(** Now a module to save constructors for sons of 
    a particular ancestor class.
    Actually the first parameter of the constructor is saved in the database
    and the following are not.

    We create a module for each class type.
    Something like boxparam -> sessionparam -> pageparam -> < print : xhtml >
    Here t is (sessionparam -> pageparam -> < print : xhtml >)
    For each son of this class, we associate its constructor to its name
    using the register function, that will give the function fold
    for this class type as result.

    unfold is common for all the module (that is, for one type) and gives
    back the common ancestor type.
*)
module type REGISTER =
sig

  (** The type we want to save is 'boxparam -> t *)
  type t

  exception Duplicate_registering of string
  
  val register : 
    name:string -> constructor:('boxparam -> t) -> ('boxparam -> Dyn.t)

  (** [unfold] can raise an exception! *)
  val unfold : Dyn.t -> t

  (** [dbget] can raise an exception! *)
  val dbget : key:int -> t

end

module MakeRegister (A: sig 
		       type t
		       val default_content : t
		     end) 
  : REGISTER with type t = A.t =
 
struct

  type t = A.t

  exception Duplicate_registering of string

  let table = Hashtbl.create 20

  let register ~name ~constructor = 
    if Hashtbl.mem table name
    then raise (Duplicate_registering name)
    else let fold,unfold = Dyn.register name 
    in (Hashtbl.add table name 
	  (fun data -> constructor (unfold data));
	fold)

  let unfold saved_data =
    try 
      (Hashtbl.find table (Dyn.tag saved_data)) saved_data
    with _ -> A.default_content

  let dbget ~key = 
    try 
      unfold (dbgetdyn key)
    with _ -> A.default_content
      
end;;






(*****************************************************************************)
(** Now the messages *)
(** We can save messages in the database. 
    (Some boxes are used print these messages) 
    Messages have no print method, as the printing depends on the kind of
    box it is in.
    Actually for messages we don't use objects.
    We use the Dyn module and dbinsert dbupdate to save in the db.
*)

(****)
(** A simple string message *)
module StringMessage = 
  MakeSaver (struct 
	       type t = string
	       let default_content = "Message not found"
	       let name = "string_message"
	     end)

(** A list of messages numbers *)
module MessagesList = 
  MakeSaver (struct 
	       type t = int list
	       let default_content = []
	       let name = "message_list"
	     end)









(* Old registering of objects
(** Now a functor to save objects. Objects cannot be marshaled (or you need
    to use them always with exactly the same code, which is not what we want).
    We need to code the object before saving. Each savable object has a
    save method, that will code the object into a caml datastructure.
    To decode the object, we register the uncoding function in a table.
    BUT : when loading an object from the table, we lose its precise class 
    type, because we use inclusion polymorphism. All the objects loaded from 
    a Register module are sons of a particular class. For ex, they all have 
    a print method.

    Each time we want to create a savable class, we need to register in a table
    - the function to load it from the store (database)
    (The function to save it to the database is a method of the class).

    We need a register module for each ancestor class, in which we register
    all sons classes.

    Warning! Keep in mind that get can fail and raise and exception.
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

  (** [get] can raise an exception! *)
  val get : saved_obj -> t

end

module MakeRegister (A: sig 
		       type t
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
class virtual savable (fold : 'data -> saved_obj) = object

  method virtual save : saved_obj

end

(** A generic savable class that saves the data given to the constructor *)
class savable_data (data : 'data) (fold : 'data -> saved_obj) = 
object

  inherit savable fold
  method save = fold data

end
*)


