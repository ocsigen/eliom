type saved_data = string * int * Obj.t (* abstract in the mli *)
(* The string is the name of the class, 
   the int is the version of the class,
   the Obj.t is a data of any type
*)


(** Each time we want to create a savable class, we need to register in a table
    - the function to load it from the store (database)
    (The function to save it to the database is a method of the class).

    This module register can be used to save types that are not classes,
    encoded.
    In this case, the string is saved_data is the encoding used.
    When registering such an encoding [load] is the decode function
*)

module type REGISTER =
sig

  type t

  exception Duplicate_registering
  
  val register : name:string -> version:int ->
    load:(int -> 'data -> ('data -> saved_data) -> t) ->
    ('data -> saved_data)
      
  val get : saved_data -> t
    
end

module MakeRegister (A: sig type t end) : REGISTER with type t = A.t =
 
struct

  type t = A.t

  exception Duplicate_registering

  let table = Hashtbl.create 10
    
  let register ~name ~version ~load:constructor = 
    if Hashtbl.mem table name
    then raise Duplicate_registering
    else
      let fold = (fun x -> (name, version, Obj.repr x)) in
	(Hashtbl.add table name 
	   (fun version data -> constructor version (Obj.obj data) fold);
	 fold)
      
  let get (name, version, data) = (Hashtbl.find table name) version data

end;;

(** Generic class that can be saved in the database.
    To force registering all new class,
    it is not possible to call the constructor because we don't have any
    value of type saved_data. To instantiate the class, use the module
    below
*)
class virtual savable (fold : 'data -> saved_data) = object

  method virtual save : saved_data

end

(******************************************************************)
(* Now the boxes and pages, that are savable *)


class virtual ['a] generic_item = object

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method virtual print : 'a

end

class virtual ['a] savable_generic_item (fold : 'b -> saved_data) = object
  inherit ['a] generic_item
  inherit savable fold
end

class box = object
  inherit [Xhtmlpp.xhtmlcont] generic_item

  (** print the page in xhtml using xml extended syntax xmlp4 *)
  method print = << empty box >>

end


class savable_box fold = object
  inherit box
  inherit savable fold

  method save = fold ()
end

module RegisterBox = MakeRegister(struct 
				    type t = savable_box 
				  end)

let fold_box = RegisterBox.register ~name:"savable_box" ~version:1
  ~load:(fun version () fold -> new savable_box fold)

let new_savable_box () = new savable_box fold_box


(** The class for description of web pages. 
*)
class page boxlist = object

  inherit [Xhtmlpp.xhtml] generic_item

  val boxlist = boxlist

  method print : Xhtmlpp.xhtml =
    let l = List.map (fun o -> o#print) boxlist
    in << <html> $list:l$ </html> >>

end


(** The class for web pages that can be saved in the database. 
    I have two constructors for a page, depending whether the page
    is created from scratch or from marshalised data in the database
*)
class savable_page (boxlist : 'a list) fold = object
  inherit page boxlist
  inherit savable fold

  method save = fold (List.map (fun o -> o#save) boxlist)
end

module RegisterPage = MakeRegister(struct 
				     type t = savable_page
				   end)

let fold_page = RegisterPage.register ~name:"savable_page" ~version:1
  ~load:(fun version data fold -> 
	   let boxlist = List.map RegisterBox.get data
	   in new savable_page boxlist fold)

let new_savable_page boxlist = new savable_page boxlist fold_page



(** Some usefull boxes: *)
class title t = object
  inherit box

  method print = << <h1>$str:t$</h1> >>

end;;

class savable_title t fold = object
  inherit title t
  inherit savable fold

  method save = fold t

end;;
    
let fold_title = RegisterBox.register ~name:"savable_title" ~version:1
  ~load:(fun version data fold -> new savable_title data fold)

let new_savable_title t = new savable_title t fold_title
