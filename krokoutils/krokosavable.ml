open Krokodata
open Krokopages

(******************************************************************)
(* Tools to save boxes and pages in the database *)


(* aeffffffffff

class boxes_register (boxes : boxes_class) = object
  inherit register
  val fold_title_box =
    register 
      ~name:"print_title" 
      ~constructor:(fun s -> boxes#print_title s)
  val fold_text_box =
    register 
      ~name:"print_text" 
      ~constructor:(fun s -> boxes#print_text s)
  method fold_title_box = fold_title_box
  method fold_text_box = fold_text_box
end

*)

(*
exception Duplicate_registering of string

let new_register () =
  let reg = ref [] in
  fun ~name ~constructor ->
    if List.mem_assoc name !reg
    then raise (Duplicate_registering name)
    else let fold,unfold = Dyn.register name 
    in (reg :=  (name, (fun data -> constructor (unfold data)))::!reg;
	fold)
*)
(*
class boxes_list = 
  let register = 
    let module Reg = 
      MakeRegister(struct 
		     type t = Xhtmlpp.xhtmlcont
		     let name = "boxes"
		     let default_handler = box_exn_handler
		     let default_unfolds = []
		   end) in
      Reg.register 
  in
  let fold_title_box = register ~name:"title_box" ~constructor:title_box
  and fold_text_box  = register ~name:"text_box"  ~constructor:text_box
  in object
      val mutable boxlist = []
      method add_title_box d = boxlist <- (fold_title_box d)::boxlist
      method add_text_box (d:string) = boxlist <- (fold_text_box d)::boxlist
(*      method dbgetpage
      method dbinsertpage *)
    end

class standardboxes_list =

class userboxes_list = 
  let register = 
    let module Reg = 
      MakeRegister(struct 
		     type t = Rights.user -> Xhtmlpp.xhtmlcont
		     let name = "userboxes"
		     let default_handler = box_exn_handler
		     let default_unfolds = []
		   end) in
      Reg.register 
  in object
    end


class userintboxes_list = 
  let register, unfold = 
    let module Reg = 
      MakeRegister(struct 
       type t = Rights.user -> int -> Xhtmlpp.xhtmlcont
       let name = "userintboxes"
       let default_handler ex u i = box_exn_handler ex
       let default_unfolds = []
		   end)
    in (Reg.register, Reg.unfold)
  in
  let fold_string_message_box = 
    register ~name:"string_message_box" 
      ~constructor:(fun () u i -> string_message_box i u)
  in object
      inherit boxes_list
      method add_string_message_box d = 
	boxlist <- (fold_string_message_box d)::boxlist
    end
*)
(*
J'ai essayé aussi de paramétrer les constructeurs par la classe de boîtes
à utiliser pour choisir la classe de boîte au moment du chargement de la page
depuis la base de données, et non au moment de sa sauvegarde.
mais ce n'est pas terrible parce que du coup on ne peut plus rajouter
de nouvelles boîtes dans une page sans créer un nouveau Register de boîtes 
à chaque fois...
*)


(* First of all, we create a register for all kind of pages we want *)
module RegisterBoxes =
  MakeRegister(struct 
		 type t = Xhtmlpp.xhtmlcont
		 let name = "boxes"
		 let default_handler = box_exn_handler
		 let default_tables = []
	       end)

(*
module RegisterUserBoxes = 
  MakeRegister(struct 
		 type t = Rights.user -> Xhtmlpp.xhtmlcont
		 let name = "userboxes"
		 let default_handler ex u = box_exn_handler ex
		 let default_tables =
		   [RegisterBoxes.get_table (fun d u -> d)]
	       end)

module RegisterHPBoxes = 
  MakeRegister(struct 
		 type t = Omlet.http_params -> Xhtmlpp.xhtmlcont
		 let name = "hpboxes"
		 let default_handler ex h = box_exn_handler ex
		 let default_tables =
		   [RegisterBoxes.get_table (fun d h -> d)]
	       end)

module RegisterHPUserBoxes = 
  MakeRegister(struct 
		 type t = Omlet.http_params -> Rights.user -> Xhtmlpp.xhtmlcont
		 let name = "hpuserboxes"
		 let default_handler ex h u = box_exn_handler ex
		 let default_tables =
		   [RegisterUserBoxes.get_table (fun d h u -> d u);
		    RegisterHPBoxes.get_table (fun d h u -> d h);
		    RegisterBoxes.get_table (fun d h u -> d)]
	       end)

module RegisterUserIntBoxes =
  MakeRegister
    (struct 
       type t = Rights.user -> int -> Xhtmlpp.xhtmlcont
       let name = "userintboxes"
       let default_handler ex u i = box_exn_handler ex
       let default_tables =
	 [RegisterBoxes.get_table (fun d u i -> d);
	  RegisterUserBoxes.get_table (fun d u i -> d u)]
     end)

module RegisterHPUserIntBoxes =
  MakeRegister
    (struct 
       type t = Omlet.http_params -> Rights.user -> int -> Xhtmlpp.xhtmlcont
       let name = "hpuserintboxes"
       let default_handler ex h u i = box_exn_handler ex
       let default_tables =
	 [RegisterHPUserBoxes.get_table (fun d h u i -> d h u);
	  RegisterHPBoxes.get_table (fun d h u i -> d h);
	  RegisterUserBoxes.get_table (fun d h u i -> d u);
	  RegisterBoxes.get_table (fun d h u i -> d)]
     end)
*)

(*
let boxes_page ~key =
  page (RegisterBoxes.dbgetlist 
	  ~user:Rights.anonymoususer
	  ~filter:(fun d -> d)
	  ~key:key)

let hpboxes_page ~key h =
  page (RegisterHPBoxes.dbgetlist 
	  ~user:Rights.anonymoususer
	  ~filter:(fun d -> d h)
	  ~key:key)

let userboxes_page ~key u =
  page (RegisterUserBoxes.dbgetlist 
	  ~user:u
	  ~filter:(fun d -> d u)
	  ~key:key)

let hpuserboxes_page ~key h u =
  page (RegisterHPUserBoxes.dbgetlist 
	  ~user:u
	  ~filter:(fun d -> d h u)
	  ~key:key)

let userintboxes_page ~key u i =
  page (RegisterUserIntBoxes.dbgetlist 
	  ~user:u
	  ~filter:(fun d -> d u i)
	  ~key:key)

let hpuserintboxes_page ~key h u i =
  page (RegisterHPUserIntBoxes.dbgetlist 
	  ~user:u
	  ~filter:(fun d -> d h u i)
	  ~key:key)
*)

(* Then register all constructors in the right register *)

let fold_title_box = 
  RegisterBoxes.register 
    ~name:"title_box" 
    ~constructor:(fun ~box_param -> title_box box_param)

let fold_text_box = 
  RegisterBoxes.register 
    ~name:"text_box" ~constructor:(fun ~box_param -> text_box box_param)

(*
let fold_string_message_box = 
  RegisterUserIntBoxes.register ~name:"string_message_box" 
    ~constructor:(fun ~(box_param:unit) u i -> string_message_box i u)
*)




























(****** aefffffffffffffffffffffffffffffffffffffffffffffffffffff
(* Create a register for each type of page: *)
module RegisterPage = 
  MakeRegister(struct
		 type t = Xhtmlpp.xhtml
		 let default_handler ex = page_exn_handler ex
		 let default_unfolds = []
	       end)

module RegisterUserPage =
  MakeRegister
    (struct 
       type t = Rights.user -> Xhtmlpp.xhtml
       let default_handler ex u = page_exn_handler ex
       let default_unfolds = []
     end)

module RegisterUserIntPage =
  MakeRegister
    (struct 
       type t = Rights.user -> int -> Xhtmlpp.xhtml
       let default_handler ex u i = page_exn_handler ex
       let default_unfolds = []
     end)

(* Register each page in the right table: *)
let fold_page =
  RegisterPage.register ~name:"userpage"
    ~constructor:
    (fun bdl -> page (List.map (fun a -> RegisterBoxes.unfold a) bdl))

let fold_userpage =
  RegisterUserPage.register ~name:"userpage" 
    ~constructor:(fun bdl u -> 
		    page
		      (List.map 
			 (fun a -> RegisterUserBoxes.unfold a u)
			 bdl))

let fold_userintpage =
  RegisterUserIntPage.register ~name:"userintpage" 
    ~constructor:(fun bdl u i -> 
		    page
		      (List.map 
			 (fun a -> RegisterUserIntBoxes.unfold a u i)
			 bdl))
*******************)


(* AEFFFFFFFFFFFFFFFFFFFFFFF
let box_exn_handler = function
    Rights.Read_Forbidden -> error_box "You cannot read this data"
  | Rights.Write_Forbidden -> error_box "You don't have write access to this data"
  | Rights.Permission_Denied -> error_box "Permission denied"
  | Rights.Wrong_Password -> error_box "Wrong password"
  | Not_found -> error_box "not found"
  | _ -> error_box "unknown error while creating box"

let page_exn_handler ex = page [box_exn_handler ex]

module RegisterBox = 
  MakeRegister(struct 
		 type t = Xhtmlpp.xhtmlcont
		 let default_handler = box_exn_handler
	       end)

let fold_text_box = 
    RegisterBox.register ~name:"text_box" ~constructor:text_box

let fold_title_box = 
    RegisterBox.register ~name:"title_box" ~constructor:title_box

module RegisterPage = 
  MakeRegister(struct 
		 type t = Xhtmlpp.xhtml
		 let default_handler = page_exn_handler
	       end)

let page_fromdb boxdescrlist = 
  page (List.map RegisterBox.unfold boxdescrlist)

let fold_page = 
  RegisterPage.register ~name:"page_fromdb" ~constructor:page_fromdb


(** Pages that take one int parameter *)
module RegisterIntPage = 
  MakeRegister(struct 
		 type t = int -> Xhtmlpp.xhtml
		 let default_handler ex i = page_exn_handler ex
	       end)

module RegisterIntBox = 
  MakeRegister(struct 
		 type t = int -> Xhtmlpp.xhtmlcont
		 let default_handler ex i = box_exn_handler ex
	       end)

let intpage_fromdb boxdescrlist p = 
  page (List.map (fun a -> (RegisterIntBox.unfold a) p) boxdescrlist)

let fold_intpage =
  RegisterIntPage.register ~name:"intpage" ~constructor:intpage_fromdb

let fold_boxes_intbox =
    RegisterIntBox.register 
      ~name:"boxes_intbox" 
      ~constructor:(fun bdl i -> div (List.map RegisterBox.unfold bdl))

let fold_box_intbox =
    RegisterIntBox.register 
      ~name:"box_intbox" 
      ~constructor:(fun x i -> RegisterBox.unfold x)


(** Pages that take one user parameter *)
module RegisterUserPage = 
  MakeRegister(struct 
		 type t = Rights.user -> Xhtmlpp.xhtml
		 let default_handler ex u = page_exn_handler ex
	       end)

module RegisterUserBox = 
  MakeRegister(struct 
		 type t = Rights.user -> Xhtmlpp.xhtmlcont
		 let default_handler ex u = box_exn_handler ex
	       end)

let userpage_fromdb boxdescrlist p = 
  page (List.map (fun a -> (RegisterUserBox.unfold a) p) boxdescrlist)

let fold_userpage =
    RegisterUserPage.register ~name:"userpage_fromdb" 
    ~constructor:userpage_fromdb

let fold_box_userbox =
    RegisterUserBox.register 
      ~name:"box_userbox" 
      ~constructor:(fun x i -> RegisterBox.unfold x)

let fold_boxes_userbox =
    RegisterUserBox.register 
      ~name:"boxes_userbox" 
      ~constructor:(fun bdl i -> div (List.map RegisterBox.unfold bdl))

(** Pages that take one user and one int parameter *)
module RegisterUserIntPage =
  MakeRegister(struct 
		 type t = Rights.user -> int -> Xhtmlpp.xhtml
		 let default_handler ex u i = page_exn_handler ex
	       end)

module RegisterUserIntBox = 
  MakeRegister(struct 
		 type t = Rights.user -> int -> Xhtmlpp.xhtmlcont
		 let default_handler ex u i = box_exn_handler ex
	       end)

let userintpage_fromdb boxdescrlist sp p = 
  page (List.map (fun a -> (RegisterUserIntBox.unfold a) sp p) boxdescrlist)

let fold_userintpage =
  RegisterUserIntPage.register ~name:"userintpage" 
    ~constructor:userintpage_fromdb

let fold_string_message_box = 
  RegisterUserIntBox.register ~name:"string_message_userintbox" 
    ~constructor:(fun () u i -> string_message_box i u)

let fold_box_userintbox =
    RegisterUserIntBox.register 
      ~name:"box_userintbox" 
      ~constructor:(fun x u i -> RegisterBox.unfold x)

let fold_boxes_userintbox =
    RegisterUserIntBox.register 
      ~name:"boxes_userintbox" 
      ~constructor:(fun bdl u i -> div (List.map RegisterBox.unfold bdl))

*)

(*

class register_box = 
  let register,unfold =
    let module RegisterBox = 
      MakeRegister(struct 
		     type t = Xhtmlpp.xhtmlcont
		     let default_handler = box_exn_handler
		   end)
    in RegisterBox.register, RegisterBox.unfold
  in object (moi)
      method private register = register
      val fold_title_box = moi#register ~name:"title_box" ~constructor:title_box
      val fold_text_box  = moi#register ~name:"text_box" ~constructor:text_box
      method fold_title_box = fold_title_box
      method fold_text_box  = fold_text_box
      method unfold = unfold
    end

   
class virtual ['a,'tfolded,'t] foldclass = object
  method virtual register : name:string -> constructor:('a -> 't) -> ('a -> 'tfolded)
  method virtual unfold : 'tfolded -> 't
end

module MakeRegisterBox =
  functor (A : sig 
	     type t 
	     val default_handler : exn -> t 
	     class folds : foldclass
	   end) -> struct
      module R = MakeRegister(A)
      class registerclass = object
	inherit A.folds
	method private register = R.register
	method unfold = R.unfold
      end
    end
    *)

(*
class virtual ['tfolded,'t] virtual_register_box = object
  method virtual unfold : 'tfolded -> 't
end

class register_box = 
object (moi)
  inherit [RegisterBox.tfolded, RegisterBox.t] virtual_register_box
  val fold_title_box = 
    RegisterBox.register ~name:"title_box" ~constructor:title_box
  val fold_text_box  = 
    RegisterBox.register ~name:"text_box" ~constructor:text_box
  method fold_title_box = fold_title_box
  method fold_text_box  = fold_text_box
  method unfold = RegisterBox.unfold
end



class virtual ['tfolded,'t] page 
  (register_box : ('tfolded,'t) virtual_register_box) 
  (boxesdescrlist : 'tfolded list) = 
object
end

class message_page register_box boxesdescrlist =
object
  inherit [RegisterUserIntBox.tfolded, RegisterUserIntBox.t] page 
    register_box boxesdescrlist
  method print (u : Rights.user) (i : int) : Xhtmlpp.xhtml =
    page (List.map 
	    (fun a -> (register_box#unfold a) u i)
	    boxesdescrlist)
end






class message_page =
object (moi)
  method print_title s =
  method print_message u i =
  method print u i =
    page [(moi#print_title "hello");(moi#print_message u i)]
end

class rb = object
  method print_title s =
  method print_message u i =
end

class message_page =
object (moi)
  inherit rb
  method print u i =
    page [(moi#print_title "hello");(moi#print_message u i)]
end

ou :
class message_page rb =
object
  method print u i =
    page [(rb#print_title "hello");(rb#print_message u i)]
end

******************************************************************

  *)


