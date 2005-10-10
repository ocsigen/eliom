open Krokodata
open Krokopages

(******************************************************************)
(* Tools to save boxes and pages in the database *)

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


(* Then register all constructors in the right register *)
let fold_title_box = 
  RegisterBoxes.register 
    ~name:"title_box" 
    ~constructor:(fun ~box_param -> title_box box_param)

let fold_text_box = 
  RegisterBoxes.register 
    ~name:"text_box" ~constructor:(fun ~box_param -> text_box box_param)




