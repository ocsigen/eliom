open Ocsidata
(******************************************************************)
(* The boxes that can appear in pages and pages *)

  
(******************************************************************)
(** Some usefull boxes: *)
(** Title *)
let title_box titre = << <h1>$str:titre$</h1> >>

(** A simple box that prints something *)
let text_box msg = << <div>$str:msg$</div> >>

(** A box that prints an error message *)
let error_box s = << <p><b>$str:s$</b></p> >>

(** A simple box that contains other boxes *)
let div l = << <div>$list:l$</div> >>

(** A simple box that prints a message of the db *)
let string_message_box key user resource =
  let msg = StringMessage.dbget user resource key
  in << <div>$str:msg$</div> >>

let box_exn_handler ex = match ex with
    Rights.Read_Forbidden_for_user -> error_box "You cannot read this data"
  | Rights.Write_Forbidden_for_user -> 
      error_box "You don't have write access to this data"
  | Rights.Read_Forbidden_for_resource -> 
      error_box "Data not readable in this context"
  | Rights.Write_Forbidden_for_resource -> 
      error_box "Data not writable in this context"
  | Rights.Permission_Denied -> error_box "Permission denied"
  | Rights.Wrong_Password -> error_box "Wrong password"
  | Ocsidata.Box_not_available s -> error_box ("Box not available here : "^s)
  | Not_found -> error_box "not found"
  | Ocsidata.Dyn.Dyn_typing_error_while_unfolding (_,_) -> 
      error_box "Wrong data (index error?)"
  | Ocsidata.Unfolds_not_registered s -> 
      error_box ("Internal error : Unfolds not registered for "^s)
  | _ -> error_box "unknown error while creating box"


(** Container *)
let boxes_container ?classe ?id l :> [> Xhtmlpp.xhbodycont ] =
  let attrid = (match id with None -> [] | Some c -> [`Id,c]) in
  let attrs = 
    (match classe with None -> attrid | Some c -> (`Class,c)::attrid) in
  let l = (l :> Xhtmlpp.xhdivcont list) in
  << <div $list:attrs$>$list:l$</div> >>



(******************************************************************)
(* Now the pages *)

(** The class for description of web pages.
    We need two constructors, one to create pages from database,
    the other one manually.
 *)

let page h ?(js=[]) ?(css=[]) (bl : [> Xhtmlpp.xhbodycont] list) = 
  let rec make_hl make_link l = function
      [] -> l
    | (filedir, filename)::ll -> 
	(make_link h.Ocsigen.current_url filedir filename)
	::(make_hl make_link l ll)
  in 
  let hl = make_hl Ocsigen.css_link (make_hl Ocsigen.js_link [] js) css in
  << <html> 
      <head> $list:hl$ </head> 
      <body> $list:bl$ </body> 
     </html> >>

let empty_page h = page h <:xmllist< <p><b>empty page</b></p> >>

let page_exn_handler h ex = page h [box_exn_handler ex]



(*
(******************************************************************)
(** It can be usefull to have late binding. To do that, we organize
    boxes and pages in classes.
    pages can take such a register as a parameter
*)
class boxes_class = object
  method print_title s : Xhtmlpp.xhtmlcont = title_box s
  method print_text s : Xhtmlpp.xhtmlcont = text_box s
  method print_error s : Xhtmlpp.xhtmlcont = error_box s
end

(* Put in the class message_boxes all the boxes you want to be able
   to print from a "message page".
*)
class message_boxes_class = object
  inherit boxes_class
  method print_string_message u i : Xhtmlpp.xhtmlcont = string_message_box i u
end


*)

