(* Copyright Vincent Balat 2005 *)

open Kroko
open Krokosavable
open Krokodata

(** Authentification *)
let create_login_form = 
  (fun login password ->
     let login = string_box login in
     let password = string_box password in
     let b = button "Entrer" in
       <:xmllist< 
         Login: $login$ <br/>
         Password: $password$ <br/>
	 $b$
       >>)

let login_box_action h actionurl = action_form h actionurl create_login_form

let login_box h url = form_post h.current_url url create_login_form

let deconnect_action = 
  register_new_actionurl _unit close_session

let deconnect_box h s = action_link s h deconnect_action


(** User information *)
let connected_box h user =
  let login,name,_ = Rights.get_user_info user in
  let deconnect = deconnect_box h "déconnexion" in
    << <div> 
         Vous êtes connecté comme utilisateur $str:login$ <br/>
         $deconnect$
       </div>
    >>
  


(* 
   If we want to make the url a box parameter saved in the db, it is not
   easy, because we need to make a table of url in which we register
   all the url we want to be able to save. For ex:
let fold_login_box = 
  RegisterHPBoxes.register ~name:"login_box" 
    ~constructor:(fun ~box_param:urltag h -> login_box h (unfold_url urltag))

   But anyway it is not easy to propose to the user (in the web interface to
   create pages) a list of url to put as parameters of boxes.
   
*)

(*
let urltag = fold_url "mapage" url
*)

(*
let form = register_new_url (Url ["form"]) _noparam 
  (let f = form_get plop_params create_form in
  << <html> $f$ </html> >>)
*)
