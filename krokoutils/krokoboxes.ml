(* Copyright Vincent Balat 2005 *)

open Omlet
open Krokosavable

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

let login_box h url = form_post h.current_url url create_login_form

(* I think the right way is to register the login box for each page,
   with a fix url. (There is a register for each kind of page).
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
