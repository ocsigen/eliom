(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open XHTML.M
open Ocsigen
open Ocsigen.Xhtml
open Ocsisav
open Ocsidata

(** Authentification *)
let create_login_form = 
  (fun (login, password) ->
       <:xmllist< 
         <p>
         Login: $string_input ~a:[a_size 8] login$ <br/>
         Password: $string_input ~a:[a_size 8] password$ <br/>
         $submit_input "Entrer"$
         </p>
       >>)

let login_box_action h action = 
  action_form ~a:[a_id "loginbox";a_class ["userbox"]]
    action h create_login_form

let login_box h serv = post_form serv h create_login_form

let deconnect_box deconnect_action h s = action_a deconnect_action h s


(** User information *)
let connected_box deconnect_action h user =
  let login,name,_ = Rights.get_user_info user in
  let deconnect = deconnect_box deconnect_action h <:xmllist< déconnexion >> in
    << <div id="loggedbox" class="userbox"> 
         <p>Vous êtes connecté comme utilisateur $str:login$ </p>
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
let form = register_new_service (Url ["form"]) _noparam 
  (let f = get_form plop_params create_form in
  << <html> $f$ </html> >>)
*)
