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

open Ocsidata
open Ocsipages

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
    type content = Xhtmltypes.body_content XHTML.M.elt
    type 'a t = 'a
    type box = [`Box of content t tfolded]
    type boxes = box
    let name = "boxes"
    let tag x = `Box x
    let untag (`Box x) = x
    let default_handler = box_exn_handler
      let make_boxofboxes ~filter l = 
        List.map (fun b -> (filter b)) l
    type container_param = 
        Xhtmltypes.div_attrib XHTML.M.attrib list option
    let container f ~box_param:(a,l) = 
      boxes_container ?a:a
        (f ~user:Rights.anonymoususer ~resource:Rights.anyresource l)
  end)

let fold_boxes = 
  RegisterBoxes.register_unfolds
    ~box_constructor:RegisterBoxes.unfold

(* Then register all constructors in the right register *)
let fold_title_box = 
  RegisterBoxes.register 
    ~name:"title_box" 
    ~constructor:(fun ~box_param -> title_box box_param)

let fold_text_box = 
  RegisterBoxes.register 
    ~name:"text_box" ~constructor:(fun ~box_param -> text_box box_param)




