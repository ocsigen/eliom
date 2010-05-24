(* $Id: xML.ml,v 1.14 2004/12/13 14:57:45 ohl Exp $

   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   XHTML is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   XHTML is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

open Js
open JSOO


type aname = string
type attrib = string * JSOO.obj
type attribs = attrib list

let int_attrib name value = (name, JSOO.int value)
let string_attrib name value = (name, JSOO.string value)
let space_sep_attrib name values = (name, JSOO.string (List.fold_left (fun r s -> r ^ " " ^ s) "" values))
let comma_sep_attrib name values = (name, JSOO.string  (List.fold_left (fun r s -> r ^ "," ^ s) "" values))
let event_attrib name value = (name, JSOO.wrap_event (fun _ -> ignore (value ())))

let attrib_name = fst

type ename = string

type elt = Js.Node.t

(*
type node_type =
  | Element_node
  | Attribute_node
  | Text_node
  | Cdata_section_node
  | Entity_reference_node
  | Entity_node
  | Processing_instruction_node
  | Comment_node
  | Document_node
  | Document_type_node
  | Document_fragment_node
  | Notation_node

let node_type n =
  match get_attribute n "nodeType" >>> as_int with
  | 1 -> Element_node
  | 2 -> Attribute_node
  | 3 -> Text_node
  | 4 -> Cdata_section_node
  | 5 -> Entity_reference_node
  | 6 -> Entity_node
  | 7 -> Processing_instruction_node
  | 8 -> Comment_node
  | 9 -> Document_node
  | 10 -> Document_type_node
  | 11 -> Document_fragment_node
  | 12 -> Notation_node
  | _ -> assert false    
*)

let empty () = inject Nil

let comment c = Node.text c

let pcdata d = Node.text d
let encodedpcdata d = Node.text d
let entity e = assert false

let leaf ?a name =
  let n = Node.element name in
    (match a with
       | Some a -> List.iter (fun (p, v) -> n >>> set p v >>> ignore) a
       | None -> ()) ;
    n

let node ?a name children =
  let n = Node.element name in
    (match a with
       | Some a -> List.iter (fun (p, v) -> n >>> set p v >>> ignore) a
       | None -> ()) ;
    List.iter (Node.append n) children ;
    n


let register_event elt name f v =
  Lwt_obrowser.register_event elt "onclick" f v
