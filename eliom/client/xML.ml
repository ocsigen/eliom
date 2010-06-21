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

module Html = Dom_html

type aname = string
type attrib = string * Js.Unsafe.any
type attribs = attrib list
type event = unit -> unit

let int_attrib name value = (name, Js.Unsafe.inject value)
let string_attrib name value = (name, Js.Unsafe.inject (Js.string value))
let space_sep_attrib name values = 
  (name, Js.Unsafe.inject (Js.string
     (match values with
        | [] -> ""
        | a::l -> List.fold_left (fun r s -> r ^ " " ^ s) a l)))
let comma_sep_attrib name values =
  (name, Js.Unsafe.inject (Js.string
     (match values with
        | [] -> ""
        | a::l -> List.fold_left (fun r s -> r ^ "," ^ s) a l)))
(*FIX: wrong type*)
let event_attrib name value = (name, Js.Unsafe.inject (Dom_html.handler (fun _ -> ignore (value ()); Js._false)))

let attrib_name = fst

type ename = string

type elt = Dom.node Js.t

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

let empty () = assert false  (*FIX: what is this supposed to be?*)

let comment c = assert false (*FIX*)

(*FIX: what should we quote?*)
let pcdata d = (Dom_html.document##createTextNode (Js.string d) :> elt)
let encodedpcdata d = (Dom_html.document##createTextNode (Js.string d) :> elt)
let entity e = assert false (*FIX: should implement*)

let cdata s = (*FIX: what should we quote?*)
  encodedpcdata s
    
let cdata_script s =(*FIX: what should we quote?*)
  encodedpcdata s
    
let cdata_style s =(*FIX: what should we quote?*)
  encodedpcdata s


(*FIX: cannot set input/name with IE  *)
let node ?a name children =
  let n = Dom_html.document##createElement (Js.string name) in
  begin match a with
  | Some a -> List.iter (fun (p, v) -> Js.Unsafe.set n p v) a
  | None -> ()
  end;
  List.iter (fun c -> Dom.appendChild n c) children;
  (n :> elt)

(*FIX: cannot set input/name with IE  *)
let leaf ?a name = node ?a name []

let register_event elt name f v =
  elt##onclick <- Dom_html.handler (fun _ -> ignore (f v); Js._false)
