(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

open Eliom_pervasives

(* Some types are different on client side: *)

type server_params = unit

let sp = ()

type sitedata = (* sent while starting the program *)
  {site_dir: Url.path;
   site_dir_string: string;
  }

(* Abstract type for the polymorphic contents in the table of page data. *)
type poly

type 'a data_key = int64 * int

let to_data_key_ v = v
let of_data_key_ v = v

type onload_form_creators_info =
  | OFA of XML.elt * string * (bool * Url.path) option
  | OFForm_get of
      XML.elt * string * (bool * Url.path) option
  | OFForm_post of
      XML.elt * string * (bool * Url.path) option

type separator = Space | Comma

type attrib =
  | AFloat of string * float
  | AInt of string * int
  | AStr of string * string
  | AStrL of separator * string * string list

type elt_content =
  | Empty
  | Comment of string
  | EncodedPCDATA of string
  | PCDATA of string
  | Entity of string
  | Leaf of string * attrib list
  | Node of string * attrib list * elt list
  | Ref of int

and elt = ( elt_content * int option )

(* The data that comes with each page: *)
type eliom_data_type =
    ((* The ref tree, to relink the DOM *)
      (XML.ref_tree, (int * XML.ref_tree) list) leftright *
	(* node sent that are not in the original page *)
	elt list *
        (* Table of page data *)
        (poly * ((int64 * int) * poly list)) *
        (* Tab cookies to set or unset *)
        Ocsigen_cookies.cookieset *
        onload_form_creators_info list data_key (* info for creating xhr forms *) *
        string list (* on load scripts *) *
        string list (* on change scripts *) *
        Eliom_common.sess_info
(*VVV si contains too much information ... 
  We probably don't need to send cookies.
*)
    )


(*SGO* Server generated onclicks/onsubmits
(* For client side program, we sometimes simulate links and forms
   with client side functions.
   Here are there identifiers: *)
let a_closure_id = 0x0
let a_closure_id_string = Printf.sprintf "0x%02X" a_closure_id
let get_closure_id = 0x3
let get_closure_id_string = Printf.sprintf "0x%02X" get_closure_id
let post_closure_id = 0x4
let post_closure_id_string = Printf.sprintf "0x%02X" post_closure_id


let eliom_temporary_form_node_name = "eliom__temp_form_node_name"
*)

(*POSTtabcookies* forms with tab cookies in POST params:

let add_tab_cookies_to_get_form_id = 0x1
let add_tab_cookies_to_get_form_id_string =
  Printf.sprintf "0x%02X" add_tab_cookies_to_get_form_id
let add_tab_cookies_to_post_form_id = 0x2
let add_tab_cookies_to_post_form_id_string =
  Printf.sprintf "0x%02X" add_tab_cookies_to_post_form_id

*)

