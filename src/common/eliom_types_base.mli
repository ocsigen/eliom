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

type sitedata = {
    site_dir: Url.path;
    site_dir_string: string;
  }

type server_params

val sp : server_params

type 'a data_key

val to_data_key_ : (int64 * int) -> 'a data_key
val of_data_key_ : 'a data_key -> (int64 * int)


(**/**)
type poly

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

type eliom_js_data =
    { (* Sparse tree for HTML body and header, to relink the DOM.
	 Left  -> first page;
	 Right -> change page. *)
      ejs_body: (XML.ref_tree, (int * XML.ref_tree) list) leftright;
      ejs_headers: ((int * XML.ref_tree) list, int list) leftright;
      (* Wrapped value for JS. (see Eliom_wrap) *)
      ejs_page_data: poly * ((int64 * int) * poly list);
      (* XML nodes not included in the page but referenced by JS. *)
      ejs_node_list: elt list;
      (* ... *)
      ejs_cookies: Ocsigen_cookies.cookieset;
      (* Info for creating xhr (list of link and forms) *)
      ejs_xhr: onload_form_creators_info list data_key;
      (* Raw javascript *)
      ejs_onload: string list ;
      ejs_onunload: string list;
      (* ... *)
      ejs_sess_info: Eliom_common.sess_info;
    }

(* the data sent on channels *)
type 'a eliom_comet_data_type = (poly * 'a) * (elt list)

(*SGO* Server generated onclicks/onsubmits
val a_closure_id : int
val a_closure_id_string : string
val get_closure_id : int
val get_closure_id_string : string
val post_closure_id : int
val post_closure_id_string : string

val eliom_temporary_form_node_name : string

*)

(*POSTtabcookies* forms with tab cookies in POST params:
val add_tab_cookies_to_get_form_id : int
val add_tab_cookies_to_get_form_id_string : string
val add_tab_cookies_to_post_form_id : int
val add_tab_cookies_to_post_form_id_string : string
*)


