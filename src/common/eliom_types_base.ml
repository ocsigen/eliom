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

open Eliom_lib
open Eliom_content_core

(* Some types are different on client side: *)

type server_params = unit

let sp = ()

type sitedata = (* sent while starting the program *)
  {site_dir: Url.path;
   site_dir_string: string;
  }

type eliom_js_page_data = {
  (* Event handlers *)
  ejs_event_handler_table: Xml.event_handler_table;
  ejs_initializations: (int64 * int * poly) list;
  ejs_global_injections: (string * poly) list;
  ejs_request_injections: (string * poly) list;
  ejs_onload: Dom_html.event Xml.caml_event_handler list;
  ejs_onunload: Dom_html.event Xml.caml_event_handler list;
  (* Session info *)
  ejs_sess_info: Eliom_common.sess_info;
}

type 'a eliom_caml_service_data = {
  ecs_onload: Dom_html.event Xml.caml_event_handler list;
  ecs_data: 'a;
}

(* the data sent on channels *)
type 'a eliom_comet_data_type = 'a Eliom_wrap.wrapped_value

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
