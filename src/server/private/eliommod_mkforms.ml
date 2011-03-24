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

(* This module is different on client and server side *)

let (make_a_with_onclick :
       (?a:'a -> ?onclick:XML.event -> ?href:string -> 'c -> 'd) ->
     (?keep_default:bool -> 'd -> string -> ('e -> unit Lwt.t) -> unit -> 'f) ->
     ?a:'a ->
     ?cookies_info:'ci ->
     Eliom_services.send_appl_content ->
     string ->
     'r) =
  fun
    make_a
    register_event
    ?a
    ?cookies_info
    send_appl_content
    href
    content
  ->
    let node =
      make_a
        ?a
        ~href
        ?onclick:None
(*SGO* Server generated onclicks/onsubmits
        ?onclick:
        (Some ("return caml_run_from_table ("^
                  Eliom_client_types.a_closure_id_string^", \'"^
                  (Eliom_client_types.jsmarshal
                     ((Eliommod_cli.wrap cookies_info),
                      (Eliommod_cli.wrap href)))
               ^ "\');")
        )
*)
        content
    in
    Eliom_services.add_onload_form_creator
      (send_appl_content, (Eliom_types.OFA
                             (XHTML5.M.toelt node, href, cookies_info)));
    node

let make_get_form_with_onsubmit
    make_get_form
    register_event
    ?a
    ?cookies_info
    send_appl_content
    uri
    field
    fields
    =
  let node =
    make_get_form
      ?a
      ~action:uri
      ?onsubmit:None
(*SGO* Server generated onclicks/onsubmits
(* As we cannot wrap the node while defining it,
   we use "this". But "this" is not the form any more while executing
   the function from the table ...
   We save it temporarily in a global variable ...

   The alternative would be to do an onload {{ onsubmit form >>> ... }}
   after creating the form,
   but the code is difficult to write without syntax extension ...
*)
    ?onsubmit:(Some (Eliom_client_types.eliom_temporary_form_node_name^
                       " = this; return caml_run_from_table ("^
                       Eliom_client_types.get_closure_id_string^", \'"^
                       (Eliom_client_types.jsmarshal
                          ((Eliommod_cli.wrap cookies_info),
                           (Eliommod_cli.wrap uri)))
                     ^ "\');"))
*)
      field
      fields
  in
  Eliom_services.add_onload_form_creator
    (send_appl_content, (Eliom_client_types.OFForm_get
                           (XHTML5.M.toelt node, uri, cookies_info)));
  node


let make_post_form_with_onsubmit
    make_post_form
    register_event
    ?a
    ?cookies_info
    send_appl_content
    uri
    field
    fields
    =
  let node =
    make_post_form
      ?a
      ~action:uri
      ?onsubmit:None
(*SGO* Server generated onclicks/onsubmits
    ?onsubmit:(Some (Eliom_client_types.eliom_temporary_form_node_name^
                     " = this; return caml_run_from_table ("^
                       Eliom_client_types.post_closure_id_string^", \'"^
                       (Eliom_client_types.jsmarshal
                          ((Eliommod_cli.wrap cookies_info),
                           (Eliommod_cli.wrap uri)))
                     ^ "\');"))
*)
      field
      fields
  in
  Eliom_services.add_onload_form_creator
    (send_appl_content, (Eliom_client_types.OFForm_post
                           (XHTML5.M.toelt node, uri, cookies_info)));
  node



(*POSTtabcookies* forms with tab cookies in POST params:

let make_add_tab_cookies_to_form id form_ref =
  let reqnum = Eliom_request_info.get_request_id () in
  "caml_run_from_table (" ^
    id^", \'" ^
    (Eliom_types.jsmarshal (reqnum, form_ref)) ^
    "\')"

let make_get_form_with_post_tab_cookies
    make_get_form register_event _ id ?a ~action i1 i =
  let onsubmit = Some (make_add_tab_cookies_to_form id (XML.next_ref ())) in
  make_get_form ?a ~action ?onsubmit i1 i


let make_post_form_with_post_tab_cookies
    make_post_form register_event _ id ?a ~action i1 i =
  let onsubmit = Some (make_add_tab_cookies_to_form id (XML.next_ref ())) in
  make_post_form ?a ~action ?onsubmit ?id:None ?inline:None i1 i


let add_tab_cookies_to_get_form node = Lwt.return
  (* implemented only client side *)
  
let add_tab_cookies_to_post_form node = Lwt.return
  (* implemented only client side *)

let add_tab_cookies_to_get_form5 node = Lwt.return
  (* implemented only client side *)
  
let add_tab_cookies_to_post_form5 node = Lwt.return
  (* implemented only client side *)

*)

