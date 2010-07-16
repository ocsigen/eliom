(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)

let make_a_with_onclick = Eliom_client.make_a_with_onclick

let add_tab_cookies_to_post_form' node =
  let action = Js.to_string node##action in
  let action = Eliom_client.add_cookie_nlp_to_uri action in
  node##action <- Js.string action;
  Lwt_js.sleep 0.05 >|=
  Eliom_client_comet.Engine.restart

let add_tab_cookies_to_post_form node () =
  Firebug.console##log (Js.string "add");
  let node = Js.Unsafe.coerce (XHTML.M.toelt node) in
  add_tab_cookies_to_post_form' node

let add_tab_cookies_to_post_form5 node () =
  let node = Js.Unsafe.coerce (XHTML5.M.toelt node) in
  add_tab_cookies_to_post_form' node

let add_tab_cookies_to_get_form node () =
  failwith "unimpl"

let add_tab_cookies_to_get_form5 =
  add_tab_cookies_to_get_form

let make_get_form_with_onsubmit
    make_get_form register_event add_tab_cookies_to_get_form _
    ~(sp:Eliom_sessions.server_params) ?a ~action i1 i =
  let node =
    make_get_form ?a ~action ?onsubmit:(None : XML.event option) i1 i in
  register_event node "onsubmit" (add_tab_cookies_to_get_form node);
  node

let make_post_form_with_onsubmit
    make_post_form register_event add_tab_cookies_to_post_form _
    ~sp ?a ~action i1 i =
  let node = make_post_form ?a ~action ?onsubmit:None
    ?id:None ?inline:None i1 i
  in
  register_event node "onsubmit" (add_tab_cookies_to_post_form node);
  node



let _ =
  Eliommod_cli.register_closure
    Eliom_client_types.add_tab_cookies_to_get_form_id
    (fun node ->
         let node = (Eliommod_cli.unwrap_node node :> Dom.node Js.t) in
         add_tab_cookies_to_get_form (XHTML.M.tot node) ();
         Js._true)

let _ =
  Eliommod_cli.register_closure
    Eliom_client_types.add_tab_cookies_to_post_form_id
    (fun node ->
         let node = (Eliommod_cli.unwrap_node node :> Dom.node Js.t) in
         add_tab_cookies_to_post_form (XHTML.M.tot node) ();
         Js._true)
