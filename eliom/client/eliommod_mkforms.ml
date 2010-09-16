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


let tab_cookie_class = "__eliom_tab_cookies"

let remove_tab_cookie_fields node =
  let children = node##childNodes in
  for i = children##length - 1 downto 0 do
    let child = children##item (i) in
    let classes = Ocsigen_lib.split
                    ~multisep:true ' ' (Js.to_string child##className)
    in
    if List.mem tab_cookie_class classes
    then node##removeChild (child)
  done

let add_tab_cookie_fields l node =
  if l = []
  then ()
  else
    let my_div = 
      XHTML5.M.div ~a:[XHTML5.M.a_class [tab_cookie_class;
                                       Eliom_common.nodisplay_class_name]]
        (List.map (fun (n, v) ->
(*VVV Warning: This is not xhtml5! *)
          XHTML5.M.input ~a:[XHTML5.M.a_input_type `Hidden;
                             XHTML5.M.a_name n;
                             XHTML5.M.a_value v] ())
           l)
    in
    node##appendChild (my_div)



let add_tab_cookies_to_form' l node =
  remove_tab_cookie_fields node;
  add_tab_cookie_fields l node;
  Lwt_js.sleep 0.05 >|=
  Eliom_client_comet.Engine.restart

let add_tab_cookies_to_post_form' node =
  let action = node##action in
  let (https, path) = Eliom_request.get_cookie_info_for_uri_js action in
  let cookies = Eliommod_client_cookies.get_cookies_to_send https path in
  let l = [(Eliom_common.tab_cookies_param_name,
            Ocsigen_lib.encode_form_value cookies)]
  in
  add_tab_cookies_to_form' l node

let add_tab_cookies_to_post_form node () =
  let node = Js.Unsafe.coerce (XHTML5.M.toelt node) in
  add_tab_cookies_to_post_form' node


let add_tab_cookies_to_get_form' node =
  (* we transform the form into POST form:
     - to avoid long URLs (not supported by (old?) IE)
     - ...
  *)
  node##_method <- "post";
  let action = node##action in
  let (https, path) = Eliom_request.get_cookie_info_for_uri_js action in
  let cookies = Eliommod_client_cookies.get_cookies_to_send https path in
  let l = [(Eliom_common.get_request_post_param_name, "1");
           (Eliom_common.tab_cookies_param_name,
            Ocsigen_lib.encode_form_value cookies)]
  in
  add_tab_cookies_to_form' l node

let add_tab_cookies_to_get_form node () =
  let node = Js.Unsafe.coerce (XHTML5.M.toelt node) in
  add_tab_cookies_to_get_form' node

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
      ignore (add_tab_cookies_to_get_form (XHTML5.M.tot node) ());
      Js._true)

let _ =
  Eliommod_cli.register_closure
    Eliom_client_types.add_tab_cookies_to_post_form_id
    (fun node ->
      let node = (Eliommod_cli.unwrap_node node :> Dom.node Js.t) in
      ignore (add_tab_cookies_to_post_form (XHTML5.M.tot node) ());
      Js._true)

