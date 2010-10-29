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

(* This module is different on client and server side *)

let (make_a_with_onclick :
       (?a:'a -> ?onclick:string -> 'c -> 'd) ->
      ('d -> string -> ('e -> unit Lwt.t) -> unit -> 'f) ->
      ?absolute:bool ->
      ?absolute_path:bool ->
      ?https:bool ->
      ?a:'a ->
      service:('g, unit, [< Eliom_services.get_service_kind ],
               [< Eliom_services.suff ], 'h, 'i,
               [< Eliom_services.registrable ], 'j)
        Eliom_services.service ->
      sp:Eliom_request_info.server_params ->
      ?hostname:string ->
      ?port:int ->
      ?fragment:string ->
      ?keep_nl_params:[ `All | `None | `Persistent ] ->
      ?nl_params:Eliom_parameters.nl_params_set ->
      'c -> 'g -> 'd) =
  fun
    make_a
    register_event
    ?absolute
    ?absolute_path
    ?https
    ?a
    ~service
    ~sp
    ?hostname
    ?port
    ?fragment
    ?keep_nl_params
    ?nl_params
    content
    getparams ->
  make_a
    ?a
    ?onclick:
    (Some ("caml_run_from_table ("^
              Eliom_client_types.a_closure_id_string^", \'"^
              (Eliom_client_types.jsmarshal
                 ((Eliommod_cli.wrap ~sp absolute),
                  (Eliommod_cli.wrap ~sp absolute_path),
                  (Eliommod_cli.wrap ~sp https),
                  (Eliommod_cli.wrap ~sp service),
(*                  (Eliommod_cli.wrap_sp sp),*)
                  (),
                  (Eliommod_cli.wrap ~sp hostname),
                  (Eliommod_cli.wrap ~sp port),
                  (Eliommod_cli.wrap ~sp fragment),
                  (Eliommod_cli.wrap ~sp keep_nl_params),
                  (Eliommod_cli.wrap ~sp nl_params),
                  (Eliommod_cli.wrap ~sp getparams))
               ^ "\')")
     ))
    content


let make_add_tab_cookies_to_form id ~sp form_ref =
  let reqnum = Eliom_request_info.get_request_id ~sp in
  "caml_run_from_table (" ^
    id^", \'" ^
    (Eliom_client_types.jsmarshal (reqnum, form_ref)) ^
    "\')"

let make_get_form_with_onsubmit
    make_get_form register_event _ id ~sp ?a ~action i1 i =
  let onsubmit = Some (make_add_tab_cookies_to_form id ~sp (XML.next_ref ()))
  in
  make_get_form ?a ~action ?onsubmit i1 i


let make_post_form_with_onsubmit
    make_post_form register_event _ id ~sp ?a ~action i1 i =
  let onsubmit = Some (make_add_tab_cookies_to_form id ~sp (XML.next_ref ()))
  in
  make_post_form ?a ~action ?onsubmit ?id:None ?inline:None i1 i


let add_tab_cookies_to_get_form node = Lwt.return
  (* implemented only client side *)
  
let add_tab_cookies_to_post_form node = Lwt.return
  (* implemented only client side *)

let add_tab_cookies_to_get_form5 node = Lwt.return
  (* implemented only client side *)
  
let add_tab_cookies_to_post_form5 node = Lwt.return
  (* implemented only client side *)
