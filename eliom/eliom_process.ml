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


include Eliom_process_cli

(*****************************************************************************)
let appl_name_key = Polytables.make_key ()
let content_only_key = Polytables.make_key ()

(*VVV better put this somewhere else than in rc? directly in sp? *)
let get_application_name_cookie ~sp =
  let esp = Eliom_sessions.esp_of_sp sp in
  let rc = esp.Eliom_common.sp_request.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache in
  try
    let cookie_table =
      Polytables.get ~table:rc ~key:Eliom_parameters.request_tab_cookies_key
    in
    Some (Ocsigen_lib.String_Table.find
            Eliom_common.appl_name_cookie_name cookie_table)
  with Not_found -> None

let get_application_name ~sp =
  let esp = Eliom_sessions.esp_of_sp sp in
  let rc = esp.Eliom_common.sp_request.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache in
  try
    Polytables.get ~table:rc ~key:appl_name_key
  with Not_found -> None
    (* If it is a Eliom_app service, 
       the value has already been set in pre_service. *)

let get_content_only ~sp =
  let esp = Eliom_sessions.esp_of_sp sp in
  let rc = esp.Eliom_common.sp_request.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache in
  try 
    Polytables.get ~table:rc ~key:content_only_key
  with Not_found ->
    false
