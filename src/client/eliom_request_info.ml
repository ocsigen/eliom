(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_sessions.ml
 * Copyright (C) 2009 Vincent Balat
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

(* TODO: add missing functions to get 
   (almost) the same interface as server side *)

(* - Part of sp is reconstructed client side
   - Another part is sent as application parameter (sitedata)
   - Antother part is sent with each request
*)

let (>>>) x f = f x

include Eliom_client_types

let get_sess_info = ref (fun () ->
  failwith "Eliom_request_info.get_sess_info called before initialization")

let set_session_info si = get_sess_info := fun () -> si

let full_path_string_ = Url.Current.path_string

let full_uri = Url.Current.as_string

let get_original_full_path_string () = full_path_string_
let get_original_full_path_string_sp = get_original_full_path_string


let get_other_get_params () =
  (!get_sess_info ()).Eliom_common.si_other_get_params
let get_nl_get_params () = (!get_sess_info ()).Eliom_common.si_nl_get_params
let get_nl_get_params_sp = get_nl_get_params

let get_persistent_nl_get_params () =
  Lazy.force (!get_sess_info ()).Eliom_common.si_persistent_nl_get_params
let get_persistent_nl_get_params_sp = get_persistent_nl_get_params

let get_nl_post_params () =
  (!get_sess_info ()).Eliom_common.si_nl_post_params

let get_si () = !get_sess_info ()

let get_site_dir () = Eliom_process.sitedata.site_dir
let get_site_dir_string () =
  Eliom_process.sitedata.site_dir_string

let get_sp_appl_name = Eliom_process.get_application_name
