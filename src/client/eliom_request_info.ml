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

open Eliom_pervasives

let (>>>) x f = f x

include Eliom_types

let get_sess_info = ref (fun () ->
  failwith "Eliom_request_info.get_sess_info called before initialization")

let set_session_info si = get_sess_info := fun () -> si

let remove_first_slash path =
  match path with
  | ""::l -> l
  | l -> l

let path_re =
  jsnew Js.regExp (Js.bytestring "^/?([^\\?]*)(\\?.*)?$")

let current_path = ref (remove_first_slash Url.Current.path)
let set_current_path path =
  let path =
    Js.Opt.case (path_re##exec (Js.string path))
      (fun () -> [])
      (fun handle ->
	let res = Js.match_result handle in
	let path =
	  Js.to_bytestring
            (Js.Optdef.get
               (Js.array_get res 1)
               (fun () -> Js.bytestring ""))
	in
	Url.split_path path)
  in
  current_path := path

let get_original_full_path_string () =
  if Eliom_process.history_api then
    match Url.Current.get () with
    | Some (Url.Http url) | Some (Url.Https url) ->
      String.concat "/" url.Url.hu_path
    | _ -> assert false
  else
    String.concat "/" !current_path
let get_original_full_path_string_sp = get_original_full_path_string

let get_original_full_path_sp sp =
  if Eliom_process.history_api then
    match Url.Current.get () with
    | Some (Url.Http url) | Some (Url.Https url) -> url.Url.hu_path
    | _ -> assert false
  else
    !current_path

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

let ssl_ = match Url.Current.get () with
  | Some (Url.Https _) -> true
  | Some (Url.Http _) | Some (Url.File _) | None -> false

let get_csp_ssl () = ssl_
let get_csp_ssl_sp = get_csp_ssl

let host_ = Url.Current.host

let get_csp_hostname () = host_
let get_csp_hostname_sp = get_csp_hostname

let port_ = match Url.Current.port with
  | Some p -> p
  | None -> if ssl_ then 443 else 80

let get_csp_server_port () = port_
let get_csp_server_port_sp = get_csp_server_port

let get_csp_original_full_path () =
  if Eliom_process.history_api then
    match Url.Current.get () with
    | Some (Url.Http url) | Some (Url.Https url) -> url.Url.hu_path
    | _ -> assert false
  else
    remove_first_slash Url.Current.path

let get_csp_original_full_path_sp = get_csp_original_full_path

let get_request_url () = unmarshal_js_var "eliom_request_url"
let get_request_cookies () = unmarshal_js_var "eliom_request_cookies"
let get_request_data () = Eliom_unwrap.unwrap (unmarshal_js_var "eliom_request_data")
