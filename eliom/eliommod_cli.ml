(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_client.ml
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


let fresh_id = 
  let c = ref 0 in
  fun () -> c := !c+1; "id"^string_of_int !c

let client_sitedata s =
  {Eliom_client_types.site_dir = s.Eliom_common.site_dir;
   Eliom_client_types.site_dir_string = s.Eliom_common.site_dir_string;
  }

(*
let client_config_info s = {
  Eliom_client_types.default_hostname = s.Ocsigen_extensions.default_hostname;
  Eliom_client_types.default_httpport = s.Ocsigen_extensions.default_httpport;
  Eliom_client_types.default_httpsport = s.Ocsigen_extensions.default_httpsport;
}

let client_request_info s =
    {Eliom_client_types.ri_url_string = s.Ocsigen_extensions.ri_url_string;
(*     ri_url: Neturl.url; *)
     Eliom_client_types.ri_method = s.Ocsigen_extensions.ri_method;
     Eliom_client_types.ri_protocol = s.Ocsigen_extensions.ri_protocol;
     Eliom_client_types.ri_ssl = s.Ocsigen_extensions.ri_ssl;
     Eliom_client_types.ri_full_path_string = s.Ocsigen_extensions.ri_full_path_string;
     Eliom_client_types.ri_full_path = s.Ocsigen_extensions.ri_full_path;
     Eliom_client_types.ri_original_full_path_string = s.Ocsigen_extensions.ri_original_full_path_string;
     Eliom_client_types.ri_original_full_path = s.Ocsigen_extensions.ri_original_full_path;
     Eliom_client_types.ri_sub_path = s.Ocsigen_extensions.ri_sub_path;
     Eliom_client_types.ri_sub_path_string = s.Ocsigen_extensions.ri_sub_path_string;
     Eliom_client_types.ri_get_params_string = s.Ocsigen_extensions.ri_get_params_string;
     Eliom_client_types.ri_host = s.Ocsigen_extensions.ri_host;
     Eliom_client_types.ri_port_from_host_field = s.Ocsigen_extensions.ri_port_from_host_field;
(*
     Eliom_client_types.ri_get_params = s.Ocsigen_extensions.ri_get_params;
     Eliom_client_types.ri_initial_get_params = s.Ocsigen_extensions.ri_initial_get_params;
     Eliom_client_types.ri_post_params = s.Ocsigen_extensions.ri_post_params;
     Eliom_client_types.ri_files = s.Ocsigen_extensions.ri_files;
(*     Eliom_client_types.ri_remote_inet_addr = s.Ocsigen_extensions.ri_remote_inet_addr; *)
     Eliom_client_types.ri_remote_ip = s.Ocsigen_extensions.ri_remote_ip;
     Eliom_client_types.ri_remote_ip_parsed = s.Ocsigen_extensions.ri_remote_ip_parsed;
     Eliom_client_types.ri_remote_port = s.Ocsigen_extensions.ri_remote_port;
     Eliom_client_types.ri_server_port = s.Ocsigen_extensions.ri_server_port;
     Eliom_client_types.ri_user_agent = s.Ocsigen_extensions.ri_user_agent;
     Eliom_client_types.ri_cookies_string = s.Ocsigen_extensions.ri_cookies_string;
     Eliom_client_types.ri_cookies = s.Ocsigen_extensions.ri_cookies;
     Eliom_client_types.ri_ifmodifiedsince = s.Ocsigen_extensions.ri_ifmodifiedsince;
     Eliom_client_types.ri_ifunmodifiedsince = s.Ocsigen_extensions.ri_ifunmodifiedsince;
     Eliom_client_types.ri_ifnonematch = s.Ocsigen_extensions.ri_ifnonematch;
     Eliom_client_types.ri_ifmatch = s.Ocsigen_extensions.ri_ifmatch;
     Eliom_client_types.ri_content_type = s.Ocsigen_extensions.ri_content_type;
     Eliom_client_types.ri_content_type_string = s.Ocsigen_extensions.ri_content_type_string;
     Eliom_client_types.ri_content_length = s.Ocsigen_extensions.ri_content_length;
     Eliom_client_types.ri_referer = s.Ocsigen_extensions.ri_referer;

     Eliom_client_types.ri_accept = s.Ocsigen_extensions.ri_accept;
     Eliom_client_types.ri_accept_charset = s.Ocsigen_extensions.ri_accept_charset;
     Eliom_client_types.ri_accept_encoding = s.Ocsigen_extensions.ri_accept_encoding;
     Eliom_client_types.ri_accept_language = s.Ocsigen_extensions.ri_accept_language;
*)

(*     Eliom_client_types.ri_http_frame = s.Ocsigen_extensions.ri_http_frame; *)
(*     Eliom_client_types.ri_request_cache = s.Ocsigen_extensions.ri_request_cache; *)
   }

let client_request s = {
  Eliom_client_types.request_info = 
    client_request_info s.Ocsigen_extensions.request_info;
  Eliom_client_types.request_config = 
    client_config_info s.Ocsigen_extensions.request_config;
}

*)

let client_si s = 
  (* we force all lazys before serialization *)
  {s with
     Eliom_common.si_na_get_params = 
      (let r = Lazy.force s.Eliom_common.si_na_get_params in lazy r);
     Eliom_common.si_persistent_nl_get_params =
      (let r = Lazy.force s.Eliom_common.si_persistent_nl_get_params in lazy r);
     Eliom_common.si_all_get_but_na_nl = 
      (let r = Lazy.force s.Eliom_common.si_all_get_but_na_nl in lazy r);
   }


let client_sp s =
  let s = Eliom_request_info.esp_of_sp s in
  {
(* Eliom_client_types.sp_request = client_request s.Eliom_common.sp_request; *)
   Eliom_client_types.sp_si = client_si s.Eliom_common.sp_si;
   Eliom_client_types.sp_sitedata = client_sitedata s.Eliom_common.sp_sitedata;
   (*     Eliom_client_types.sp_cookie_info = s.sp_cookie_info; *)
   Eliom_client_types.sp_suffix = s.Eliom_common.sp_suffix;
   Eliom_client_types.sp_fullsessname = s.Eliom_common.sp_fullsessname;}


let eliom_appl_page_data_key : ((int64 * int) * unit list) Polytables.key = 
  Polytables.make_key ()

let get_eliom_appl_page_data_ ~sp = 
  let rc = Eliom_request_info.get_request_cache ~sp in
  try 
    Polytables.get ~table:rc ~key:eliom_appl_page_data_key
  with Not_found -> 
    let d = ((Eliom_request_info.get_request_id ~sp, 0), []) in
    Polytables.set ~table:rc ~key:eliom_appl_page_data_key ~value:d;
    d

let wrap ~sp (v : 'a) : 'a Eliom_client_types.data_key =
  let rc = Eliom_request_info.get_request_cache ~sp in
  let ((reqnum, num) as n, data) =
    try Polytables.get ~table:rc ~key:eliom_appl_page_data_key
    with Not_found -> ((Eliom_request_info.get_request_id ~sp, 0), [])
  in
  Polytables.set ~table:rc ~key:eliom_appl_page_data_key
    ~value:((reqnum, num+1), Obj.magic v::data);
  Eliom_client_types.to_data_key_ n


let wrap_sp ~sp = wrap ~sp (client_sp sp)

let wrap_node ~sp n = 
  let reqnum = Eliom_request_info.get_request_id ~sp in
  Eliom_client_types.to_data_key_ (reqnum, XML.ref_node (XHTML5.M.toelt n))


