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


let get_default_hostname () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_hostname" in
  sitedata.Eliom_common.config_info.Ocsigen_extensions.default_hostname

let get_default_port () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_port" in
  sitedata.Eliom_common.config_info.Ocsigen_extensions.default_httpport

let get_default_sslport () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_sslport" in
  sitedata.Eliom_common.config_info.Ocsigen_extensions.default_httpsport

let default_protocol_is_https () =
  let sitedata = Eliom_request_info.find_sitedata "default_protocol_is_https" in
  sitedata.Eliom_common.config_info.Ocsigen_extensions.default_protocol_is_https

let get_default_links_xhr () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_links_xhr" in
  sitedata.Eliom_common.default_links_xhr#get

let set_default_links_xhr ?override_configfile:_ v =
  let sitedata = Eliom_request_info.find_sitedata "set_default_links_xhr" in
  sitedata.Eliom_common.default_links_xhr#set v

let get_config_default_charset_sp sp =
  Ocsigen_charset_mime.default_charset
    sp.Eliom_common.sp_request.Ocsigen_extensions.request_config.Ocsigen_extensions.charset_assoc

let get_config_default_charset () =
  let sp = Eliom_common.get_sp () in
  get_config_default_charset_sp sp

let get_config_info_sp sp =
  sp.Eliom_common.sp_request.Ocsigen_extensions.request_config
let get_config_info () =
  let sp = Eliom_common.get_sp () in
  get_config_info_sp sp

let get_config () =
  match Eliom_common.global_register_allowed () with
  | Some _ -> !Eliommod.config
  | None ->
    raise (Eliom_common.Eliom_site_information_not_available
             "Eliom_config.get_config")

let parse_config ?pcdata ?other_elements elements =
  Ocsigen_extensions.Configuration.process_elements
    ~in_tag:!Eliommod.config_in_tag ?pcdata
    ?other_elements ~elements (get_config ())

let get_debugmode = Ocsigen_config.get_debugmode
