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
let set_session_gc_frequency t =
  let t = Option.map float_of_int t in
  Mod_gc.set_servicesessiongcfrequency t;
  Mod_gc.set_datasessiongcfrequency t

let set_service_session_gc_frequency t =
  let t = Option.map float_of_int t in
  Mod_gc.set_servicesessiongcfrequency t

let set_data_session_gc_frequency t =
  let t = Option.map float_of_int t in
  Mod_gc.set_datasessiongcfrequency t

let set_persistent_session_gc_frequency t =
  let t = Option.map float_of_int t in
  Mod_gc.set_persistentsessiongcfrequency t

let set_volatile_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Mod_timeouts.set_default ?scope_hierarchy `Data cookie_level v;
  Mod_timeouts.set_default ?scope_hierarchy `Service cookie_level v

let set_data_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Mod_timeouts.set_default ?scope_hierarchy `Data cookie_level v

let set_service_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Mod_timeouts.set_default ?scope_hierarchy `Service cookie_level v

let set_persistent_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Mod_timeouts.set_default ?scope_hierarchy `Persistent cookie_level v

let set_max_service_sessions_per_group v =
  Mod_main.default_max_service_sessions_per_group := v

let set_max_volatile_data_sessions_per_group v =
  Mod_main.default_max_volatile_data_sessions_per_group := v

let set_max_persistent_data_sessions_per_group v =
  Mod_main.default_max_persistent_data_sessions_per_group := v

let set_max_service_tab_sessions_per_group v =
  Mod_main.default_max_service_tab_sessions_per_group := v

let set_max_volatile_data_tab_sessions_per_group v =
  Mod_main.default_max_volatile_data_tab_sessions_per_group := v

let set_max_persistent_data_tab_sessions_per_group v =
  Mod_main.default_max_persistent_data_tab_sessions_per_group := v

let set_max_anonymous_services_per_session v =
  Mod_main.default_max_anonymous_services_per_session := v

let set_max_volatile_groups_per_site v =
  Mod_main.default_max_volatile_groups_per_site := v

let set_secure_cookies v = Mod_main.default_secure_cookies := v
let set_application_script v = Mod_main.default_application_script := v
let set_enable_wasm v = Mod_main.default_enable_wasm := v
let get_enable_wasm () = !Mod_main.default_enable_wasm
let set_cache_global_data v = Mod_main.default_cache_global_data := v
let set_html_content_type v = Mod_main.default_html_content_type := Some v

let add_ignored_get_params regexp =
  Mod_main.default_ignored_get_params :=
    regexp :: !Mod_main.default_ignored_get_params

let add_ignored_post_params regexp =
  Mod_main.default_ignored_post_params :=
    regexp :: !Mod_main.default_ignored_post_params

let set_omitpersistentstorage v = Mod_main.default_omitpersistentstorage := v

let get_default_hostname () =
  let sitedata = Request_info.find_sitedata "get_default_hostname" in
  (Common.get_config_info sitedata).Ocsigen_extensions.default_hostname

let get_default_port () =
  let sitedata = Request_info.find_sitedata "get_default_port" in
  (Common.get_config_info sitedata).Ocsigen_extensions.default_httpport

let get_default_sslport () =
  let sitedata = Request_info.find_sitedata "get_default_sslport" in
  (Common.get_config_info sitedata).Ocsigen_extensions.default_httpsport

let default_protocol_is_https () =
  let sitedata = Request_info.find_sitedata "default_protocol_is_https" in
  (Common.get_config_info sitedata).Ocsigen_extensions.default_protocol_is_https

let get_default_links_xhr () =
  let sitedata = Request_info.find_sitedata "get_default_links_xhr" in
  sitedata.Common.default_links_xhr#get

let set_default_links_xhr ?override_configfile:_ v =
  let sitedata = Request_info.find_sitedata "set_default_links_xhr" in
  sitedata.Common.default_links_xhr#set v

let get_config_default_charset_sp sp =
  Ocsigen_charset_mime.default_charset
    sp.Common.sp_request.Ocsigen_extensions.request_config
      .Ocsigen_extensions.charset_assoc

let get_config_default_charset () =
  let sp = Common.get_sp () in
  get_config_default_charset_sp sp

let get_config_info_sp sp =
  sp.Common.sp_request.Ocsigen_extensions.request_config

let get_config_info () =
  let sp = Common.get_sp () in
  get_config_info_sp sp

let get_config () =
  match Common.global_register_allowed () with
  | Some _ -> (
    match !Mod_main.config with
    | Some c -> c
    | None -> failwith "No config file. Is it a statically linked executable?")
  | None ->
      raise (Common.Eliom_site_information_not_available "Config.get_config")

let parse_config ?pcdata ?other_elements elements =
  Ocsigen_extensions.Configuration.process_elements
    ~in_tag:!Mod_main.config_in_tag ?pcdata ?other_elements ~elements
    (get_config ())

let get_debugmode = Ocsigen_config.get_debugmode
