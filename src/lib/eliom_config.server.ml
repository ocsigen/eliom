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
  Eliommod_gc.set_servicesessiongcfrequency t;
  Eliommod_gc.set_datasessiongcfrequency t

let set_service_session_gc_frequency t =
  let t = Option.map float_of_int t in
  Eliommod_gc.set_servicesessiongcfrequency t

let set_data_session_gc_frequency t =
  let t = Option.map float_of_int t in
  Eliommod_gc.set_datasessiongcfrequency t

let set_persistent_session_gc_frequency t =
  let t = Option.map float_of_int t in
  Eliommod_gc.set_persistentsessiongcfrequency t

let set_volatile_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Eliommod_timeouts.set_default ?scope_hierarchy `Data cookie_level v;
  Eliommod_timeouts.set_default ?scope_hierarchy `Service cookie_level v

let set_data_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Eliommod_timeouts.set_default ?scope_hierarchy `Data cookie_level v

let set_service_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Eliommod_timeouts.set_default ?scope_hierarchy `Service cookie_level v

let set_persistent_timeout ?scope_hierarchy ~cookie_level v =
  let v = Option.map float_of_int v in
  Eliommod_timeouts.set_default ?scope_hierarchy `Persistent cookie_level v

let set_max_service_sessions_per_group v =
  Eliommod.default_max_service_sessions_per_group := v

let set_max_volatile_data_sessions_per_group v =
  Eliommod.default_max_volatile_data_sessions_per_group := v

let set_max_persistent_data_sessions_per_group v =
  Eliommod.default_max_persistent_data_sessions_per_group := v

let set_max_service_tab_sessions_per_group v =
  Eliommod.default_max_service_tab_sessions_per_group := v

let set_max_volatile_data_tab_sessions_per_group v =
  Eliommod.default_max_volatile_data_tab_sessions_per_group := v

let set_max_persistent_data_tab_sessions_per_group v =
  Eliommod.default_max_persistent_data_tab_sessions_per_group := v

let set_max_anonymous_services_per_session v =
  Eliommod.default_max_anonymous_services_per_session := v

let set_max_volatile_groups_per_site v =
  Eliommod.default_max_volatile_groups_per_site := v

let set_secure_cookies v = Eliommod.default_secure_cookies := v
let set_application_script v = Eliommod.default_application_script := v
let set_cache_global_data v = Eliommod.default_cache_global_data := v
let set_html_content_type v = Eliommod.default_html_content_type := Some v

let add_ignored_get_params regexp =
  Eliommod.default_ignored_get_params :=
    regexp :: !Eliommod.default_ignored_get_params

let add_ignored_post_params regexp =
  Eliommod.default_ignored_post_params :=
    regexp :: !Eliommod.default_ignored_post_params

let set_omitpersistentstorage v = Eliommod.default_omitpersistentstorage := v

let get_default_hostname () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_hostname" in
  (Eliom_common.get_config_info sitedata).Ocsigen_extensions.default_hostname

let get_default_port () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_port" in
  (Eliom_common.get_config_info sitedata).Ocsigen_extensions.default_httpport

let get_default_sslport () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_sslport" in
  (Eliom_common.get_config_info sitedata).Ocsigen_extensions.default_httpsport

let default_protocol_is_https () =
  let sitedata = Eliom_request_info.find_sitedata "default_protocol_is_https" in
  (Eliom_common.get_config_info sitedata)
    .Ocsigen_extensions.default_protocol_is_https

let get_default_links_xhr () =
  let sitedata = Eliom_request_info.find_sitedata "get_default_links_xhr" in
  sitedata.Eliom_common.default_links_xhr#get

let set_default_links_xhr ?override_configfile:_ v =
  let sitedata = Eliom_request_info.find_sitedata "set_default_links_xhr" in
  sitedata.Eliom_common.default_links_xhr#set v

let get_config_default_charset_sp sp =
  Ocsigen_charset_mime.default_charset
    sp.Eliom_common.sp_request.Ocsigen_extensions.request_config
      .Ocsigen_extensions.charset_assoc

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
  | Some _ -> (
      match !Eliommod.config with
      | Some c -> c
      | None -> failwith "No config file. Is it a statically linked executable?"
      )
  | None ->
      raise
        (Eliom_common.Eliom_site_information_not_available
           "Eliom_config.get_config")

let parse_config ?pcdata ?other_elements elements =
  Ocsigen_extensions.Configuration.process_elements
    ~in_tag:!Eliommod.config_in_tag ?pcdata ?other_elements ~elements
    (get_config ())

let get_debugmode = Ocsigen_config.get_debugmode
