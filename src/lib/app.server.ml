let default_app_name = Eliom_common.default_app_name
let set_app_name = Eliommod.set_app_name

let run
      ?(app = default_app_name)
      ?xhr_links
      ?data_timeout
      ?service_timeout
      ?persistent_timeout
      ?max_service_sessions_per_group
      ?max_volatile_data_sessions_per_group
      ?max_persistent_data_sessions_per_group
      ?max_service_tab_sessions_per_group
      ?max_volatile_data_tab_sessions_per_group
      ?max_persistent_data_tab_sessions_per_group
      ?max_anonymous_services_per_session
      ?secure_cookies
      ?application_script
      ?enable_wasm
      ?global_data_caching
      ?html_content_type
      ?ignored_get_params
      ?ignored_post_params
      ?omitpersistentstorage
      ()
      vh
      conf_info
      site_dir
  =
  let sitedata = Eliommod.update_sitedata app vh site_dir conf_info in
  (* customize sitedata according to optional parameters: *)
  Option.iter
    (fun v ->
       sitedata.Eliom_common.default_links_xhr#set ~override_tenable:true v)
    xhr_links;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Eliommod.set_timeout
         (Eliommod_timeouts.set_global_ ~kind:`Data)
         sitedata level hierarchyname v)
    data_timeout;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Eliommod.set_timeout
         (Eliommod_timeouts.set_global_ ~kind:`Service)
         sitedata level hierarchyname v)
    service_timeout;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Eliommod.set_timeout
         (Eliommod_timeouts.set_global_ ~kind:`Persistent)
         sitedata level hierarchyname v)
    persistent_timeout;
  Option.iter
    (fun v -> sitedata.max_service_sessions_per_group <- v)
    max_service_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_volatile_data_sessions_per_group <- v)
    max_volatile_data_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_persistent_data_sessions_per_group <- Some v, true)
    max_persistent_data_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_service_tab_sessions_per_group <- v)
    max_service_tab_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_volatile_data_tab_sessions_per_group <- v)
    max_volatile_data_tab_sessions_per_group;
  Option.iter
    (fun v ->
       sitedata.max_persistent_data_tab_sessions_per_group <- Some v, true)
    max_persistent_data_tab_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_anonymous_services_per_session <- v)
    max_anonymous_services_per_session;
  Option.iter (fun v -> sitedata.secure_cookies <- v) secure_cookies;
  Option.iter (fun v -> sitedata.application_script <- v) application_script;
  (* Always update enable_wasm: use provided value or current global default *)
  sitedata.enable_wasm <-
    Option.value enable_wasm ~default:!Eliommod.default_enable_wasm;
  Option.iter (fun v -> sitedata.cache_global_data <- v) global_data_caching;
  Option.iter (fun v -> sitedata.html_content_type <- Some v) html_content_type;
  Option.iter
    (fun v -> sitedata.ignored_get_params <- v :: sitedata.ignored_get_params)
    ignored_get_params;
  Option.iter
    (fun v -> sitedata.ignored_post_params <- v :: sitedata.ignored_post_params)
    ignored_post_params;
  Option.iter
    (fun v -> sitedata.omitpersistentstorage <- v)
    omitpersistentstorage;
  (* end sitedata *)
  Eliom_common.absolute_change_sitedata sitedata;
  Eliommod.site_init (ref true);
  (* Load app: *)
  Eliommod.load_eliom_module sitedata (Eliommod.Name app) "" [];
  Eliommod_pagegen.gen None sitedata
