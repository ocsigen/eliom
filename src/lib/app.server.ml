let default_app_name = Common.default_app_name
let set_app_name = Mod_main.set_app_name

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
  let check name = Option.iter (Common.check_limit ("App.run ~" ^ name)) in
  check "max_service_sessions_per_group" max_service_sessions_per_group;
  check "max_volatile_data_sessions_per_group"
    max_volatile_data_sessions_per_group;
  check "max_service_tab_sessions_per_group" max_service_tab_sessions_per_group;
  check "max_volatile_data_tab_sessions_per_group"
    max_volatile_data_tab_sessions_per_group;
  check "max_anonymous_services_per_session" max_anonymous_services_per_session;
  check "max_persistent_data_sessions_per_group"
    max_persistent_data_sessions_per_group;
  check "max_persistent_data_tab_sessions_per_group"
    max_persistent_data_tab_sessions_per_group;
  let sitedata = Mod_main.update_sitedata app vh site_dir conf_info in
  (* The values of App.run are kept as those of a configuration file *)
  let configured v = {Common.cf_value = v; cf_from_config = true} in
  (* customize sitedata according to optional parameters: *)
  Option.iter
    (fun v -> sitedata.Common.default_links_xhr#set ~override_tenable:true v)
    xhr_links;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Mod_main.set_timeout
         (Mod_timeouts.set_global_ ~kind:`Data)
         sitedata level hierarchyname v)
    data_timeout;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Mod_main.set_timeout
         (Mod_timeouts.set_global_ ~kind:`Service)
         sitedata level hierarchyname v)
    service_timeout;
  Option.iter
    (fun (level, hierarchyname, v) ->
       Mod_main.set_timeout
         (Mod_timeouts.set_global_ ~kind:`Persistent)
         sitedata level hierarchyname v)
    persistent_timeout;
  Option.iter
    (fun v -> sitedata.max_service_sessions_per_group <- configured v)
    max_service_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_volatile_data_sessions_per_group <- configured v)
    max_volatile_data_sessions_per_group;
  Option.iter
    (fun v ->
       sitedata.max_persistent_data_sessions_per_group <- configured (Some v))
    max_persistent_data_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_service_tab_sessions_per_group <- configured v)
    max_service_tab_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_volatile_data_tab_sessions_per_group <- configured v)
    max_volatile_data_tab_sessions_per_group;
  Option.iter
    (fun v ->
       sitedata.max_persistent_data_tab_sessions_per_group <-
         configured (Some v))
    max_persistent_data_tab_sessions_per_group;
  Option.iter
    (fun v -> sitedata.max_anonymous_services_per_session <- configured v)
    max_anonymous_services_per_session;
  (* The site of the default application is created when Eliom is
     initialised (see [Mod_main.get_sitedata]), before the toplevel code of
     the modules sets the defaults with the functions of [Config]: these
     options are the values given here, or else the current defaults. Only
     [Config] and [App.run] set them. *)
  let current param default = Option.value param ~default:!default in
  sitedata.secure_cookies <-
    current secure_cookies Mod_main.default_secure_cookies;
  sitedata.application_script <-
    (match application_script with
    | Some (defer, async) -> {defer; async}
    | None -> !Mod_main.default_application_script);
  sitedata.enable_wasm <- current enable_wasm Mod_main.default_enable_wasm;
  sitedata.cache_global_data <-
    current global_data_caching Mod_main.default_cache_global_data;
  sitedata.html_content_type <-
    (match html_content_type with
    | Some _ -> html_content_type
    | None -> !Mod_main.default_html_content_type);
  sitedata.ignored_get_params <-
    Option.to_list ignored_get_params @ !Mod_main.default_ignored_get_params;
  sitedata.ignored_post_params <-
    Option.to_list ignored_post_params @ !Mod_main.default_ignored_post_params;
  sitedata.omitpersistentstorage <-
    current omitpersistentstorage Mod_main.default_omitpersistentstorage;
  (* end sitedata *)
  Common.absolute_change_sitedata sitedata;
  Fun.protect ~finally:Common.end_current_sitedata (fun () ->
    Mod_main.site_init (ref true);
    (* Load app: *)
    Mod_main.load_eliom_module sitedata (Mod_main.Name app) "" [];
    (* As at the end of a site of a configuration file (Mod_main.end_init),
       which Ocsigen Server does not call for sites built by instructions *)
    Common.verify_all_registered sitedata);
  Mod_pagegen.gen None sitedata
