let run ?site ?xhr_links ?data_timeout ?service_timeout ?persistent_timeout
    ?max_service_sessions_per_group ?max_volatile_data_sessions_per_group
    ?max_persistent_data_sessions_per_group ?max_service_tab_sessions_per_group
    ?max_volatile_data_tab_sessions_per_group
    ?max_persistent_data_tab_sessions_per_group
    ?max_anonymous_services_per_session ?secure_cookies ?application_script
    ?global_data_caching ?html_content_type ?ignored_get_params
    ?ignored_post_params ?omitpersistentstorage ?app_names ()
  =
  Ocsigen_server.Site.register ?site
    (Eliom_registration.instruction ?xhr_links ?data_timeout ?service_timeout
       ?persistent_timeout ?max_service_sessions_per_group
       ?max_volatile_data_sessions_per_group
       ?max_persistent_data_sessions_per_group
       ?max_service_tab_sessions_per_group
       ?max_volatile_data_tab_sessions_per_group
       ?max_persistent_data_tab_sessions_per_group
       ?max_anonymous_services_per_session ?secure_cookies ?application_script
       ?global_data_caching ?html_content_type ?ignored_get_params
       ?ignored_post_params ?omitpersistentstorage ?app_names ())
