val run :
   ?site:Ocsigen_server.Site.t
  -> ?xhr_links:bool
  -> ?data_timeout:
       [< Eliom_common.cookie_level]
       * Eliom_common_base.scope_hierarchy option
       * float option
  -> ?service_timeout:
       [< Eliom_common.cookie_level]
       * Eliom_common_base.scope_hierarchy option
       * float option
  -> ?persistent_timeout:
       [< Eliom_common.cookie_level]
       * Eliom_common_base.scope_hierarchy option
       * float option
  -> ?max_service_sessions_per_group:int * bool
  -> ?max_volatile_data_sessions_per_group:int * bool
  -> ?max_persistent_data_sessions_per_group:int
  -> ?max_service_tab_sessions_per_group:int * bool
  -> ?max_volatile_data_tab_sessions_per_group:int * bool
  -> ?max_persistent_data_tab_sessions_per_group:int
  -> ?max_anonymous_services_per_session:int * bool
  -> ?secure_cookies:bool
  -> ?application_script:bool * bool
  -> ?global_data_caching:(string list * int) option
  -> ?html_content_type:string
  -> ?ignored_get_params:string * Re.re
  -> ?ignored_post_params:string * Re.re
  -> ?omitpersistentstorage:Eliom_common.omitpersistentstorage_rule list option
  -> ?app_names:string list
  -> unit
  -> unit
(** [run ?site ?app_names ()] run Eliom applications [app_names] under site [site].
    Use this to build a static executable without configuration file.
    Default values are default site (root of the Web site on all virtual hosts)
    and default app name.
    Other optional values correspond to Eliom configuration for this site. *)
