val set_app_name : string -> unit
(** Set your application name. Use this if you want to make it possible
    to link your Eliom application statically and to have several Eliom
    applications on the same Web server.
    See also parameter [?app_names] of [instruction] or [Eliom.App.run].
 *)

val default_app_name : string
(** The default application name, if you don't specify any *)

val run :
   ?app:string
  -> ?xhr_links:bool
  -> ?data_timeout:
       [< Common.cookie_level]
       * Common_base.scope_hierarchy option
       * float option
  -> ?service_timeout:
       [< Common.cookie_level]
       * Common_base.scope_hierarchy option
       * float option
  -> ?persistent_timeout:
       [< Common.cookie_level]
       * Common_base.scope_hierarchy option
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
  -> ?enable_wasm:bool
  -> ?global_data_caching:(string list * int) option
  -> ?html_content_type:string
  -> ?ignored_get_params:string * Re.re
  -> ?ignored_post_params:string * Re.re
  -> ?omitpersistentstorage:Common.omitpersistentstorage_rule list option
  -> unit
  -> Ocsigen.Server.instruction
(** [run ?app ()] run Eliom application [app] under current site.
    Use this to build a static executable without configuration file.
    The program must then also configure Ocsipersist, for example with
    [Ocsipersist_settings.set_db_file] for [ocsipersist-sqlite] (see the
    {{!//workflow/page-configuration.no_config_file}manual}).
    Default value of [?app] is [default_app_name].
    Other optional values correspond to Eliom configuration for this site;
    the maximum numbers of sessions and services must be positive ([run]
    raises [Invalid_argument] otherwise).

    As for a site of a configuration file, the services with a path created
    during the initialisation of the application must be registered by then:
    otherwise [run] raises {!Common.Eliom_there_are_unregistered_services},
    and the server does not start. Unregistered non-attached coservices are
    reported by a warning.

    When [?enable_wasm] is set to [true], the server will generate a detection
    script that loads the WASM version of your client code if the browser
    supports WebAssembly, with automatic fallback to JavaScript otherwise.
    If not specified, the global default is used
    (see {!Config.set_enable_wasm}), which is [false]. *)
