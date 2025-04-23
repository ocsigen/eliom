val set_app_name : string -> unit
(** Set your application name. Use this if you want to make it possible
    to link your Eliom application statically and to have several Eliom
    applications on the same Web server.
    See also parameter [?app_names] of [instruction] or [Eliom.run].
 *)

val default_app_name : string
(** The default application name, if you don't specify any *)

val run :
  ?app:string ->
  ?xhr_links:bool ->
  ?data_timeout:
    [< Eliom_common.cookie_level ]
    * Eliom_common_base.scope_hierarchy option
    * float option ->
  ?service_timeout:
    [< Eliom_common.cookie_level ]
    * Eliom_common_base.scope_hierarchy option
    * float option ->
  ?persistent_timeout:
    [< Eliom_common.cookie_level ]
    * Eliom_common_base.scope_hierarchy option
    * float option ->
  ?max_service_sessions_per_group:int * bool ->
  ?max_volatile_data_sessions_per_group:int * bool ->
  ?max_persistent_data_sessions_per_group:int ->
  ?max_service_tab_sessions_per_group:int * bool ->
  ?max_volatile_data_tab_sessions_per_group:int * bool ->
  ?max_persistent_data_tab_sessions_per_group:int ->
  ?max_anonymous_services_per_session:int * bool ->
  ?secure_cookies:bool ->
  ?application_script:bool * bool ->
  ?global_data_caching:(string list * int) option ->
  ?html_content_type:string ->
  ?ignored_get_params:string * Re.re ->
  ?ignored_post_params:string * Re.re ->
  ?omitpersistentstorage:Eliom_common.omitpersistentstorage_rule list option ->
  unit ->
  Ocsigen_server.instruction
(** [run ?app ()] run Eliom application [app] under current site.
    Use this to build a static executable without configuration file.
    Default value of [?app] is [default_app_name].
    Other optional values correspond to Eliom configuration for this site. *)
