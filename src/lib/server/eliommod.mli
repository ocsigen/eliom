val default_max_persistent_data_sessions_per_group : int ref
val default_max_service_sessions_per_group : int ref
val default_max_service_sessions_per_subnet : int ref
val default_max_volatile_data_sessions_per_group : int ref
val default_max_volatile_data_sessions_per_subnet : int ref
val default_max_persistent_data_tab_sessions_per_group : int ref
val default_max_service_tab_sessions_per_group : int ref
val default_max_volatile_data_tab_sessions_per_group : int ref
val default_secure_cookies : bool ref
val default_application_script : (bool * bool) ref
val default_cache_global_data : (Eliom_lib.Url.path * int) option ref
val default_html_content_type : string option ref
val default_ignored_get_params : (string * Re.re) list ref
val default_ignored_post_params : (string * Re.re) list ref

val default_omitpersistentstorage :
  Eliom_common.omitpersistentstorage_rule list option ref

val default_max_anonymous_services_per_subnet : int ref
val default_max_anonymous_services_per_session : int ref
val default_max_volatile_groups_per_site : int ref

module S : sig
  type key = Ocsigen_extensions.virtual_hosts * Eliom_lib.Url.path
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

val create_sitedata :
   Ocsigen_extensions.virtual_hosts
  -> Eliom_lib.Url.path
  -> Ocsigen_extensions.config_info
  -> Eliom_common.sitedata

val parse_eliom_option :
   ([> `Client_process | `Session]
    -> Eliom_common_base.scope_hierarchy option
    -> float option
    -> unit)
   * ([> `Client_process | `Session]
      -> Eliom_common_base.scope_hierarchy option
      -> float option
      -> unit)
   * ([> `Client_process | `Session]
      -> Eliom_common_base.scope_hierarchy option
      -> float option
      -> unit)
   * ([> `Client_process | `Session]
      -> Eliom_common_base.scope_hierarchy option
      -> float option
      -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (bool -> unit)
   * (int -> unit)
   * (int -> unit)
   * (bool * bool -> unit)
   * ((Eliom_lib.Url.path * int) option -> unit)
   * (string -> unit)
   * (string * Re.re -> unit)
   * (string * Re.re -> unit)
   * (Eliom_common.omitpersistentstorage_rule list option -> unit)
  -> Xml_light_types.xml
  -> unit

val parse_eliom_options :
   ([> `Client_process | `Session]
    -> Eliom_common_base.scope_hierarchy option
    -> float option
    -> unit)
   * ([> `Client_process | `Session]
      -> Eliom_common_base.scope_hierarchy option
      -> float option
      -> unit)
   * ([> `Client_process | `Session]
      -> Eliom_common_base.scope_hierarchy option
      -> float option
      -> unit)
   * ([> `Client_process | `Session]
      -> Eliom_common_base.scope_hierarchy option
      -> float option
      -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (int -> unit)
   * (bool -> unit)
   * (int -> unit)
   * (int -> unit)
   * (bool * bool -> unit)
   * ((Eliom_lib.Url.path * int) option -> unit)
   * (string -> unit)
   * (string * Re.re -> unit)
   * (string * Re.re -> unit)
   * (Eliom_common.omitpersistentstorage_rule list option -> unit)
  -> Xml_light_types.xml list
  -> Xml_light_types.xml list

val parse_global_config : Xml_light_types.xml list -> unit
val exception_during_eliommodule_loading : bool ref
val end_init : unit -> unit
val handle_init_exn : exn -> string
val site_init_ref : (unit -> unit) list ref
val register_site_init : (unit -> unit) -> unit
val config : Xml_light_types.xml list ref
val config_in_tag : string ref
val site_init : bool ref -> unit

val load_eliom_module :
   'a
  -> [< `Default_site | `Name of string | `Site of Ocsigen_loader.site]
  -> string
  -> Xml_light_types.xml list
  -> unit

val gen_nothing : unit -> 'a -> Ocsigen_extensions.answer Lwt.t
val default_module_action : 'a -> 'b

val set_timeout :
   (?full_st_name:Eliom_common.full_state_name
    -> ?cookie_level:([< Eliom_common.cookie_level] as 'a)
    -> recompute_expdates:bool
    -> bool
    -> bool
    -> Eliom_common.sitedata
    -> float option
    -> unit)
  -> Eliom_common.sitedata
  -> 'a
  -> Eliom_common_base.scope_hierarchy option
  -> float option
  -> unit

val parse_config :
   'a
  -> Ocsigen_extensions.virtual_hosts
  -> Ocsigen_extensions.config_info
  -> Eliom_lib.Url.path
  -> 'b
  -> 'c
  -> Xml_light_types.xml
  -> Ocsigen_extensions.request_state
  -> Ocsigen_extensions.answer Lwt.t
