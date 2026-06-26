
# Module `Eliom.Mod_main`

```ocaml
val default_max_persistent_data_sessions_per_group : int ref
```
```ocaml
val default_max_service_sessions_per_group : int ref
```
```ocaml
val default_max_service_sessions_per_subnet : int ref
```
```ocaml
val default_max_volatile_data_sessions_per_group : int ref
```
```ocaml
val default_max_volatile_data_sessions_per_subnet : int ref
```
```ocaml
val default_max_persistent_data_tab_sessions_per_group : int ref
```
```ocaml
val default_max_service_tab_sessions_per_group : int ref
```
```ocaml
val default_max_volatile_data_tab_sessions_per_group : int ref
```
```ocaml
val default_secure_cookies : bool ref
```
```ocaml
val default_application_script : (bool * bool) ref
```
```ocaml
val default_enable_wasm : bool ref
```
```ocaml
val default_cache_global_data : (Lib.Url.path * int) option ref
```
```ocaml
val default_html_content_type : string option ref
```
```ocaml
val default_ignored_get_params : (string * Re.re) list ref
```
```ocaml
val default_ignored_post_params : (string * Re.re) list ref
```
```ocaml
val default_omitpersistentstorage : 
  Common.omitpersistentstorage_rule list option ref
```
```ocaml
val default_max_anonymous_services_per_subnet : int ref
```
```ocaml
val default_max_anonymous_services_per_session : int ref
```
```ocaml
val default_max_volatile_groups_per_site : int ref
```
```ocaml
module S : sig ... end
```
```ocaml
val create_sitedata : 
  Ocsigen.Extensions.virtual_hosts ->
  Lib.Url.path ->
  Ocsigen.Extensions.config_info ->
  Common.sitedata
```
```ocaml
val parse_eliom_option : 
  (([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * ([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * ([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * ([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (bool ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * ((bool * bool) ->
   unit)
   * (bool ->
   unit)
   * ((Lib.Url.path * int) option ->
   unit)
   * (string ->
   unit)
   * ((string * Re.re) ->
   unit)
   * ((string * Re.re) ->
   unit)
   * (Common.omitpersistentstorage_rule list option ->
   unit)) ->
  Xml_light_types.xml ->
  unit
```
```ocaml
val parse_eliom_options : 
  (([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * ([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * ([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * ([> `Client_process | `Session ] ->
   Common_base.scope_hierarchy option ->
   float option ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * (bool ->
   unit)
   * (int ->
   unit)
   * (int ->
   unit)
   * ((bool * bool) ->
   unit)
   * (bool ->
   unit)
   * ((Lib.Url.path * int) option ->
   unit)
   * (string ->
   unit)
   * ((string * Re.re) ->
   unit)
   * ((string * Re.re) ->
   unit)
   * (Common.omitpersistentstorage_rule list option ->
   unit)) ->
  Xml_light_types.xml list ->
  Xml_light_types.xml list
```
```ocaml
val parse_global_config : Xml_light_types.xml list -> unit
```
```ocaml
val exception_during_eliommodule_loading : bool ref
```
```ocaml
val end_init : unit -> unit
```
```ocaml
val handle_init_exn : exn -> string
```
```ocaml
val site_init_ref : (unit -> unit) list ref
```
```ocaml
val register_site_init : (unit -> unit) -> unit
```
```ocaml
val config : Xml_light_types.xml list option ref
```
```ocaml
val config_in_tag : string ref
```
```ocaml
type module_to_load = 
  | Files of string list
  | Name of string
```
```ocaml
val set_app_name : string -> unit
```
```ocaml
val site_init : bool ref -> unit
```
```ocaml
val update_sitedata : 
  string ->
  Ocsigen.Extensions.virtual_hosts ->
  Lib.Url.path ->
  Ocsigen.Extensions.config_info ->
  Common.sitedata
```
```ocaml
val load_eliom_module : 
  'a ->
  module_to_load ->
  string ->
  Xml_light_types.xml list ->
  unit
```
```ocaml
val gen_nothing : unit -> 'a -> Ocsigen.Extensions.answer Lwt.t
```
```ocaml
val default_module_action : 'a -> 'b
```
```ocaml
val set_timeout : 
  (?full_st_name:Common.full_state_name ->
    ?cookie_level:([< Common.cookie_level ] as 'a) ->
    recompute_expdates:bool ->
    bool ->
    bool ->
    Common.sitedata ->
    float option ->
    unit) ->
  Common.sitedata ->
  'a ->
  Common_base.scope_hierarchy option ->
  float option ->
  unit
```
```ocaml
val parse_config : 
  'a ->
  Ocsigen.Extensions.virtual_hosts ->
  Ocsigen.Extensions.config_info ->
  Lib.Url.path ->
  'b ->
  'c ->
  Xml_light_types.xml ->
  Ocsigen.Extensions.request_state ->
  Ocsigen.Extensions.answer Lwt.t
```