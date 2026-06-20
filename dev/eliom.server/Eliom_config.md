
# Module `Eliom_config`

This module makes also possible get information from the configuration file.

It make also possible to add configuration options in configuration file for your Eliom modules. Use [`Eliom_config.parse_config`](./#val-parse_config) for that.

Example:

```
     <eliommodule ...>
        <myoption myattr="...">myvalue</myoption>
     </eliommodule>
```
```ocaml
val set_session_gc_frequency : int option -> unit
```
Set frequency for garnage collection of both in memory data and service sessions. `None` means never.

```ocaml
val set_service_session_gc_frequency : int option -> unit
```
Set frequency for garbage collection of service sessions. `None` means never.

```ocaml
val set_data_session_gc_frequency : int option -> unit
```
Set frequency for garbage collection of in memory data sessions. `None` means never.

```ocaml
val set_persistent_session_gc_frequency : int option -> unit
```
Set frequency for garbage collection of persistent data sessions. `None` means never.

```ocaml
val set_volatile_timeout : 
  ?scope_hierarchy:Eliom_common.scope_hierarchy ->
  cookie_level:[< `Session | `Client_process ] ->
  int option ->
  unit
```
```ocaml
val set_data_timeout : 
  ?scope_hierarchy:Eliom_common.scope_hierarchy ->
  cookie_level:[< `Session | `Client_process ] ->
  int option ->
  unit
```
```ocaml
val set_service_timeout : 
  ?scope_hierarchy:Eliom_common.scope_hierarchy ->
  cookie_level:[< `Session | `Client_process ] ->
  int option ->
  unit
```
```ocaml
val set_persistent_timeout : 
  ?scope_hierarchy:Eliom_common.scope_hierarchy ->
  cookie_level:[< `Session | `Client_process ] ->
  int option ->
  unit
```
```ocaml
val set_max_service_sessions_per_group : int -> unit
```
```ocaml
val set_max_volatile_data_sessions_per_group : int -> unit
```
```ocaml
val set_max_persistent_data_sessions_per_group : int -> unit
```
```ocaml
val set_max_service_tab_sessions_per_group : int -> unit
```
```ocaml
val set_max_volatile_data_tab_sessions_per_group : int -> unit
```
```ocaml
val set_max_persistent_data_tab_sessions_per_group : int -> unit
```
```ocaml
val set_max_anonymous_services_per_session : int -> unit
```
```ocaml
val set_max_volatile_groups_per_site : int -> unit
```
```ocaml
val set_secure_cookies : bool -> unit
```
```ocaml
val set_application_script : (bool * bool) -> unit
```
```ocaml
val set_enable_wasm : bool -> unit
```
Enable or disable WebAssembly support with automatic browser detection. When enabled, Eliom will generate a script that detects WebAssembly support in the browser and loads either the WASM version (.wasm.js) or the JavaScript fallback (.js) accordingly.

Disabled by default for backward compatibility. The Eliom templates enable it explicitly.

This setting can be overridden per-site using the `~enable_wasm` parameter of [`Eliom.run`](./Eliom.md#val-run) or the `<wasm enabled="true|false"/>` tag in the configuration file.

```ocaml
val get_enable_wasm : unit -> bool
```
Get the current global setting for WebAssembly support.

```ocaml
val set_cache_global_data : (Eliom_lib.Url.path * int) option -> unit
```
```ocaml
val set_html_content_type : string -> unit
```
```ocaml
val add_ignored_get_params : (string * Re.re) -> unit
```
```ocaml
val add_ignored_post_params : (string * Re.re) -> unit
```
```ocaml
val set_omitpersistentstorage : 
  Eliom_common.omitpersistentstorage_rule list option ->
  unit
```
```ocaml
val get_default_hostname : unit -> string
```
The function `get_default_hostname ()`returns the hostname declared in the config file (`<host defaulthostname="...">`) or the default machine hostname. In that case, absolute URL will use that hostname.

```ocaml
val get_default_port : unit -> int
```
The function `get_default_port ()` returns the port number declared in the config file (`<host defaulthttpport="...">`) or \-\> unit 80 if undeclared.

```ocaml
val get_default_sslport : unit -> int
```
The function `get_default_sslport ()` returns the https port number declared in the config file (`<host defaulthttpsport="...">`) or 443 if undeclared.

```ocaml
val default_protocol_is_https : unit -> bool
```
The function `default_protocol_is_https ()` returns `true` if there is option (`<host defaultprotocol="https">`)or false otherwise. In that case, absolute links will use https protocol even if the current request is http.

```ocaml
val get_config_default_charset : unit -> string
```
The function `get_config_default_charset ()` returns the default charset for this site.

```ocaml
val set_default_links_xhr : ?override_configfile:bool -> bool -> unit
```
The provided value serves as a default value for the optional parameter `~xhr` in the functions `Eliom_registration.*.{a, get_form, post_form, lwt_get_form, lwt_post_form}` (cf. `Eliom_registration.Html.a` et al.). This value can also be set in the [config file](./../config.md).

```ocaml
val get_config : unit -> Xml.xml list
```
The function `get_config ()` returns the information of the configuration file concerning that site (between `<eliommodule>` and `</eliommodule>` or `<eliom>` and `</eliom>`).

*Warning: You must call that function during the initialisation of your module (not during a Lwt thread or a service) otherwise it will raise the exception [`Eliom_common.Eliom_site_information_not_available`](./Eliom_common.md#exception-Eliom_site_information_not_available). If you want to build a statically linkable module, you must call this function inside the initialisation function given to [`Eliom_service.register_eliom_module`](./Eliom_service.md#val-register_eliom_module).*

```ocaml
val parse_config : 
  ?pcdata:(string -> unit) ->
  ?other_elements:(string -> (string * string) list -> Xml.xml list -> unit) ->
  Ocsigen_extensions.Configuration.element list ->
  unit
```
Process the configuration (same as the one returned by [`Eliom_config.get_config`](./#val-get_config)) by a given specification (cf. [`Ocsigen_extensions.Configuration.element`](./../../ocsigenserver/ocsigenserver/Ocsigen_extensions-Configuration.md#type-element))

```ocaml
val get_config_info : unit -> Ocsigen_extensions.config_info
```
The function `get_config_info ()` returns the information concerning the current request from the configuration files (must be called during a request). The configuration may have been modified by previous Ocsigen server extensions.

```ocaml
val get_debugmode : unit -> bool
```
Same as `Ocsigen_config.get_debugmode`. On client side, returns `false` for now.
