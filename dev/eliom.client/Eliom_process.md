# Module `Eliom_process`

```ocaml
val section : Logs.src
```
```ocaml
val log_section : Logs.src
```
```ocaml
val history_api : bool
```
```ocaml
val get_set_js_serverside_value : 
  'a option ref ->
  string ->
  ('a -> unit) * (unit -> bool) * (unit -> 'a) * (unit -> unit)
```
```ocaml
val set_sitedata : Eliom_types.sitedata -> unit
```
```ocaml
val is_set_sitedata : unit -> bool
```
```ocaml
val get_sitedata : unit -> Eliom_types.sitedata
```
```ocaml
val reset_sitedata : unit -> unit
```
```ocaml
val ignored_get_params : Re.re list ref
```
```ocaml
val ignored_post_params : Re.re list ref
```
```ocaml
val set_ignored_params : string list -> string list -> unit
```
```ocaml
val set_info : Eliom_common.client_process_info -> unit
```
```ocaml
val is_set_info : unit -> bool
```
```ocaml
val get_info : unit -> Eliom_common.client_process_info
```
```ocaml
val reset_info : unit -> unit
```
```ocaml
val set_request_cookies : Ocsigen_cookie_map.t -> unit
```
```ocaml
val is_set_request_cookies : unit -> bool
```
```ocaml
val get_request_cookies : unit -> Ocsigen_cookie_map.t
```
```ocaml
val reset_request_cookies : unit -> unit
```
```ocaml
val set_request_template : string option -> unit
```
```ocaml
val is_set_request_template : unit -> bool
```
```ocaml
val get_request_template : unit -> string option
```
```ocaml
val reset_request_template : unit -> unit
```
```ocaml
val appl_name : Deriving_Json.Json_string.a lazy_t
```
```ocaml
val set_base_url : string -> unit
```
```ocaml
val get_base_url : unit -> string
```
```ocaml
val appl_name_r : Deriving_Json.Json_string.a option ref
```
None on server side

```ocaml
val get_application_name : unit -> Deriving_Json.Json_string.a
```
```ocaml
val client_side : bool
```
