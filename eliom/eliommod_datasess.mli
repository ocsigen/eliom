val close_data_group :
  Eliom_common.sessgrp -> unit
val close_data_session :
  ?close_group:bool ->
  ?session_name:string -> 
  ?cookie_type:Eliom_common.cookie_type ->
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> unit
val new_data_cookie :
  Eliom_common.sitedata ->
  Eliom_common.sessgrp ->
  'a ->
  ('a * float option ref * Eliom_common.timeout ref *
   Eliom_common.sessgrp ref * string Ocsigen_cache.Dlist.node)
  Eliom_common.SessionCookies.t -> Eliom_common.one_data_cookie_info
val find_or_create_data_cookie :
  ?set_session_group:string ->
  ?session_name:string ->
  ?cookie_type:Eliom_common.cookie_type ->
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> Eliom_common.one_data_cookie_info
val find_data_cookie_only :
  ?session_name:string ->
  ?cookie_type:Eliom_common.cookie_type ->
  secure:bool option ->
  sp:Eliom_common.server_params -> unit -> Eliom_common.one_data_cookie_info
val counttableelements : (unit -> int) list ref
val create_volatile_table : unit -> 'a Eliom_common.SessionCookies.t
val create_volatile_table_during_session :
  Eliom_common.server_params -> 'a Eliom_common.SessionCookies.t
